#!/usr/bin/env python3
"""Compile descriptors into an elaboration plan and a verified manifest.

Two outputs, because they answer different questions and have different prerequisites.

  generated/elaboration_plan.json       What to elaborate and with what params (schema only)
  generated/descriptor_manifest.json    Checks the I/O port contract; needs lockfiles.

Checks:
  1. schema     every YAML matches descriptors/schema.json
  2. binding    every declared port exists in the lock with matching direction and width
  3. coverage   every lock port is mapped, ignored, or is clock/reset
  4. profile    flags and rounding claims agree with the ports that actually exist
  5. identity   stem and module agree with the lock; no duplicate designs

  python3 scripts/build_manifest.py
  python3 scripts/build_manifest.py --check      # validate only, write nothing
  python3 scripts/build_manifest.py --plan-only  # stop after the plan, skip the lock gate
"""

from __future__ import annotations

import argparse
import glob
import json
import os
import sys

import yaml
from jsonschema import Draft202012Validator

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DESC_DIR = os.path.join(ROOT, "descriptors")
LOCK_DIR = os.path.join(DESC_DIR, "_locks")
OUT = os.path.join(ROOT, "generated", "descriptor_manifest.json")
PLAN_OUT = os.path.join(ROOT, "generated", "elaboration_plan.json")

# Roles the driver understands. Anything else in ports is a typo.
KNOWN_ROLES = {
    "a", "b", "c", "result", "flags", "rounding_mode", "tininess",
    "enable", "valid_in", "valid_out", "accept_out", "select",
}


class ContractError(Exception):
    pass


def flatten(chisel_name):
    """Chisel bundle path to RTL signal. io.in_a becomes io_in_a."""
    return chisel_name.replace(".", "_")


def load_locks():
    locks = {}
    for f in glob.glob(os.path.join(LOCK_DIR, "*.json")):
        with open(f) as fh:
            d = json.load(fh)
        locks[d["design"]] = d
    return locks


def check_one(desc, lock, path):
    """Run every contract layer for one descriptor. Returns the resolved port map."""
    where = os.path.relpath(path, ROOT)
    design = f"{desc['library']}/{desc['stem']}"

    # 5. identity
    if lock["module"] != desc["module"]:
        raise ContractError(
            f"{where}: module is '{desc['module']}' but {design} elaborates to '{lock['module']}'")

    lock_ports = {p["name"]: p for p in lock["ports"]}
    resolved = {}

    # 2. binding
    for role, spec in desc["ports"].items():
        if role not in KNOWN_ROLES:
            raise ContractError(
                f"{where}: unknown role '{role}'. Known roles: {', '.join(sorted(KNOWN_ROLES))}")
        rtl = spec.get("rtl") or flatten(spec["chisel"])
        if rtl not in lock_ports:
            near = [n for n in lock_ports if n.endswith(rtl.split("_")[-1])]
            hint = f" Did you mean {near}?" if near else ""
            raise ContractError(
                f"{where}: role '{role}' maps to '{rtl}', which {design} does not have. "
                f"Module has: {', '.join(sorted(lock_ports))}.{hint}")
        actual = lock_ports[rtl]
        if actual["dir"] != spec["dir"]:
            raise ContractError(
                f"{where}: role '{role}' ({rtl}) declared {spec['dir']} but is {actual['dir']}")
        if actual["width"] != spec["width"]:
            raise ContractError(
                f"{where}: role '{role}' ({rtl}) declared width {spec['width']} but is {actual['width']}")
        resolved[role] = dict(spec, rtl=rtl)

    # 3. coverage
    implicit = set()
    for field, fallback in (("clock", "clock"), ("reset", "reset")):
        name = desc["sim"].get(field, fallback)
        if name is None:
            continue
        if field in desc["sim"] and name not in lock_ports:
            near = [n for n in lock_ports if field[:3] in n.lower()]
            hint = f" Did you mean {near}?" if near else ""
            raise ContractError(
                f"{where}: sim.{field} is '{name}', which {design} does not have. "
                f"Module has: {', '.join(sorted(lock_ports))}.{hint} "
                f"Use null if this design has no {field}.")
        implicit.add(name)

    mapped = {r["rtl"] for r in resolved.values()}
    ignored = set(desc.get("ignore_ports", []))
    for name in ignored:
        if name not in lock_ports:
            raise ContractError(f"{where}: ignore_ports lists '{name}', which the module does not have")
    unaccounted = set(lock_ports) - mapped - ignored - implicit
    if unaccounted:
        raise ContractError(
            f"{where}: unaccounted ports on {design}: {', '.join(sorted(unaccounted))}. "
            f"Map them to a role or list them in ignore_ports.")

    # 4. profile
    prof = desc["profile"]
    has_flags = "flags" in resolved
    if prof["exception_flags"] == "ieee5" and not has_flags:
        raise ContractError(
            f"{where}: profile claims exception_flags ieee5 but no 'flags' port is mapped. "
            f"A claimed flag check with no flag port would pass silently.")
    if prof["exception_flags"] == "none" and has_flags:
        raise ContractError(
            f"{where}: profile says exception_flags none but a 'flags' port is mapped. "
            f"Set exception_flags to ieee5 or drop the port.")

    has_rm = "rounding_mode" in resolved
    if prof["rounding_control"] == "port" and not has_rm:
        raise ContractError(
            f"{where}: rounding_control is 'port' but no 'rounding_mode' port is mapped")
    if prof["rounding_control"] != "port" and has_rm:
        raise ContractError(
            f"{where}: a 'rounding_mode' port is mapped but rounding_control is "
            f"'{prof['rounding_control']}'. Set it to 'port'.")
    if prof["rounding_control"] != "port" and len(prof["rounding_modes"]) > 1:
        raise ContractError(
            f"{where}: claims {len(prof['rounding_modes'])} rounding modes but has no rounding_mode "
            f"port. One DUT cannot cover several modes without one.")
    if desc.get("tier", 1) == 2 and "ulp_budget" not in prof:
        raise ContractError(f"{where}: tier 2 requires profile.ulp_budget")
    if desc.get("tier", 1) == 1 and "ulp_budget" in prof:
        raise ContractError(f"{where}: tier 1 is bit exact; drop profile.ulp_budget")

    # fixed_latency needs a latency
    if desc["sim"]["protocol"] == "fixed_latency" and "latency" not in desc["sim"]:
        raise ContractError(f"{where}: fixed_latency protocol requires sim.latency")

    return resolved


def conformance_level(prof):
    """strict means every mode plus exact flags. Recorded per design, not inferred later."""
    all_modes = {"rne", "rtz", "rdn", "rup", "rna", "rto"}
    if prof["exception_flags"] == "ieee5" and set(prof["rounding_modes"]) >= all_modes:
        return "strict"
    if prof["exception_flags"] == "ieee5" or len(prof["rounding_modes"]) > 1:
        return "reduced"
    return "minimal"


def load_descriptors():
    """Schema-check every YAML. Returns (accepted, errors); accepted is [(path, where, desc)]."""
    with open(os.path.join(DESC_DIR, "schema.json")) as fh:
        schema = json.load(fh)
    validator = Draft202012Validator(schema)

    paths = sorted(
        p for p in glob.glob(os.path.join(DESC_DIR, "*", "*.yaml"))
        if os.path.basename(os.path.dirname(p)) != "_locks"
    )
    if not paths:
        raise ContractError(f"no descriptors found under {DESC_DIR}")

    accepted, errors, seen = [], [], {}
    for path in paths:
        where = os.path.relpath(path, ROOT)
        with open(path) as fh:
            desc = yaml.safe_load(fh)

        # 1. schema
        schema_errors = sorted(validator.iter_errors(desc), key=lambda e: e.path)
        if schema_errors:
            for e in schema_errors:
                loc = "/".join(str(x) for x in e.path) or "<root>"
                errors.append(f"{where}: schema at {loc}: {e.message}")
            continue

        design = f"{desc['library']}/{desc['stem']}"
        if design in seen:
            errors.append(f"{where}: design {design} already declared by {seen[design]}")
            continue
        seen[design] = where
        accepted.append((path, where, desc))

    return accepted, errors


def build_plan(accepted):
    """Elaboration list for Scala. Designs carrying "source" already have RTL, so they are absent."""
    designs = []
    for _, where, desc in accepted:
        if "generator" not in desc:
            continue
        designs.append({
            "design_id": f"{desc['library']}/{desc['stem']}",
            "descriptor_path": where,
            "library": desc["library"],
            "stem": desc["stem"],
            "module": desc["module"],
            "generator": desc["generator"],
        })
    return {"version": 1, "designs": designs}


def source_designs(accepted):
    return [(w, d) for _, w, d in accepted if "source" in d]


def report(errors):
    print(f"\n{len(errors)} contract violation(s):\n", file=sys.stderr)
    for e in errors:
        print(f"  {e}", file=sys.stderr)


def build(check_only=False, plan_only=False):
    accepted, errors = load_descriptors()
    plan = build_plan(accepted)

    # Kill partial plans
    if errors:
        report(errors)
        return None, plan, errors

    if not check_only:
        os.makedirs(os.path.dirname(PLAN_OUT), exist_ok=True)
        with open(PLAN_OUT, "w") as fh:
            json.dump(plan, fh, indent=2)
            fh.write("\n")

    if plan_only:
        return None, plan, []

    locks = load_locks()
    entries = []
    for path, where, desc in accepted:
        design = f"{desc['library']}/{desc['stem']}"

        if design not in locks:
            hint = desc.get("source") or f"generated/{desc['library']}/{desc['stem']}.sv"
            errors.append(
                f"{where}: no lockfile for {design}. Elaborate it, then run "
                f"scripts/scaffold.py {hint} --library {desc['library']}")
            continue

        try:
            resolved = check_one(desc, locks[design], path)
        except ContractError as exc:
            errors.append(str(exc))
            continue

        fmt = desc["format"]
        entries.append({
            "design_id": design,
            "descriptor_path": where,
            "library": desc["library"],
            "operator": desc["operator"],
            "stem": desc["stem"],
            "module": desc["module"],
            "tier": desc.get("tier", 1),
            "format": fmt,
            "generator": desc.get("generator"),
            "source": desc.get("source"),
            "sim": desc["sim"],
            "ports": resolved,
            "ignore_ports": desc.get("ignore_ports", []),
            "profile": desc["profile"],
            "conformance_level": conformance_level(desc["profile"]),
            "stimulus": desc.get("stimulus", {}),
            "notes": desc.get("notes"),
            "derived": {
                "precision": fmt.get("name"),
                "exponent_width": fmt["exp_bits"],
                "significand_width": fmt["sig_bits"],
                "mantissa_width": fmt["sig_bits"] - 1,
                "bitwidth": fmt["exp_bits"] + fmt["sig_bits"],
                "source_path": desc.get("source")
                or f"generated/{desc['library']}/{desc['stem']}.sv",
            },
        })

    if errors:
        report(errors)
        return None, plan, errors

    manifest = {
        "version": 1,
        "libraries": sorted({e["library"] for e in entries}),
        "designs": entries,
    }
    if not check_only:
        os.makedirs(os.path.dirname(OUT), exist_ok=True)
        with open(OUT, "w") as fh:
            json.dump(manifest, fh, indent=2)
            fh.write("\n")
    return manifest, plan, []


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true", help="Validate only; write nothing.")
    ap.add_argument("--plan-only", action="store_true",
                    help="Write the elaboration plan and stop. Skips the lockfile gate.")
    args = ap.parse_args()

    try:
        manifest, plan, errors = build(check_only=args.check, plan_only=args.plan_only)
    except ContractError as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    if errors:
        return 1

    if args.plan_only:
        print(f"{len(plan['designs'])} designs to elaborate")
        if not args.check:
            print(f"-> {os.path.relpath(PLAN_OUT, ROOT)}")
        return 0

    by_lib = {}
    by_level = {}
    for e in manifest["designs"]:
        by_lib[e["library"]] = by_lib.get(e["library"], 0) + 1
        by_level[e["conformance_level"]] = by_level.get(e["conformance_level"], 0) + 1

    print(f"{len(manifest['designs'])} designs pass the contract")
    for lib in sorted(by_lib):
        print(f"  {lib:12s} {by_lib[lib]}")
    print("conformance:", ", ".join(f"{k}={v}" for k, v in sorted(by_level.items())))
    elaborated = len(plan["designs"])
    print(f"sources: {elaborated} elaborated, {len(manifest['designs']) - elaborated} pre-existing RTL")
    if not args.check:
        print(f"-> {os.path.relpath(PLAN_OUT, ROOT)}")
        print(f"-> {os.path.relpath(OUT, ROOT)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

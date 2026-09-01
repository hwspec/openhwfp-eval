#!/usr/bin/env python3
"""Extract a module's real port list into a lockfile, and hold the RTL to it afterwards.

Lockfiles are machine-owned artifacts that make an upstream port rename a build error instead of
an unbound signal. The check compares the freshly extracted port map against the stored one, so
it works in a tarball, a vendored copy, or anywhere else git is not.

  python3 scripts/scaffold.py generated/                    create missing, fail on drift
  python3 scripts/scaffold.py generated/ --check            write nothing, fail on drift or missing
  python3 scripts/scaffold.py generated/ --update           accept the RTL as the new truth
  python3 scripts/scaffold.py cvfpu/src/fpnew_top.sv --library cvfpu

Only module name, port names, directions and widths count as drift. Port order and the recorded
Verilator version do not, so a toolchain upgrade is not a false alarm.

Library is read from the path for elaborated RTL under generated/<library>/. Anything living
elsewhere, a submodule especially, needs --library.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from estimate import preprocess_sv_file  # noqa: E402  shared with the Yosys and ORFS paths

LOCK_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "descriptors", "_locks")


def _walk(node, want, out):
    if isinstance(node, dict):
        if node.get("type") == want:
            out.append(node)
        for v in node.values():
            _walk(v, want, out)
    elif isinstance(node, list):
        for v in node:
            _walk(v, want, out)
    return out


def _width(range_str):
    # Absent range means a scalar, "31:0" means 32 bits
    if not range_str:
        return 1
    left, right = range_str.split(":")
    return int(left) - int(right) + 1


def verilator_ports(sv_path, top=None):
    """Return (top_module_name, [{name, dir, width}]) via verilator --json-only."""
    # firtool emits `include "layers-<mod>-Verification.sv" for files it writes separately
    # The same preprocessing feeds Yosys and ORFS
    content = preprocess_sv_file(sv_path)
    if content is None:
        raise RuntimeError(f"preprocessing failed for {sv_path}")

    with tempfile.TemporaryDirectory() as tmp:
        staged = os.path.join(tmp, os.path.basename(sv_path))
        with open(staged, "w") as fh:
            fh.write(content)

        cmd = ["verilator", "--json-only", "-Wno-fatal", "-Mdir", tmp]
        if top:
            cmd += ["--top-module", top]
        cmd.append(staged)
        proc = subprocess.run(cmd, capture_output=True, text=True, check=False)
        trees = [f for f in os.listdir(tmp) if f.endswith(".tree.json")]
        if not trees:
            raise RuntimeError(f"verilator produced no AST for {sv_path}\n{proc.stderr[:800]}")
        with open(os.path.join(tmp, trees[0])) as fh:
            tree = json.load(fh)

    widths = {}
    for dt in _walk(tree, "BASICDTYPE", []):
        widths[dt["addr"]] = _width(dt.get("range"))

    modules = _walk(tree, "MODULE", [])
    if not modules:
        raise RuntimeError(f"no MODULE node in {sv_path}")
    # Verilator marks the elaborated top; fall back to the first module it kept.
    tops = [m for m in modules if m.get("topModule")] or modules[:1]
    top_mod = tops[0]
    top_name = top_mod.get("origName") or top_mod.get("name")

    ports = []
    for var in _walk(top_mod, "VAR", []):
        direction = var.get("direction")
        if direction not in ("INPUT", "OUTPUT"):
            continue
        ports.append({
            "name": var["name"],
            "dir": "in" if direction == "INPUT" else "out",
            "width": widths.get(var.get("dtypep"), 1),
        })
    # Verilator can visit a port more than once; keep first sighting
    seen, uniq = set(), []
    for p in ports:
        if p["name"] not in seen:
            seen.add(p["name"])
            uniq.append(p)
    return top_name, uniq


def verilator_version():
    out = subprocess.run(["verilator", "--version"], capture_output=True,
                         text=True, check=False).stdout.strip()
    return out or "unknown"


def contract(lock):
    """The part of a lockfile that is the contract. Everything else is provenance."""
    return {
        "module": lock["module"],
        "ports": {p["name"]: (p["dir"], p["width"]) for p in lock["ports"]},
    }


def drift(old, new):
    """What moved between two port contracts, one line each. Empty means they agree."""
    lines = []
    if old["module"] != new["module"]:
        lines.append(f"top module {old['module']} -> {new['module']}")
    o, n = old["ports"], new["ports"]
    for name in sorted(set(o) - set(n)):
        lines.append(f"port removed: {name} ({o[name][0]}, {o[name][1]} bits)")
    for name in sorted(set(n) - set(o)):
        lines.append(f"port added:   {name} ({n[name][0]}, {n[name][1]} bits)")
    for name in sorted(set(o) & set(n)):
        if o[name][0] != n[name][0]:
            lines.append(f"{name}: direction {o[name][0]} -> {n[name][0]}")
        if o[name][1] != n[name][1]:
            lines.append(f"{name}: width {o[name][1]} -> {n[name][1]}")
    return lines


def build_lock(sv_path, repo_root, library=None):
    """Extract the port map. Writes nothing."""
    rel = os.path.relpath(sv_path, repo_root)
    parts = rel.replace("\\", "/").split("/")
    if library is None:
        if len(parts) > 2 and parts[0] == "generated":
            library = parts[1]
        else:
            raise RuntimeError(f"cannot infer library from {rel}. Pass --library.")
    stem = os.path.splitext(os.path.basename(sv_path))[0]

    top, ports = verilator_ports(sv_path)
    lock = {
        "design": f"{library}/{stem}",
        "library": library,
        "stem": stem,
        "module": top,
        "source": rel,
        "tool": verilator_version(),
        "ports": ports,
    }
    return os.path.join(LOCK_DIR, f"{library}__{stem}.json"), lock


def write_lock(out_path, lock):
    os.makedirs(LOCK_DIR, exist_ok=True)
    with open(out_path, "w") as fh:
        json.dump(lock, fh, indent=2)
        fh.write("\n")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("path", help="An .sv file or a directory to walk.")
    ap.add_argument("--library", help="Override the library name. Required outside generated/.")
    ap.add_argument("--update", action="store_true",
                    help="Accept the RTL as the new truth and rewrite every lockfile.")
    ap.add_argument("--check", action="store_true",
                    help="Write nothing, fail when lockfile and RTL aren't coherent, or a lockfile doesn't exist yet")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args()

    if args.update and args.check:
        print("--update and --check contradict each other", file=sys.stderr)
        return 2

    repo_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    targets = []
    if os.path.isdir(args.path):
        for root, _, files in os.walk(args.path):
            targets += [os.path.join(root, f) for f in sorted(files) if f.endswith(".sv")]
    else:
        targets = [args.path]

    if not targets:
        print(f"no .sv files under {args.path}", file=sys.stderr)
        return 1

    created, matched, drifted, absent, failures = [], [], [], [], []

    for sv in targets:
        try:
            out_path, fresh = build_lock(sv, repo_root, args.library)
        except Exception as exc:
            failures.append((sv, str(exc).splitlines()[0]))
            continue

        design = fresh["design"]
        if not os.path.exists(out_path):
            if args.check:
                absent.append(design)
            else:
                write_lock(out_path, fresh)
                created.append(design)
            continue

        with open(out_path) as fh:
            stored = json.load(fh)
        moved = drift(contract(stored), contract(fresh))

        if not moved:
            matched.append(design)
            if args.update and stored.get("tool") != fresh["tool"]:
                write_lock(out_path, fresh)
        elif args.update:
            write_lock(out_path, fresh)
            created.append(design)
        else:
            drifted.append((design, moved))

    if not args.quiet:
        for design in created:
            print(f"  wrote   {design}")
        for design in matched:
            print(f"  ok      {design}")

    print(f"\n{len(matched)} match, {len(created)} written, {len(drifted)} drifted, "
          f"{len(absent)} missing, {len(failures)} failed  ({len(targets)} designs)")

    for design, moved in drifted:
        print(f"\ndrift in {design}:", file=sys.stderr)
        for line in moved:
            print(f"  {line}", file=sys.stderr)
    if drifted:
        print("\nThe RTL no longer matches its lockfile. Update the descriptor to follow, then\n"
              "rerun with --update to accept the new port map.", file=sys.stderr)

    for design in absent:
        print(f"missing lockfile: {design}", file=sys.stderr)
    if absent:
        print("Run without --check to create them.", file=sys.stderr)

    for sv, msg in failures:
        print(f"FAILED {sv}: {msg}", file=sys.stderr)

    return 1 if (drifted or absent or failures) else 0


if __name__ == "__main__":
    sys.exit(main())

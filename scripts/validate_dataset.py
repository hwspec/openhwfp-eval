#!/usr/bin/env python3
"""
Validate the FORGE flow-instance dataset. Read-only: this script never writes.

Checks structure, ID uniqueness, flow-key collisions, and whether records that
claim an implementation result actually carry the evidence fields that support it.

Usage (from repo root):
  python3 scripts/validate_dataset.py
  python3 scripts/validate_dataset.py --dataset dataset/flow_instances.jsonl
  python3 scripts/validate_dataset.py --strict     # warnings become errors

Exit status:
  0  required checks passed
  1  schema violation, duplicate ID, flow-key collision, or missing critical evidence
  2  dataset/schema unreadable, or jsonschema not installed
"""

from __future__ import annotations

import argparse
import collections
import json
import os
import sys
from typing import Any, Dict, List, Optional, Tuple

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))

# overall_status is a free-form string in the current schema. Two writers emit it:
#   export_flow_instances.py:overall_status()   -> pass|fail|ignored:<gate>, unknown
#   extract_orfs_metrics.py:_overall_from_impl() -> fail:<failure_stage>, pass:implementation
# Track the vocabulary so drift is visible before the schema is tightened.
KNOWN_PREFIXES = {"pass", "fail", "ignored", "unknown"}
KNOWN_STAGES = {
    "verification", "synthesis", "implementation",
    "timing", "route", "drc", "placement", "cts", "synth", "grt",
}

IMPL_ACTIVE = {"pass", "fail"}

# Fields without which an implementation row cannot be traced back to its run.
EVIDENCE_ALWAYS = ("nickname", "clock_period_ps", "report_dir")
EVIDENCE_ON_PASS = ("area_um2", "route_drc")
EVIDENCE_SLACK = ("wns", "setup_slack_ps")          # at least one required on pass
EVIDENCE_ON_FAIL = ("failure_stage", "failure_message")  # at least one required on fail
EVIDENCE_OPTIONAL = ("tns", "fmax_mhz", "util_percent", "achieved_period_ps")


class Report:
    """Collects findings. Errors set the exit code; warnings are advisory."""

    def __init__(self, strict: bool = False) -> None:
        self.errors: List[str] = []
        self.warnings: List[str] = []
        self.strict = strict

    def error(self, msg: str) -> None:
        self.errors.append(msg)

    def warn(self, msg: str) -> None:
        self.warnings.append(msg)

    @property
    def failed(self) -> bool:
        return bool(self.errors) or (self.strict and bool(self.warnings))


def load_rows(path: str, rep: Report) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    with open(path, "r", encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                obj = json.loads(line)
            except json.JSONDecodeError as exc:
                rep.error(f"line {lineno}: malformed JSON: {exc}")
                continue
            if not isinstance(obj, dict):
                rep.error(f"line {lineno}: expected object, got {type(obj).__name__}")
                continue
            obj["__line__"] = lineno
            rows.append(obj)
    return rows


def impl_of(rec: Dict[str, Any]) -> Dict[str, Any]:
    return rec.get("implementation") or {}


def label(rec: Dict[str, Any]) -> str:
    nick = impl_of(rec).get("nickname")
    base = f"line {rec.get('__line__')} {rec.get('design_id')}"
    return f"{base} [{nick}]" if nick else base


# --- distributions (checks 1-8) ---------------------------------------------

def print_distributions(rows: List[Dict[str, Any]]) -> None:
    def dist(name: str, values) -> None:
        counts = collections.Counter(values)
        rendered = ", ".join(f"{k}={v}" for k, v in sorted(counts.items(), key=lambda kv: (-kv[1], str(kv[0]))))
        print(f"  {name:<16} {rendered}")

    n = len(rows)
    print("Counts")
    print(f"  {'rows':<16} {n}")
    print(f"  {'design_id':<16} {len({r.get('design_id') for r in rows})} unique")
    print(f"  {'flow_instance_id':<16} {len({r.get('flow_instance_id') for r in rows})} unique")
    print()
    print("Distributions")
    dist("library", (r.get("library") for r in rows))
    dist("verification", ((r.get("verification") or {}).get("status") for r in rows))
    dist("synthesis", ((r.get("synthesis") or {}).get("status") for r in rows))
    dist("implementation", (impl_of(r).get("status") for r in rows))
    dist("overall_status", (r.get("overall_status") for r in rows))


# --- check 9: schema ---------------------------------------------------------

def check_schema(rows: List[Dict[str, Any]], schema_path: str, rep: Report) -> None:
    try:
        from jsonschema import Draft202012Validator
    except ImportError:
        print("ERROR: jsonschema is not installed (pip install jsonschema)", file=sys.stderr)
        raise SystemExit(2)

    with open(schema_path, "r", encoding="utf-8") as f:
        schema = json.load(f)
    validator = Draft202012Validator(schema)

    bad = 0
    for rec in rows:
        payload = {k: v for k, v in rec.items() if k != "__line__"}
        for err in sorted(validator.iter_errors(payload), key=str):
            where = "/".join(str(p) for p in err.path) or "<root>"
            rep.error(f"{label(rec)}: schema: {where}: {err.message}")
            bad += 1
    print(f"  schema           {len(rows) - bad if bad <= len(rows) else 0}/{len(rows)} rows valid "
          f"({bad} violation{'s' if bad != 1 else ''})")


# --- check 10: duplicate flow_instance_id ------------------------------------

def check_duplicate_ids(rows: List[Dict[str, Any]], rep: Report) -> None:
    seen: Dict[str, List[Dict[str, Any]]] = collections.defaultdict(list)
    for rec in rows:
        seen[rec.get("flow_instance_id")].append(rec)
    dups = {k: v for k, v in seen.items() if len(v) > 1}
    for fid, recs in sorted(dups.items(), key=lambda kv: str(kv[0])):
        where = "; ".join(label(r) for r in recs)
        rep.error(f"duplicate flow_instance_id {fid}: {where}")
    print(f"  unique ids       {len(seen)}/{len(rows)} ({len(dups)} collision{'s' if len(dups) != 1 else ''})")


# --- check 11: flow-key collisions and overwrite hazards ---------------------

def flow_key(rec: Dict[str, Any]) -> Tuple[Any, Any, Any, Any]:
    impl = impl_of(rec)
    return (
        rec.get("design_id"),
        impl.get("backend"),
        impl.get("clock_period_ps"),
        impl.get("nickname"),
    )


def check_flow_keys(rows: List[Dict[str, Any]], rep: Report) -> None:
    exact: Dict[Tuple, List[Dict[str, Any]]] = collections.defaultdict(list)
    for rec in rows:
        exact[flow_key(rec)].append(rec)
    collisions = {k: v for k, v in exact.items() if len(v) > 1}
    for key, recs in collisions.items():
        did, backend, period, nick = key
        rep.error(
            f"flow-key collision (design_id={did}, backend={backend}, "
            f"period={period}, nickname={nick}): {'; '.join(label(r) for r in recs)}"
        )

    # extract_orfs_metrics.merge_impl matches on (design_id, clock_period_ps) alone --
    # its nickname guard is dead code -- so two nicknames at one period silently
    # overwrite each other on the next merge.
    hazard: Dict[Tuple, set] = collections.defaultdict(set)
    for rec in rows:
        impl = impl_of(rec)
        if impl.get("status") in IMPL_ACTIVE and impl.get("clock_period_ps") is not None:
            hazard[(rec.get("design_id"), impl.get("clock_period_ps"))].add(impl.get("nickname"))
    for (did, period), nicks in sorted(hazard.items(), key=lambda kv: str(kv[0])):
        if len(nicks) > 1:
            rep.warn(
                f"overwrite hazard: design_id={did} period={period} has "
                f"{len(nicks)} nicknames {sorted(map(str, nicks))} -- merge_impl "
                f"matches on period alone and will overwrite one"
            )
    print(f"  flow keys        {len(exact)} distinct ({len(collisions)} collision"
          f"{'s' if len(collisions) != 1 else ''})")


# --- check 12: report_dir existence ------------------------------------------

def check_report_dirs(rows: List[Dict[str, Any]], rep: Report) -> None:
    checked = missing = 0
    for rec in rows:
        impl = impl_of(rec)
        if impl.get("status") not in IMPL_ACTIVE:
            continue
        rd = impl.get("report_dir")
        if not rd:
            continue  # absence is reported by the evidence check
        checked += 1
        if not os.path.isdir(os.path.join(REPO_ROOT, rd)):
            missing += 1
            rep.warn(f"{label(rec)}: report_dir not on this host: {rd}")
    print(f"  report_dir       {checked - missing}/{checked} resolve on this host")
    if checked and missing == 0:
        # Present locally is not the same as reproducible for a reader.
        sample = next((impl_of(r).get("report_dir") for r in rows
                       if impl_of(r).get("status") in IMPL_ACTIVE and impl_of(r).get("report_dir")), None)
        if sample and sample.split("/")[0] == "openroad_results":
            rep.warn(
                "all report_dir values point into openroad_results/, which is "
                "gitignored -- these paths will not resolve in a fresh clone"
            )


# --- checks 13 + 14: evidence fields on implementation rows ------------------

def check_evidence(rows: List[Dict[str, Any]], rep: Report) -> None:
    active = 0
    for rec in rows:
        impl = impl_of(rec)
        status = impl.get("status")
        if status not in IMPL_ACTIVE:
            continue
        active += 1

        for field in EVIDENCE_ALWAYS:
            if impl.get(field) in (None, ""):
                rep.error(f"{label(rec)}: implementation.status={status} but {field} is missing")

        if status == "pass":
            for field in EVIDENCE_ON_PASS:
                if impl.get(field) is None:
                    rep.error(f"{label(rec)}: implementation.status=pass but {field} is missing")
            if all(impl.get(f) is None for f in EVIDENCE_SLACK):
                rep.error(f"{label(rec)}: implementation.status=pass but no slack "
                          f"({' or '.join(EVIDENCE_SLACK)})")

        if status == "fail":
            if all(impl.get(f) in (None, "") for f in EVIDENCE_ON_FAIL):
                rep.error(f"{label(rec)}: implementation.status=fail but no "
                          f"{' or '.join(EVIDENCE_ON_FAIL)} -- an undocumented failure "
                          f"is not a usable record")
            if impl.get("area_um2") is None:
                rep.warn(f"{label(rec)}: failed run has no area_um2")

        thin = [f for f in EVIDENCE_OPTIONAL if impl.get(f) is None]
        if thin:
            rep.warn(f"{label(rec)}: sparse metrics, missing {', '.join(thin)}")

    print(f"  evidence         {active} implementation row{'s' if active != 1 else ''} checked")


# --- check 15: overall_status vocabulary and consistency ---------------------

def derived_status(rec: Dict[str, Any]) -> Optional[str]:
    """Mirror of export_flow_instances.overall_status(), for cross-checking."""
    verif = rec.get("verification") or {}
    synth = rec.get("synthesis") or {}
    impl = impl_of(rec)
    for stage, part in (("implementation", impl), ("synthesis", synth), ("verification", verif)):
        if part.get("status") == "fail":
            return f"fail:{stage}"
        if part.get("status") == "ignored":
            return f"ignored:{stage}"
    if synth.get("status") == "pass":
        return "pass:synthesis"
    if verif.get("status") == "pass":
        return "pass:verification"
    return "unknown"


def check_overall_status(rows: List[Dict[str, Any]], rep: Report) -> None:
    vocab = collections.Counter()
    downgradable: List[Dict[str, Any]] = []
    for rec in rows:
        raw = rec.get("overall_status")
        vocab[raw] += 1
        if not isinstance(raw, str) or not raw:
            rep.error(f"{label(rec)}: overall_status missing or not a string: {raw!r}")
            continue
        prefix, _, stage = raw.partition(":")
        if prefix not in KNOWN_PREFIXES:
            rep.error(f"{label(rec)}: overall_status {raw!r} has unknown prefix {prefix!r} "
                      f"(expected one of {sorted(KNOWN_PREFIXES)})")
            continue
        if prefix == "unknown":
            if stage:
                rep.warn(f"{label(rec)}: overall_status {raw!r} -- 'unknown' takes no stage")
        elif not stage:
            rep.warn(f"{label(rec)}: overall_status {raw!r} has no stage suffix")
        elif stage not in KNOWN_STAGES:
            rep.warn(f"{label(rec)}: overall_status {raw!r} uses unrecognised stage {stage!r} "
                     f"(known: {sorted(KNOWN_STAGES)})")

        # The two writers disagree by design. extract_orfs_metrics._overall_from_impl
        # emits the richer implementation-derived value (pass:implementation,
        # fail:<failure_stage>); export_flow_instances.overall_status() cannot produce
        # either -- it has no pass:implementation branch and returns pass:synthesis
        # first. Those rows are correct as written, so count them instead of warning
        # per row, and report the re-export hazard once.
        want = derived_status(rec)
        if want and raw != want:
            impl_status = impl_of(rec).get("status")
            if (impl_status == "fail" and raw.startswith("fail:")) or \
               (impl_status == "pass" and raw == "pass:implementation"):
                downgradable.append(rec)
            else:
                rep.warn(f"{label(rec)}: overall_status {raw!r} disagrees with component "
                         f"statuses (derived {want!r})")

    if downgradable:
        rep.warn(
            f"{len(downgradable)} row(s) carry an implementation-derived overall_status "
            f"that export_flow_instances.overall_status() cannot produce -- re-exporting "
            f"would downgrade them to synthesis-level status"
        )
    print(f"  overall_status   {len(vocab)} distinct value{'s' if len(vocab) != 1 else ''}")


# --- driver ------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(description="Validate the FORGE flow-instance dataset (read-only)")
    ap.add_argument("--dataset", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.jsonl"))
    ap.add_argument("--schema", default=os.path.join(REPO_ROOT, "dataset", "schema.json"))
    ap.add_argument("--strict", action="store_true", help="treat warnings as errors")
    args = ap.parse_args()

    for path, what in ((args.dataset, "dataset"), (args.schema, "schema")):
        if not os.path.isfile(path):
            print(f"ERROR: {what} not found: {path}", file=sys.stderr)
            return 2

    rep = Report(strict=args.strict)
    rows = load_rows(args.dataset, rep)
    if not rows:
        print(f"ERROR: no records in {args.dataset}", file=sys.stderr)
        return 2

    rel = os.path.relpath(args.dataset, REPO_ROOT)
    print(f"FORGE dataset validation -- {rel}")
    print()
    print_distributions(rows)
    print()
    print("Checks")
    check_schema(rows, args.schema, rep)
    check_duplicate_ids(rows, rep)
    check_flow_keys(rows, rep)
    check_report_dirs(rows, rep)
    check_evidence(rows, rep)
    check_overall_status(rows, rep)

    if rep.warnings:
        print()
        print(f"Warnings ({len(rep.warnings)})")
        for msg in rep.warnings:
            print(f"  WARN  {msg}")
    if rep.errors:
        print()
        print(f"Errors ({len(rep.errors)})")
        for msg in rep.errors:
            print(f"  ERROR {msg}")

    print()
    if rep.errors:
        print(f"FAIL  {len(rep.errors)} error(s), {len(rep.warnings)} warning(s)")
    elif rep.warnings and args.strict:
        print(f"FAIL  0 errors, {len(rep.warnings)} warning(s) (--strict)")
    elif rep.warnings:
        print(f"OK    0 errors, {len(rep.warnings)} warning(s)")
    else:
        print("OK    all checks passed")
    return 1 if rep.failed else 0


if __name__ == "__main__":
    raise SystemExit(main())

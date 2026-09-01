"""Roll verification records up into a coverage matrix.

  python3 -m scripts.verification.summarize
  python3 -m scripts.verification.summarize --json summary.json
"""

from __future__ import annotations

import argparse
import collections
import glob
import json
import os
import sys

from .record import RESULTS_DIR


def load(results_dir):
    files = sorted(glob.glob(os.path.join(results_dir, "*.json")))
    records = []
    for f in files:
        with open(f) as fh:
            records.append(json.load(fh))
    return records


def rollup(records):
    by_design = collections.OrderedDict()
    for r in records:
        d = by_design.setdefault(r["design_id"], {
            "design_id": r["design_id"],
            "library": r["library"],
            "operator": r["operator"],
            "precision": r["precision"],
            "tier": r["tier"],
            "conformance_level": r["conformance_level"],
            "flag_check": r["flag_check"],
            "reference_model": r["reference_model"],
            "runs": 0, "passed": 0, "failed": 0,
            "checks": 0, "mismatches": 0,
            "modes": set(), "tininess": set(),
            "max_ulp": 0.0, "coverage": collections.Counter(),
            "canary_ok": True,
            "not_evaluated_reason": r["profile"].get("not_evaluated_reason"),
        })
        d["runs"] += 1
        d["passed" if r["status"] == "pass" else "failed"] += 1
        d["checks"] += r["checks_performed"]
        d["mismatches"] += r["mismatch_count"]
        d["modes"].add(r["rounding_mode"])
        d["tininess"].add(r["tininess"])
        d["max_ulp"] = max(d["max_ulp"], r["max_ulp"])
        d["coverage"].update(r["special_case_coverage"])
        d["canary_ok"] = d["canary_ok"] and r.get("canary_ok", False)

    for d in by_design.values():
        d["modes"] = sorted(d["modes"])
        d["tininess"] = sorted(d["tininess"])
        d["coverage"] = dict(sorted(d["coverage"].items()))
        d["status"] = "pass" if d["failed"] == 0 and d["checks"] > 0 else "fail"
    return list(by_design.values())


def render(rows):
    if not rows:
        print("no records found")
        return

    width = max(len(r["design_id"]) for r in rows)
    print(f"{'design':<{width}}  {'lvl':<8} {'flags':<5} {'runs':>4} {'checks':>10} "
          f"{'mismatch':>9}  {'modes':<24} status")
    print("-" * (width + 74))
    for r in sorted(rows, key=lambda x: (x["library"], x["operator"], x["precision"] or "")):
        modes = "+".join(r["modes"])
        mark = "pass" if r["status"] == "pass" else "FAIL"
        canary = "" if r["canary_ok"] else "  [CANARY NOT CONFIRMED]"
        print(f"{r['design_id']:<{width}}  {r['conformance_level']:<8} {r['flag_check']:<5} "
              f"{r['runs']:>4} {r['checks']:>10,} {r['mismatches']:>9,}  {modes:<24} {mark}{canary}")

    print()
    by_lib = collections.defaultdict(lambda: {"pass": 0, "fail": 0, "checks": 0})
    by_level = collections.Counter()
    for r in rows:
        by_lib[r["library"]]["pass" if r["status"] == "pass" else "fail"] += 1
        by_lib[r["library"]]["checks"] += r["checks"]
        by_level[r["conformance_level"]] += 1

    for lib in sorted(by_lib):
        s = by_lib[lib]
        print(f"  {lib:<12} {s['pass']:>3} pass  {s['fail']:>3} fail   {s['checks']:>12,} checks")
    print("  conformance: " + ", ".join(f"{k}={v}" for k, v in sorted(by_level.items())))

    total_checks = sum(r["checks"] for r in rows)
    print(f"\n  {sum(1 for r in rows if r['status']=='pass')}/{len(rows)} designs pass, "
          f"{total_checks:,} checks total")

    reduced = [r for r in rows if r["conformance_level"] != "strict"]
    if reduced:
        print("\n  Reduced coverage, by declaration:")
        seen = set()
        for r in reduced:
            reason = r["not_evaluated_reason"]
            if reason and reason not in seen:
                seen.add(reason)
                libs = sorted({x["library"] for x in reduced if x["not_evaluated_reason"] == reason})
                print(f"    {', '.join(libs)}: {reason}")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--results-dir", default=RESULTS_DIR)
    ap.add_argument("--json", help="also write the rollup here")
    args = ap.parse_args()

    records = load(args.results_dir)
    rows = rollup(records)
    render(rows)
    if args.json:
        with open(args.json, "w") as fh:
            json.dump(rows, fh, indent=2)
            fh.write("\n")
        print(f"\n-> {args.json}")
    return 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""
Parse ORFS reports for one nickname and merge into dataset/flow_instances.jsonl.

Usage:
  python3 scripts/extract_orfs_metrics.py --nickname openfloat_FP_add_32_1
"""

from __future__ import annotations

import argparse
import json
import os
import re
from glob import glob
from typing import Any, Dict, Optional

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))


def _read(path: str) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="ignore") as f:
            return f.read()
    except OSError:
        return ""


def find_report_dir(nickname: str) -> Optional[str]:
    patterns = [
        os.path.join(REPO_ROOT, "openroad_results", "reports", "asap7", nickname, "*"),
        os.path.join(REPO_ROOT, "openroad_results", "asap7", nickname, "*"),
        os.path.join(REPO_ROOT, "openroad_results", "**", nickname, "**"),
    ]
    hits = []
    for p in patterns:
        hits.extend(glob(p, recursive=True))
    dirs = [h for h in hits if os.path.isdir(h)]
    if dirs:
        dirs.sort(key=lambda d: os.path.getmtime(d), reverse=True)
        return dirs[0]
    files = [h for h in hits if os.path.isfile(h)]
    if files:
        files.sort(key=lambda f: os.path.getmtime(f), reverse=True)
        return os.path.dirname(files[0])
    return None


def parse_timing(text: str) -> Dict[str, Any]:
    out: Dict[str, Any] = {}
    m = re.search(r"wns\s+(-?\d+\.?\d*)", text, re.IGNORECASE)
    if m:
        out["wns"] = float(m.group(1))
    m = re.search(r"tns\s+(-?\d+\.?\d*)", text, re.IGNORECASE)
    if m:
        out["tns"] = float(m.group(1))
    m = re.search(r"worst\s+slack\s+[:=]\s*(-?\d+\.?\d*)", text, re.IGNORECASE)
    if m and "wns" not in out:
        out["wns"] = float(m.group(1))
    return out


def parse_util(text: str) -> Optional[float]:
    m = re.search(r"utilization\s*[:=]\s*(\d+\.?\d*)\s*%", text, re.IGNORECASE)
    if m:
        return float(m.group(1))
    return None


def infer_status(report_dir: Optional[str], logs_text: str) -> Dict[str, Any]:
    if report_dir is None:
        return {
            "status": "fail",
            "failure_stage": "implementation",
            "failure_message": "No ORFS report directory found under openroad_results/",
        }
    final_ok = any(
        os.path.exists(os.path.join(report_dir, name))
        or glob(os.path.join(os.path.dirname(report_dir), "**", name), recursive=True)
        for name in ("6_final.gds", "6_final.odb", "6_report.json")
    )
    if re.search(r"error|failed", logs_text, re.IGNORECASE) and not final_ok:
        stage = "unknown"
        if re.search(r"synth", logs_text, re.IGNORECASE):
            stage = "synthesis"
        if re.search(r"detailed.?route|grt|route", logs_text, re.IGNORECASE):
            stage = "routing"
        if re.search(r"place", logs_text, re.IGNORECASE):
            stage = "placement"
        return {"status": "fail", "failure_stage": stage, "failure_message": "ORFS log contains error/failed"}
    if final_ok:
        return {"status": "pass", "failure_stage": None, "failure_message": None}
    return {"status": "unknown", "failure_stage": None, "failure_message": "Reports present but final GDS/ODB not found"}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--nickname", required=True)
    ap.add_argument("--jsonl", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.jsonl"))
    args = ap.parse_args()

    meta_path = os.path.join(REPO_ROOT, "orfs_designs", "asap7", args.nickname, "design_meta.json")
    meta = {}
    if os.path.exists(meta_path):
        meta = json.load(open(meta_path, encoding="utf-8"))
    design = meta.get("design_id")
    period = meta.get("clock_period_ps")

    report_dir = find_report_dir(args.nickname)
    blob = ""
    if report_dir:
        for dirpath, _, files in os.walk(os.path.join(REPO_ROOT, "openroad_results")):
            for fn in files:
                if args.nickname in dirpath.replace("\\", "/") and fn.endswith((".rpt", ".log", ".txt", ".json")):
                    blob += _read(os.path.join(dirpath, fn)) + "\n"

    timing = parse_timing(blob)
    util = parse_util(blob)
    impl_status = infer_status(report_dir, blob)
    fmax = None
    if period and timing.get("wns") is not None and period > 0:
        # ASAP7 SDC period is picoseconds. Achieved period ~= period - wns (same units).
        achieved = period - timing["wns"]
        if achieved > 0:
            fmax = 1e6 / achieved  # ps -> MHz

    rel_dir = os.path.relpath(report_dir, REPO_ROOT).replace("\\", "/") if report_dir else None
    impl = {
        "status": impl_status["status"],
        "backend": "openroad-asap7",
        "platform": "asap7",
        "clock_period_ps": period,
        "wns": timing.get("wns"),
        "tns": timing.get("tns"),
        "fmax_mhz": fmax,
        "util_percent": util,
        "report_dir": rel_dir,
        "failure_stage": impl_status["failure_stage"],
        "failure_message": impl_status["failure_message"],
    }
    print(json.dumps(impl, indent=2))

    if not os.path.exists(args.jsonl) or not design:
        print("No flow_instances.jsonl merge (missing jsonl or design_id).")
        return 0

    rows = []
    updated = 0
    with open(args.jsonl, "r", encoding="utf-8") as f:
        for line in f:
            if not line.strip():
                continue
            rec = json.loads(line)
            if rec.get("design_id") == design:
                rec["implementation"] = impl
                if impl["status"] == "fail":
                    rec["overall_status"] = f"fail:{impl.get('failure_stage') or 'implementation'}"
                elif impl["status"] == "pass":
                    rec["overall_status"] = "pass:implementation"
                updated += 1
            rows.append(rec)
    with open(args.jsonl, "w", encoding="utf-8") as f:
        for rec in rows:
            f.write(json.dumps(rec) + "\n")
    json_path = os.path.splitext(args.jsonl)[0] + ".json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(rows, f, indent=2)
    print(f"Updated {updated} record(s) in {args.jsonl}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

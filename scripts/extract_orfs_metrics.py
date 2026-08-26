#!/usr/bin/env python3
"""
Parse one ORFS ASAP7 run and merge into dataset/flow_instances.jsonl.

Sources (do not grep the concatenated log blob):
  finish report_design_area  -> area_um2, util_percent   (6_report.log / 6_finish.rpt)
  finish report_worst_slack  -> wns / setup_slack_ps     (6_finish.rpt; not "wns max 0.00")
  finish report_tns          -> tns                      ("tns max", not repair_tns 100)
  finish report_clock_min_period -> achieved_period_ps, fmax_mhz
  FLW-0007/0008/0009         -> target / GRT period / GRT slack (5_1_grt.log)
  last DRT-0199              -> route_drc                (5_2_route.log)

Usage:
  python3 scripts/extract_orfs_metrics.py --nickname openfloat_FP_add_32_1
  python3 scripts/extract_orfs_metrics.py --nickname openfloat_FP_add_32_1 \\
      --nickname hardfloat_FPADD_8_24 --nickname rial_RialAddFP32
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import os
import re
import shutil
from glob import glob
from typing import Any, Dict, List, Optional

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))

_NUM = r"(-?\d+\.?\d*(?:[eE][+-]?\d+)?)"
_DESIGN_AREA_RE = re.compile(
    r"Design area\s+" + _NUM + r"\s*um\^2\s+" + _NUM + r"%\s+utilization",
    re.IGNORECASE,
)
_FLW7_RE = re.compile(r"\[INFO FLW-0007\]\s+clock\s+\S+\s+period\s+" + _NUM, re.IGNORECASE)
_FLW8_RE = re.compile(r"\[INFO FLW-0008\]\s+Clock\s+\S+\s+period\s+" + _NUM, re.IGNORECASE)
_FLW9_RE = re.compile(r"\[INFO FLW-0009\]\s+Clock\s+\S+\s+slack\s+" + _NUM, re.IGNORECASE)
_TNS_MAX_RE = re.compile(r"^tns max\s+" + _NUM + r"\s*$", re.MULTILINE | re.IGNORECASE)
_WORST_SLACK_RE = re.compile(r"worst slack max\s+" + _NUM, re.IGNORECASE)
_PERIOD_MIN_RE = re.compile(
    r"period_min\s*=\s*" + _NUM + r"\s+fmax\s*=\s*" + _NUM, re.IGNORECASE
)
_DRT_RE = re.compile(r"\[INFO DRT-0199\]\s+Number of violations\s*=\s*(\d+)", re.IGNORECASE)


def _read(path: str) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="ignore") as f:
            return f.read()
    except OSError:
        return ""


def _last_float(text: str, regex: re.Pattern[str], group: int = 1) -> Optional[float]:
    matches = list(regex.finditer(text))
    if not matches:
        return None
    return float(matches[-1].group(group))


def _last_int(text: str, regex: re.Pattern[str], group: int = 1) -> Optional[int]:
    matches = list(regex.finditer(text))
    if not matches:
        return None
    return int(matches[-1].group(group))


def _section(text: str, heading: str) -> str:
    """Return text from a 'finish <heading>' banner until the next finish banner."""
    start = re.search(
        r"^={10,}\s*\nfinish " + re.escape(heading) + r"\s*$",
        text,
        re.MULTILINE | re.IGNORECASE,
    )
    if not start:
        return ""
    rest = text[start.end() :]
    nxt = re.search(r"^={10,}\s*\nfinish ", rest, re.MULTILINE | re.IGNORECASE)
    return rest[: nxt.start()] if nxt else rest


def orfs_dirs(nickname: str) -> Dict[str, str]:
    root = os.path.join(REPO_ROOT, "openroad_results")
    return {
        "logs": os.path.join(root, "logs", "asap7", nickname, "base"),
        "reports": os.path.join(root, "reports", "asap7", nickname, "base"),
        "results": os.path.join(root, "results", "asap7", nickname, "base"),
    }


def find_report_dir(nickname: str) -> Optional[str]:
    reports = orfs_dirs(nickname)["reports"]
    if os.path.isdir(reports):
        return reports
    patterns = [
        os.path.join(REPO_ROOT, "openroad_results", "reports", "asap7", nickname, "*"),
        os.path.join(REPO_ROOT, "openroad_results", "**", nickname, "**"),
    ]
    hits: List[str] = []
    for p in patterns:
        hits.extend(glob(p, recursive=True))
    dirs = [h for h in hits if os.path.isdir(h)]
    if dirs:
        dirs.sort(key=lambda d: os.path.getmtime(d), reverse=True)
        return dirs[0]
    return None


def load_metrics_json(path: str) -> Dict[str, Any]:
    """ORFS 6_report.json repeats some keys; last occurrence is the finish stdcell area."""
    text = _read(path)
    if not text.strip():
        return {}
    kv: Dict[str, Any] = {}
    for m in re.finditer(
        r'"([^"]+)":\s*(null|' + _NUM + r")",
        text,
    ):
        raw = m.group(2)
        kv[m.group(1)] = None if raw == "null" else float(raw)
    return kv


def parse_finish_reports(logs_dir: str, reports_dir: str) -> Dict[str, Any]:
    out: Dict[str, Any] = {}
    finish_rpt = _read(os.path.join(reports_dir, "6_finish.rpt"))
    report_log = _read(os.path.join(logs_dir, "6_report.log"))
    metrics_json = load_metrics_json(os.path.join(logs_dir, "6_report.json"))

    area_src = _section(finish_rpt, "report_design_area") or report_log
    area_m = list(_DESIGN_AREA_RE.finditer(area_src))
    if not area_m:
        area_m = list(_DESIGN_AREA_RE.finditer(report_log))
    if area_m:
        out["area_um2"] = float(area_m[-1].group(1))
        out["util_percent"] = float(area_m[-1].group(2))
    elif metrics_json.get("finish__design__instance__area") is not None:
        out["area_um2"] = metrics_json["finish__design__instance__area"]
    if "util_percent" not in out and metrics_json.get("finish__design__instance__utilization") is not None:
        out["util_percent"] = metrics_json["finish__design__instance__utilization"] * 100.0

    slack_sec = _section(finish_rpt, "report_worst_slack")
    slack = _last_float(slack_sec, _WORST_SLACK_RE)
    if slack is None:
        slack = metrics_json.get("finish__timing__setup__ws")
    if slack is not None:
        out["setup_slack_ps"] = slack
        out["wns"] = slack

    tns_sec = _section(finish_rpt, "report_tns")
    tns = _last_float(tns_sec, _TNS_MAX_RE)
    if tns is None:
        tns = metrics_json.get("finish__timing__setup__tns")
    if tns is not None:
        out["tns"] = tns

    hold_ws = metrics_json.get("finish__timing__hold__ws")
    if hold_ws is not None:
        out["hold_slack_ps"] = hold_ws

    period_sec = _section(finish_rpt, "report_clock_min_period")
    pm = _PERIOD_MIN_RE.search(period_sec) or _PERIOD_MIN_RE.search(finish_rpt)
    if pm:
        out["achieved_period_ps"] = float(pm.group(1))
        out["fmax_mhz"] = float(pm.group(2))
    else:
        json_fmax = metrics_json.get("finish__timing__fmax")
        if json_fmax is not None and json_fmax > 0:
            # ORFS JSON stores Hz.
            out["fmax_mhz"] = json_fmax / 1e6 if json_fmax > 1e4 else json_fmax
            if out["fmax_mhz"] > 0:
                out["achieved_period_ps"] = 1e6 / out["fmax_mhz"]

    return out


def parse_grt_flw(logs_dir: str) -> Dict[str, Any]:
    text = _read(os.path.join(logs_dir, "5_1_grt.log"))
    out: Dict[str, Any] = {}
    t = _last_float(text, _FLW7_RE)
    if t is not None:
        out["clock_period_ps"] = t
    p = _last_float(text, _FLW8_RE)
    if p is not None:
        out["grt_period_ps"] = p
    s = _last_float(text, _FLW9_RE)
    if s is not None:
        out["grt_slack_ps"] = s
    return out


def parse_route_drc(logs_dir: str, reports_dir: str) -> Optional[int]:
    last = _last_int(_read(os.path.join(logs_dir, "5_2_route.log")), _DRT_RE)
    if last is not None:
        return last
    drc_rpt = os.path.join(reports_dir, "5_route_drc.rpt")
    if os.path.isfile(drc_rpt) and os.path.getsize(drc_rpt) == 0:
        return 0
    return None


def parse_clock_period_file(results_dir: str) -> Optional[float]:
    text = _read(os.path.join(results_dir, "clock_period.txt")).strip()
    if not text:
        return None
    try:
        return float(text.splitlines()[0].strip())
    except ValueError:
        return None


def infer_status(
    dirs: Dict[str, str],
    metrics: Dict[str, Any],
) -> Dict[str, Any]:
    results = dirs["results"]
    logs = dirs["logs"]
    gds = os.path.exists(os.path.join(results, "6_final.gds"))
    odb = os.path.exists(os.path.join(results, "6_final.odb"))
    report_json = os.path.exists(os.path.join(logs, "6_report.json"))
    final_ok = gds or odb or report_json

    if not os.path.isdir(logs) and not os.path.isdir(dirs["reports"]):
        return {
            "status": "fail",
            "failure_stage": "implementation",
            "failure_message": "No ORFS report directory found under openroad_results/",
        }

    drc = metrics.get("route_drc")
    slack = metrics.get("setup_slack_ps")
    if metrics.get("wns") is not None and slack is None:
        slack = metrics["wns"]

    if not final_ok:
        return {
            "status": "fail",
            "failure_stage": "implementation",
            "failure_message": "Reports present but final GDS/ODB not found",
        }
    if drc is not None and drc > 0:
        return {
            "status": "fail",
            "failure_stage": "routing",
            "failure_message": f"detailed route DRC {drc}",
        }
    if slack is not None and slack < 0:
        period = metrics.get("clock_period_ps")
        period_bit = f" at {period:g} ps target" if period else ""
        return {
            "status": "fail",
            "failure_stage": "timing",
            "failure_message": f"setup slack {slack:g} ps{period_bit}",
        }
    return {"status": "pass", "failure_stage": None, "failure_message": None}


def extract_nickname(nickname: str, meta: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
    meta = meta or {}
    dirs = orfs_dirs(nickname)
    report_dir = find_report_dir(nickname)
    finish = parse_finish_reports(dirs["logs"], dirs["reports"])
    grt = parse_grt_flw(dirs["logs"])
    drc = parse_route_drc(dirs["logs"], dirs["reports"])

    period = meta.get("clock_period_ps")
    if period is None:
        period = parse_clock_period_file(dirs["results"])
    if period is None:
        period = grt.get("clock_period_ps")

    metrics: Dict[str, Any] = {}
    metrics.update(finish)
    if drc is not None:
        metrics["route_drc"] = drc
    metrics["clock_period_ps"] = period
    if "grt_period_ps" in grt:
        metrics["grt_period_ps"] = grt["grt_period_ps"]
    if "grt_slack_ps" in grt:
        metrics["grt_slack_ps"] = grt["grt_slack_ps"]

    fmax = metrics.get("fmax_mhz")
    if fmax is None and period and metrics.get("wns") is not None and period > 0:
        achieved = period - metrics["wns"]
        if achieved > 0:
            metrics["achieved_period_ps"] = achieved
            metrics["fmax_mhz"] = 1e6 / achieved

    impl_status = infer_status(dirs, metrics)
    rel_dir = os.path.relpath(report_dir, REPO_ROOT).replace("\\", "/") if report_dir else None
    return {
        "status": impl_status["status"],
        "backend": "openroad-asap7",
        "platform": meta.get("platform") or "asap7",
        "clock_period_ps": period,
        "area_um2": metrics.get("area_um2"),
        "wns": metrics.get("wns"),
        "tns": metrics.get("tns"),
        "setup_slack_ps": metrics.get("setup_slack_ps"),
        "hold_slack_ps": metrics.get("hold_slack_ps"),
        "grt_period_ps": metrics.get("grt_period_ps"),
        "grt_slack_ps": metrics.get("grt_slack_ps"),
        "achieved_period_ps": metrics.get("achieved_period_ps"),
        "fmax_mhz": metrics.get("fmax_mhz"),
        "util_percent": metrics.get("util_percent"),
        "route_drc": metrics.get("route_drc"),
        "report_dir": rel_dir,
        "nickname": nickname,
        "failure_stage": impl_status["failure_stage"],
        "failure_message": impl_status["failure_message"],
    }


def load_meta(nickname: str) -> Dict[str, Any]:
    meta_path = os.path.join(REPO_ROOT, "orfs_designs", "asap7", nickname, "design_meta.json")
    if os.path.exists(meta_path):
        with open(meta_path, encoding="utf-8") as f:
            return json.load(f)
    return {}


def seed_jsonl(path: str) -> None:
    if os.path.exists(path):
        return
    saved = sorted(glob(os.path.join(REPO_ROOT, "saved_results", "*", "flow_instances.jsonl")))
    if not saved:
        return
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    shutil.copy2(saved[-1], path)
    print(f"Seeded {path} from {os.path.relpath(saved[-1], REPO_ROOT)}")


def _overall_from_impl(impl: Dict[str, Any]) -> str:
    if impl.get("status") == "fail":
        return f"fail:{impl.get('failure_stage') or 'implementation'}"
    if impl.get("status") == "pass":
        return "pass:implementation"
    return "unknown"


def _new_flow_id(design_id: str, impl: Dict[str, Any]) -> str:
    raw = f"{design_id}|openroad-asap7|{impl.get('clock_period_ps')}|{impl.get('nickname')}"
    return hashlib.sha1(raw.encode("utf-8")).hexdigest()[:16]


def merge_impl(jsonl_path: str, design_id: str, impl: Dict[str, Any]) -> int:
    """Update the matching (design_id, period) row.

    A not_run Yosys placeholder is filled in for the first PnR of that design.
    Extra clock periods append a new flow instance so 2000 ps add rows are kept.
    """
    if not os.path.exists(jsonl_path) or not design_id:
        print("No flow_instances.jsonl merge (missing jsonl or design_id).")
        return 0
    rows: List[Dict[str, Any]] = []
    with open(jsonl_path, "r", encoding="utf-8") as f:
        for line in f:
            if not line.strip():
                continue
            rows.append(json.loads(line))

    period = impl.get("clock_period_ps")
    exact: Optional[int] = None
    placeholder: Optional[int] = None
    template: Optional[Dict[str, Any]] = None
    for i, rec in enumerate(rows):
        if rec.get("design_id") != design_id:
            continue
        if template is None:
            template = rec
        rec_impl = rec.get("implementation") or {}
        rec_period = rec_impl.get("clock_period_ps")
        rec_nick = rec_impl.get("nickname")
        if rec_period == period and (rec_nick in (None, impl.get("nickname")) or rec_nick == impl.get("nickname")):
            exact = i
            break
        if rec_period == period:
            exact = i
            break
        if rec_impl.get("status") == "not_run" and rec_period is None and placeholder is None:
            placeholder = i

    changed = 0
    if exact is not None:
        rows[exact]["implementation"] = impl
        rows[exact]["overall_status"] = _overall_from_impl(impl)
        changed = 1
        action = "Updated"
    elif placeholder is not None:
        rows[placeholder]["implementation"] = impl
        rows[placeholder]["overall_status"] = _overall_from_impl(impl)
        changed = 1
        action = "Updated"
    elif template is not None:
        rec = copy.deepcopy(template)
        rec["flow_instance_id"] = _new_flow_id(design_id, impl)
        rec["implementation"] = impl
        rec["overall_status"] = _overall_from_impl(impl)
        rows.append(rec)
        changed = 1
        action = "Appended"
    else:
        print(f"No flow_instances.jsonl merge (no row for {design_id}).")
        return 0

    with open(jsonl_path, "w", encoding="utf-8") as f:
        for rec in rows:
            f.write(json.dumps(rec) + "\n")
    json_path = os.path.splitext(jsonl_path)[0] + ".json"
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(rows, f, indent=2)
    print(f"{action} {changed} record(s) in {jsonl_path}")
    return changed


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--nickname", dest="nicknames", action="append", required=True)
    ap.add_argument("--jsonl", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.jsonl"))
    ap.add_argument(
        "--also-jsonl",
        action="append",
        default=[],
        help="Extra JSONL paths to merge (e.g. saved_results archive).",
    )
    args = ap.parse_args()

    seed_jsonl(args.jsonl)
    rc = 0
    for nickname in args.nicknames:
        meta = load_meta(nickname)
        impl = extract_nickname(nickname, meta)
        print(json.dumps(impl, indent=2))
        design = meta.get("design_id")
        if not design:
            print(f"No design_id in orfs_designs/asap7/{nickname}/design_meta.json")
            rc = 1
            continue
        merge_impl(args.jsonl, design, impl)
        for extra in args.also_jsonl:
            if os.path.exists(extra):
                merge_impl(extra, design, impl)
    return rc


if __name__ == "__main__":
    raise SystemExit(main())

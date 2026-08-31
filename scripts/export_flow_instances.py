#!/usr/bin/env python3
"""
Build flow-instance records from Yosys XML + generated RTL + optional verify overlay.

Usage (from repo root, after run_ppa_estimation.sh):
  python3 scripts/export_flow_instances.py
  python3 scripts/export_flow_instances.py --xml generated/cell_count_report.xml \\
      --out dataset/flow_instances.jsonl
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import subprocess
import sys
import xml.etree.ElementTree as ET
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.abspath(os.path.join(SCRIPT_DIR, ".."))
sys.path.insert(0, SCRIPT_DIR)

from fp_design_meta import design_id, parse_generated_sv  # noqa: E402


def _run(cmd: List[str]) -> str:
    try:
        p = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True, timeout=20)
        return (p.stdout or p.stderr or "").strip()
    except Exception:
        return ""


def _first_line(cmd: List[str]) -> str:
    text = _run(cmd)
    return text.splitlines()[0] if text else ""


def collect_environment() -> Dict[str, str]:
    env = {
        "host": platform.node(),
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "java": "",
        "sbt": "",
        "verilator": _first_line(["verilator", "--version"]),
        "yosys": _first_line(["yosys", "-V"]),
        "openroad": _first_line(["openroad", "-version"]) or _first_line(["openroad", "--version"]),
    }
    try:
        java = subprocess.run(["java", "-version"], cwd=REPO_ROOT, capture_output=True, text=True, timeout=20)
        java_line = ((java.stderr or java.stdout) or "").splitlines()
        env["java"] = java_line[0] if java_line else ""
    except Exception:
        pass
    sbt = _run(["sbt", "--version"])
    for line in sbt.splitlines():
        if "sbt script version" in line.lower() or line.lower().startswith("sbt"):
            env["sbt"] = line.strip()
            break
    return {k: v for k, v in env.items() if v}


def git_submodule_commits() -> Dict[str, str]:
    commits = {}
    head = _run(["git", "rev-parse", "HEAD"])
    if head:
        commits["openhwfp-eval"] = head
    for name in ("OpenFloat", "berkeley-hardfloat", "rial-tmpfix"):
        p = subprocess.run(
            ["git", "-C", os.path.join(REPO_ROOT, name), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            timeout=20,
        )
        if p.returncode == 0:
            commits[name] = p.stdout.strip()
    return commits


def load_verify_rules(path: str) -> List[Dict[str, Any]]:
    if not os.path.exists(path):
        return []
    with open(path, "r", encoding="utf-8") as f:
        obj = json.load(f)
    return obj.get("rules", [])


def apply_verify_rules(meta: Dict[str, Any], rules: List[Dict[str, Any]]) -> Dict[str, Any]:
    for rule in rules:
        match = rule.get("match") or {}
        ok = True
        for k, v in match.items():
            if str(meta.get(k, "")).lower() != str(v).lower():
                ok = False
                break
        if ok:
            return {
                "status": rule.get("verification_status", "unknown"),
                "failure_stage": rule.get("failure_stage"),
                "failure_category": rule.get("failure_category"),
                "failure_message": rule.get("failure_message"),
            }
    return {"status": "unknown", "failure_stage": None, "failure_category": None, "failure_message": None}


def parse_xml_modules(xml_path: str) -> Dict[str, Dict[str, Any]]:
    """Index Yosys XML rows by stem / prettified name / source path."""
    out: Dict[str, Dict[str, Any]] = {}
    if not os.path.exists(xml_path):
        return out
    tree = ET.parse(xml_path)
    root = tree.getroot()
    modules = root.find("Modules")
    if modules is None:
        return out
    for module in modules.findall("Module"):
        name = (module.findtext("Name") or "").strip()
        cells = int(module.findtext("Cells") or "0")
        wire_bits = int(module.findtext("WireBits") or "0")
        area = float(module.findtext("Area_nm2") or "0")
        source = (module.findtext("SourcePath") or "").strip()
        rec = {"xml_name": name, "cell_count": cells, "wire_bits": wire_bits, "estimated_area_nm2": area, "source_path": source}
        keys = [name]
        if source:
            keys.append(source.replace("\\", "/"))
            keys.append(os.path.splitext(os.path.basename(source))[0])
        keys.append(os.path.basename(name.replace("\\", "/")))
        stem = os.path.splitext(os.path.basename(name.replace("\\", "/")))[0]
        keys.append(stem)
        for k in keys:
            if k:
                out[k] = rec
    return out


def xml_lookup(xml_index: Dict[str, Dict[str, Any]], meta: Dict[str, Any]) -> Optional[Dict[str, Any]]:
    path = meta.get("source_path") or ""
    stem = meta.get("stem") or ""
    for key in (path, os.path.splitext(path)[0], stem, meta.get("module_filename")):
        if key and key in xml_index:
            return xml_index[key]
    # Fallback: unique stem suffix match
    hits = [v for k, v in xml_index.items() if k.endswith(stem) or k.endswith(stem + ".sv")]
    if len(hits) == 1:
        return hits[0]
    return None


def overall_status(verif: Dict[str, Any], synth: Dict[str, Any], impl: Dict[str, Any]) -> str:
    """Deepest gate reached decides. Failures outrank passes.

    Must stay vocabulary-compatible with extract_orfs_metrics._overall_from_impl:
    a routed run reports pass:implementation or fail:<failure_stage> (e.g.
    fail:timing), never the synthesis-level status it would otherwise fall back to.
    Without the implementation branches below, re-exporting silently downgrades
    every routed record.
    """
    impl_st = impl.get("status")
    if impl_st == "fail":
        return f"fail:{impl.get('failure_stage') or 'implementation'}"
    if impl_st == "ignored":
        return "ignored:implementation"
    for stage, rec in (("synthesis", synth), ("verification", verif)):
        st = rec.get("status")
        if st == "fail":
            return f"fail:{stage}"
        if st == "ignored":
            return f"ignored:{stage}"
    if impl_st == "pass":
        return "pass:implementation"
    if synth.get("status") == "pass":
        return "pass:synthesis"
    if verif.get("status") == "pass":
        return "pass:verification"
    return "unknown"


def flow_instance_id(design: str, backend: str, period: Any = None, nickname: Any = None) -> str:
    """Stable id for one flow instance.

    Same shape as extract_orfs_metrics._new_flow_id, so both writers agree:
        sha1(design_id|backend|clock_period_ps|nickname)[:16]

    This previously hashed the run timestamp instead of period/nickname, which was
    wrong twice over: the id changed on every export (so records could not be
    matched across runs), and within one export the timestamp was constant, so two
    rows sharing a design_id collided. Existing ids are preserved by merge_records;
    only genuinely new designs are numbered under this scheme.
    """
    raw = f"{design}|{backend}|{period}|{nickname}"
    return hashlib.sha1(raw.encode("utf-8")).hexdigest()[:16]


def build_records(args: argparse.Namespace) -> List[Dict[str, Any]]:
    xml_index = parse_xml_modules(args.xml)
    rules = load_verify_rules(args.verify)
    env = collect_environment()
    commits = git_submodule_commits()

    sv_root = args.generated
    files: List[str] = []
    if os.path.isdir(sv_root):
        for dirpath, _, filenames in os.walk(sv_root):
            for fn in filenames:
                if fn.endswith(".sv"):
                    files.append(os.path.relpath(os.path.join(dirpath, fn), REPO_ROOT).replace("\\", "/"))
    files.sort()

    if not files and xml_index:
        # XML-only fallback (generated/ already cleaned)
        for rec in {id(v): v for v in xml_index.values()}.values():
            name = rec.get("xml_name") or rec.get("source_path") or "unknown"
            files.append(name)

    records = []
    seen = set()
    for path in files:
        if path in seen:
            continue
        seen.add(path)
        if path.endswith(".sv") and os.path.exists(os.path.join(REPO_ROOT, path)):
            meta = parse_generated_sv(path)
        else:
            meta = parse_generated_sv(path)
            meta["source_path"] = path
        did = design_id(meta)
        xml_row = xml_lookup(xml_index, meta)
        cells = xml_row["cell_count"] if xml_row else None
        synth_status = "pass" if cells and cells > 0 else ("fail" if xml_row is not None else "unknown")
        verif = apply_verify_rules(meta, rules)
        impl = {
            "status": "not_run",
            "backend": "openroad-asap7",
            "platform": "asap7",
            "clock_period_ps": None,
            "area_um2": None,
            "wns": None,
            "tns": None,
            "setup_slack_ps": None,
            "hold_slack_ps": None,
            "grt_period_ps": None,
            "grt_slack_ps": None,
            "achieved_period_ps": None,
            "fmax_mhz": None,
            "util_percent": None,
            "route_drc": None,
            "report_dir": None,
            "nickname": None,
            "failure_stage": None,
            "failure_message": None,
        }
        synth = {
            "status": synth_status,
            "backend": "yosys-generic",
            "cell_count": cells,
            "wire_bits": xml_row["wire_bits"] if xml_row else None,
            "estimated_area_nm2": xml_row["estimated_area_nm2"] if xml_row else None,
            "area_note": "Generic Yosys cell count times AREA_PER_CELL=100 nm^2 heuristic; not a PDK area.",
        }
        rec = {
            "flow_instance_id": flow_instance_id(did, "yosys-generic"),
            "design_id": did,
            "library": meta.get("library"),
            "operator": meta.get("operator"),
            "precision": meta.get("precision"),
            "exponent_width": meta.get("exponent_width"),
            "mantissa_width": meta.get("mantissa_width"),
            "bitwidth": meta.get("bitwidth"),
            "pipeline_depth": meta.get("pipeline_depth"),
            "source_path": meta.get("source_path"),
            "source_commit": commits,
            "environment": env,
            "verification": verif,
            "synthesis": synth,
            "implementation": impl,
            "overall_status": overall_status(verif, synth, impl),
        }
        records.append(rec)
    return records


# Regenerated from the RTL + Yosys XML on every export: safe to refresh in place.
DESIGN_LEVEL_FIELDS = (
    "library", "operator", "precision", "exponent_width", "mantissa_width",
    "bitwidth", "pipeline_depth", "source_path", "source_commit",
    "environment", "verification", "synthesis",
)


def load_existing(path: str) -> List[Dict[str, Any]]:
    rows: List[Dict[str, Any]] = []
    with open(path, "r", encoding="utf-8") as f:
        for lineno, line in enumerate(f, 1):
            if not line.strip():
                continue
            try:
                rows.append(json.loads(line))
            except json.JSONDecodeError as exc:
                raise SystemExit(f"ERROR: {path}:{lineno}: malformed JSON: {exc}")
    return rows


def merge_records(fresh: List[Dict[str, Any]], path: str) -> tuple:
    """Refresh design-level fields on existing rows without discarding evidence.

    synthesis/verification/environment describe the design and are regenerated.
    implementation describes one physical OpenROAD run: it is evidence, and it is
    preserved along with the flow_instance_id that cites it. Rows for designs that
    are no longer in the export are kept, not dropped.
    """
    rows = load_existing(path)
    by_design: Dict[Any, List[Dict[str, Any]]] = {}
    for rec in rows:
        by_design.setdefault(rec.get("design_id"), []).append(rec)

    stats = {"refreshed": 0, "preserved": 0, "added": 0, "kept": 0}
    matched = set()
    for new in fresh:
        did = new.get("design_id")
        targets = by_design.get(did)
        if not targets:
            rows.append(new)
            stats["added"] += 1
            continue
        matched.add(did)
        for rec in targets:
            for field in DESIGN_LEVEL_FIELDS:
                if field in new:
                    rec[field] = new[field]
            impl = rec.get("implementation") or {}
            if impl.get("status") in (None, "", "not_run"):
                rec["implementation"] = new["implementation"]
            else:
                stats["preserved"] += 1
            rec["overall_status"] = overall_status(
                rec.get("verification") or {},
                rec.get("synthesis") or {},
                rec.get("implementation") or {},
            )
            stats["refreshed"] += 1
    stats["kept"] = sum(len(v) for d, v in by_design.items() if d not in matched)
    return rows, stats


def main() -> int:
    ap = argparse.ArgumentParser(description="Export Yosys/verification results as flow-instance JSONL")
    ap.add_argument("--xml", default=os.path.join(REPO_ROOT, "generated", "cell_count_report.xml"))
    ap.add_argument("--generated", default=os.path.join(REPO_ROOT, "generated"))
    ap.add_argument("--verify", default=os.path.join(REPO_ROOT, "dataset", "verify_status.json"))
    ap.add_argument("--out", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.jsonl"))
    ap.add_argument("--json", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.json"))
    ap.add_argument(
        "--overwrite",
        action="store_true",
        help="DESTRUCTIVE: replace the output instead of merging into it. Discards "
             "every OpenROAD implementation result already recorded there.",
    )
    args = ap.parse_args()

    os.chdir(REPO_ROOT)
    records = build_records(args)
    if not records:
        print("ERROR: no records. Run bash scripts/run_ppa_estimation.sh first, or pass --xml.", file=sys.stderr)
        return 1

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    existed = os.path.exists(args.out)
    if args.overwrite and existed:
        dropped = sum(
            1 for r in load_existing(args.out)
            if (r.get("implementation") or {}).get("status") not in (None, "", "not_run")
        )
        print(
            f"WARNING: --overwrite is discarding {args.out} "
            f"({dropped} implementation record(s) will be lost).",
            file=sys.stderr,
        )
        final = records
    elif existed:
        final, stats = merge_records(records, args.out)
        print(
            f"Merged into {args.out}: {stats['refreshed']} refreshed, "
            f"{stats['preserved']} implementation block(s) preserved, "
            f"{stats['added']} added, {stats['kept']} untouched."
        )
    else:
        final = records

    with open(args.out, "w", encoding="utf-8") as f:
        for rec in final:
            f.write(json.dumps(rec) + "\n")
    with open(args.json, "w", encoding="utf-8") as f:
        json.dump(final, f, indent=2)

    n = len(final)
    synth_ok = sum(1 for r in final if (r.get("synthesis") or {}).get("status") == "pass")
    v_pass = sum(1 for r in final if (r.get("verification") or {}).get("status") == "pass")
    v_fail = sum(1 for r in final if (r.get("verification") or {}).get("status") == "fail")
    print(f"Wrote {n} flow instances -> {args.out}")
    print(f"  synthesis pass: {synth_ok}/{n}")
    print(f"  verification pass/fail/other: {v_pass}/{v_fail}/{n - v_pass - v_fail}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""
Build flow-instance records from Yosys XML + generated RTL + verification records (verification_results/).

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


def _submodule_paths() -> List[str]:
    out = _run(["git", "config", "--file", os.path.join(REPO_ROOT, ".gitmodules"),
                "--get-regexp", r"^submodule\..*\.path$"])
    paths = [line.split(" ", 1)[1] for line in out.splitlines() if " " in line] if out else []
    return sorted(paths)


def git_submodule_commits() -> Dict[str, str]:
    commits = {}
    head = _run(["git", "rev-parse", "HEAD"])
    if head:
        commits["openhwfp-eval"] = head
    for name in _submodule_paths():
        p = subprocess.run(
            ["git", "-C", os.path.join(REPO_ROOT, name), "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            timeout=20,
        )
        if p.returncode == 0:
            commits[name] = p.stdout.strip()
    return commits


# Fields already carried at the flow-instance top level or unique to one run's environment.
_DROP_FROM_RUN = {"design_id", "library", "operator", "precision", "descriptor_path", "environment"}
NOT_RUN = {"status": "not_run", "failure_stage": None, "failure_category": None, "failure_message": None}


def _failure_synthesis(status: str, runs: List[Dict[str, Any]]) -> Dict[str, Any]:
    if status == "pass":
        return {"failure_stage": None, "failure_category": None, "failure_message": None}
    if status == "aborted":
        reasons = [r.get("abort_reason") for r in runs if r.get("abort_reason")]
        return {"failure_stage": "simulation", "failure_category": "dut_abort",
                "failure_message": reasons[0] if reasons else "DUT aborted the simulation"}
    worst = max(runs, key=lambda r: r.get("mismatch_count", 0))
    cats = worst.get("mismatch_categories") or []
    total = sum(r.get("mismatch_count", 0) for r in runs)
    max_ulp = max((r.get("max_ulp") or 0) for r in runs)
    return {"failure_stage": "simulation",
            "failure_category": cats[0]["kind"] if cats else "ulp_exceeded",
            "failure_message": f"{total} mismatches; max_ulp {max_ulp:.3g}; "
                               f"budget {(worst.get('reference') or {}).get('ulp_budget')}"}


def _verification_block(runs: List[Dict[str, Any]]) -> Dict[str, Any]:
    """One verbose per-design block, aggregated across runs."""
    status = ("aborted" if all(r["status"] == "aborted" for r in runs)
              else "fail" if any(r["status"] == "fail" for r in runs) else "pass")
    checks = sum(r.get("checks_performed", 0) for r in runs)
    mism = sum(r.get("mismatch_count", 0) for r in runs)
    ulps = [r["max_ulp"] for r in runs if r.get("max_ulp") is not None]
    wsum = sum((r.get("mean_ulp") or 0) * r.get("checks_performed", 0) for r in runs)
    cov: Dict[str, int] = {}
    for r in runs:
        for k, v in (r.get("special_case_coverage") or {}).items():
            cov[k] = cov.get(k, 0) + v
    first = runs[0]
    block = {
        "status": status,
        "tier": first.get("tier"),
        "conformance_level": first.get("conformance_level"),
        "reference": first.get("reference"),
        "reference_model": first.get("reference_model"),
        "profile": first.get("profile"),
        "flag_check": first.get("flag_check"),
        "checks_performed": checks,
        "vectors_excluded_by_profile": sum(r.get("vectors_excluded_by_profile", 0) for r in runs),
        "mismatch_count": mism,
        "fraction_within_ulp_budget": (checks - mism) / checks if checks else None,
        "max_ulp": max(ulps) if ulps else None,
        "mean_ulp": wsum / checks if checks else None,
        "special_case_coverage": dict(sorted(cov.items())),
        "canary_ok": all(r.get("canary_ok", False) for r in runs),
        "modes_covered": sorted({r["rounding_mode"] for r in runs if r.get("rounding_mode")}),
        "tininess_covered": sorted({r["tininess"] for r in runs if r.get("tininess")}),
        "runs": [{k: v for k, v in r.items() if k not in _DROP_FROM_RUN} for r in runs],
    }
    block.update(_failure_synthesis(status, runs))
    return block


def load_verification(results_dir: str) -> Dict[str, Dict[str, Any]]:
    """Group verification_results/*.json by design_id."""
    import glob
    by_design: Dict[str, List[Dict[str, Any]]] = {}
    for f in sorted(glob.glob(os.path.join(results_dir, "*.json"))):
        with open(f, "r", encoding="utf-8") as fh:
            r = json.load(fh)
        by_design.setdefault(r["design_id"], []).append(r)
    return {did: _verification_block(runs) for did, runs in by_design.items()}


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
        if st in ("fail", "aborted"):
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
    verification = load_verification(args.results_dir)
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
        # Single-design mode: build only the requested row and leave the rest of the dataset alone
        if getattr(args, "design", None) and did != args.design:
            continue
        xml_row = xml_lookup(xml_index, meta)
        cells = xml_row["cell_count"] if xml_row else None
        synth_status = "pass" if cells and cells > 0 else ("fail" if xml_row is not None else "unknown")
        verif = verification.get(did) or dict(NOT_RUN)
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


def upsert_one(out_path: str, json_path: str, rec: Dict[str, Any]) -> bool:
    """Update or insert (upsert!) a single design's row in the JSONL + JSON, keeping every other row.

    Returns True if an existing row was replaced. A prior implementation block persists because
    ppa/synthesis has no business clearing ORFS results that a later impl run patched in.
    """
    rows: List[Dict[str, Any]] = []
    if os.path.exists(out_path):
        with open(out_path, "r", encoding="utf-8") as f:
            for line in f:
                line = line.strip()
                if line:
                    rows.append(json.loads(line))
    did = rec["design_id"]
    replaced = False
    for i, r in enumerate(rows):
        if r.get("design_id") == did:
            existing_impl = r.get("implementation")
            if existing_impl and existing_impl.get("status") != "not_run":
                rec["implementation"] = existing_impl
                rec["overall_status"] = overall_status(rec["verification"], rec["synthesis"], existing_impl)
            rows[i] = rec
            replaced = True
            break
    if not replaced:
        rows.append(rec)
    os.makedirs(os.path.dirname(out_path) or ".", exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        for r in rows:
            f.write(json.dumps(r) + "\n")
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(rows, f, indent=2)
    return replaced


def main() -> int:
    ap = argparse.ArgumentParser(description="Export Yosys/verification results as flow-instance JSONL")
    ap.add_argument("--xml", default=os.path.join(REPO_ROOT, "generated", "cell_count_report.xml"))
    ap.add_argument("--generated", default=os.path.join(REPO_ROOT, "generated"))
    ap.add_argument("--results-dir", default=os.path.join(REPO_ROOT, "verification_results"),
                    help="verification_results/ directory; records join by design_id")
    ap.add_argument("--out", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.jsonl"))
    ap.add_argument("--json", default=os.path.join(REPO_ROOT, "dataset", "flow_instances.json"))
    ap.add_argument(
        "--overwrite",
        action="store_true",
        help="DESTRUCTIVE: replace the output instead of merging into it. Discards "
             "every OpenROAD implementation result already recorded there.",
    )
    ap.add_argument("--design", default=None,
                    help="library/stem: refresh only this row (upsert), keeping every other row and its impl block")
    args = ap.parse_args()

    os.chdir(REPO_ROOT)
    records = build_records(args)
    if not records:
        if args.design:
            print(f"ERROR: design {args.design} not found under {args.generated}", file=sys.stderr)
        else:
            print("ERROR: no records. Run bash scripts/run_ppa_estimation.sh first, or pass --xml.", file=sys.stderr)
        return 1

    if args.design:
        rec = records[0]
        replaced = upsert_one(args.out, args.json, rec)
        verb = "Updated" if replaced else "Added"
        print(f"{verb} flow instance for {args.design} -> {args.out}")
        print(f"  synthesis={rec['synthesis']['status']}  verification={rec['verification']['status']}  "
              f"overall={rec['overall_status']}")
        return 0

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
    counts: Dict[str, int] = {}
    for r in records:
        s = r["verification"]["status"]
        counts[s] = counts.get(s, 0) + 1
    print(f"Wrote {n} flow instances -> {args.out}")
    print(f"  synthesis pass: {synth_ok}/{n}")
    print("  verification: " + ", ".join(f"{k}={v}" for k, v in sorted(counts.items())))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

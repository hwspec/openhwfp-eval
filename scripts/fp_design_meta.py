#!/usr/bin/env python3
"""Design metadata for generated FP modules, read from the descriptor manifest.

Build the manifest first:
    python3 scripts/build_manifest.py
"""

from __future__ import annotations

import functools
import json
import os
import re
from typing import Any, Dict, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MANIFEST_PATH = os.path.join(ROOT, "generated", "descriptor_manifest.json")


@functools.lru_cache(maxsize=1)
def load_manifest(path: str = MANIFEST_PATH) -> Dict[str, Any]:
    if not os.path.exists(path):
        return {}
    with open(path) as fh:
        data = json.load(fh)

    return {d["design_id"]: d for d in data.get("designs", [])}


def _norm_path(path: str) -> str:
    return path.replace("\\", "/")


def library_from_path(path: str) -> str:
    parts = _norm_path(path).split("/")
    if "generated" in parts:
        i = parts.index("generated")
        if i + 1 < len(parts) - 1:
            return parts[i + 1]
    known = {d.split("/")[0] for d in load_manifest()}
    base = os.path.basename(path).lower()
    for lib in known:
        if base.startswith(lib):
            return lib
    return "unknown"


def parse_generated_sv(path: str) -> Dict[str, Any]:
    rel = _norm_path(path)
    stem = os.path.splitext(os.path.basename(path))[0]
    lib = library_from_path(path)
    did = f"{lib}/{stem}"

    meta: Dict[str, Any] = {
        "source_path": rel,
        "module_filename": os.path.basename(path),
        "stem": stem,
        "library": lib,
        "design_id": did,
        "operator": None,
        "precision": None,
        "exponent_width": None,
        "mantissa_width": None,
        "significand_width": None,
        "pipeline_depth": None,
        "bitwidth": None,
        "manifest_status": "missing",
    }

    entry = load_manifest().get(did)
    if entry is None:
        return meta

    derived = entry["derived"]
    meta.update(
        operator=entry["operator"],
        precision=derived["precision"],
        exponent_width=derived["exponent_width"],
        mantissa_width=derived["mantissa_width"],
        significand_width=derived["significand_width"],
        bitwidth=derived["bitwidth"],
        module_name=entry["module"],
        tier=entry["tier"],
        conformance_level=entry["conformance_level"],
        profile=entry["profile"],
        generator=entry["generator"],
        descriptor_path=entry["descriptor_path"],
        clock_port=entry["sim"].get("clock") or "clock",
        reset_port=entry["sim"].get("reset") or "reset",
        protocol=entry["sim"]["protocol"],
        manifest_status="ok",
    )

    # Pipeline depth is a per-library elaboration parameter, not a universal field.
    params = entry["generator"].get("params", {})
    for key in ("pd", "latency"):
        if key in params:
            meta["pipeline_depth"] = params[key]
            break
    else:
        meta["pipeline_depth"] = 0 if entry["sim"]["protocol"] == "combinational" else None

    return meta


def design_id(meta: Dict[str, Any]) -> str:
    if meta.get("design_id"):
        return meta["design_id"]
    lib = meta.get("library") or "unknown"
    stem = meta.get("stem") or os.path.splitext(os.path.basename(meta.get("source_path", "unknown")))[0]
    return f"{lib}/{stem}"


def detect_clock_reset(sv_text: str) -> Dict[str, Optional[str]]:
    clock = None
    reset = None
    for m in re.finditer(r"\binput\s+(?:wire\s+)?(?:logic\s+)?(\w+)\s*[,;\)]", sv_text):
        name = m.group(1)
        low = name.lower()
        if clock is None and low in ("clock", "clk", "clock_i", "clk_i"):
            clock = name
        if reset is None and low in ("reset", "rst", "reset_i", "rst_i", "rst_n", "resetn"):
            reset = name
    if clock is None and re.search(r"\bclock\b", sv_text):
        clock = "clock"
    return {"clock_port": clock, "reset_port": reset}


if __name__ == "__main__":
    import sys
    manifest = load_manifest()
    if not manifest:
        print(f"no manifest at {MANIFEST_PATH}; run scripts/build_manifest.py")
        sys.exit(1)
    targets = sys.argv[1:] or sorted(d["derived"]["source_path"] for d in manifest.values())
    missing = 0
    for t in targets:
        m = parse_generated_sv(t)
        flag = "" if m["manifest_status"] == "ok" else "  <-- MISSING"
        if flag:
            missing += 1
        print(f"{m['design_id']:34s} {str(m['operator']):14s} {str(m['precision']):6s} "
              f"e={str(m['exponent_width']):3s} m={str(m['mantissa_width']):3s} "
              f"pd={str(m['pipeline_depth']):5s}{flag}")
    print(f"\n{len(targets)} designs, {missing} missing from manifest")

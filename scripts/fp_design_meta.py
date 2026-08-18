#!/usr/bin/env python3
"""Parse generated FP module filenames into dataset metadata."""

from __future__ import annotations

import os
import re
from typing import Any, Dict, Optional

FP_MAP = {
    (5, 10): "fp16",
    (8, 23): "fp32",
    (8, 24): "fp32",
    (11, 52): "fp64",
    (11, 53): "fp64",
}

BITS_MAP = {16: "fp16", 32: "fp32", 64: "fp64"}

OPENFLOAT_OPS = {
    "add": "add",
    "mult": "mul",
    "divider": "div",
    "div": "div",
    "sqrt": "sqrt",
    "cos": "cos",
}

HARDFLOAT_OPS = {
    "ADD": "add",
    "SUB": "sub",
    "MUL": "mul",
    "DIV": "div",
    "SQRT": "sqrt",
    "TEST": "recfn_roundtrip",
}

RIAL_OPS = {
    "Add": "add",
    "Mult": "mul",
    "FusedMulAdd": "fma",
    "Sqrt": "sqrt",
    "InvSqrt": "invsqrt",
    "Sin": "sin",
    "Cos": "cos",
    "Reciprocal": "reciprocal",
    "Exp": "exp",
    "Log": "log",
    "Sigmoid": "sigmoid",
    "Acos": "acos",
    "SoftPlus": "softplus",
    "SMG": "smg",
    "Atan2": "atan2",
}


def _norm_path(path: str) -> str:
    return path.replace("\\", "/")


def library_from_path(path: str) -> str:
    p = _norm_path(path).lower()
    if "/openfloat/" in p or p.startswith("openfloat"):
        return "openfloat"
    if "/hardfloat/" in p or p.startswith("hardfloat"):
        return "hardfloat"
    if "/rial/" in p or p.startswith("rial"):
        return "rial"
    name = os.path.basename(path).lower()
    if name.startswith("rial"):
        return "rial"
    if name.startswith("fpadd") or name.startswith("fpsub") or name.startswith("fpmul") or name.startswith("fpdiv") or name.startswith("fpsqrt") or name.startswith("fptest"):
        return "hardfloat"
    if name.startswith("fp_"):
        return "openfloat"
    return "unknown"


def parse_generated_sv(path: str) -> Dict[str, Any]:
    """Return design metadata for a GenerateAllTestModules .sv path."""
    rel = _norm_path(path)
    base = os.path.splitext(os.path.basename(path))[0]
    lib = library_from_path(path)
    meta: Dict[str, Any] = {
        "source_path": rel,
        "module_filename": os.path.basename(path),
        "stem": base,
        "library": lib,
        "operator": None,
        "precision": None,
        "exponent_width": None,
        "mantissa_width": None,
        "pipeline_depth": None,
        "bitwidth": None,
    }

    of = re.match(r"FP_(add|mult|divider|div|sqrt|cos)_(\d+)(?:_(\d+))?(?:_(\d+))?$", base)
    if of:
        op, bits, a, b = of.group(1), int(of.group(2)), of.group(3), of.group(4)
        meta.update(
            library="openfloat",
            operator=OPENFLOAT_OPS.get(op, op),
            bitwidth=bits,
            precision=BITS_MAP.get(bits),
        )
        if op in ("add", "mult") and a is not None:
            meta["pipeline_depth"] = int(a)
        elif op in ("divider", "div", "sqrt") and a is not None:
            meta["pipeline_depth"] = int(a)
            if b is not None:
                meta["extra_latency"] = int(b)
        elif op == "cos" and a is not None:
            meta["pipeline_depth"] = int(a)
        return meta

    hf = re.match(r"FP(TEST|ADD|SUB|MUL|DIV|SQRT)_(\d+)_(\d+)$", base, re.IGNORECASE)
    if hf:
        op, e, m = hf.group(1).upper(), int(hf.group(2)), int(hf.group(3))
        meta.update(
            library="hardfloat",
            operator=HARDFLOAT_OPS.get(op, op.lower()),
            exponent_width=e,
            mantissa_width=m,
            precision=FP_MAP.get((e, m)),
            pipeline_depth=0,
        )
        return meta

    rial = re.match(r"Rial([A-Za-z]+)(FP(?:16|32|64))$", base)
    if rial:
        op_raw, fp = rial.group(1), rial.group(2).lower()
        meta.update(
            library="rial",
            operator=RIAL_OPS.get(op_raw, op_raw.lower()),
            precision=fp,
            pipeline_depth=0,
        )
        return meta

    return meta


def design_id(meta: Dict[str, Any]) -> str:
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

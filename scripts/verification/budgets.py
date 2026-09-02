"""Per-function, per-format tier-2 ULP budgets. Single source of truth for the numbers.

The human-readable rationale and citations live in SPECIFICATION.md; tests/test_budgets.py parses
that table and asserts it matches this dict, so the doc and the code cannot drift. default is the
fp32 (float) budget: fp16 and any untabulated format inherit it.
"""

from __future__ import annotations

# operator -> {format name: max ULP, "default": fp32 value, "source": citation}
BUDGETS = {
    "sqrt":       {"fp16": 0, "fp32": 0, "fp64": 0, "default": 0, "source": "IEEE-754 §5.4.1 / CUDA"},
    "reciprocal": {"fp16": 0, "fp32": 0, "fp64": 0, "default": 0, "source": "IEEE-754 §5.4.1 / CUDA"},
    "exp":        {"fp16": 2, "fp32": 2, "fp64": 1, "default": 2, "source": "CUDA"},
    "log":        {"fp16": 1, "fp32": 1, "fp64": 1, "default": 1, "source": "CUDA"},
    "sin":        {"fp16": 2, "fp32": 2, "fp64": 2, "default": 2, "source": "CUDA"},
    "cos":        {"fp16": 2, "fp32": 2, "fp64": 2, "default": 2, "source": "CUDA"},
    "acos":       {"fp16": 2, "fp32": 2, "fp64": 2, "default": 2, "source": "CUDA"},
    "atan2":      {"fp16": 3, "fp32": 3, "fp64": 2, "default": 3, "source": "CUDA"},
    "invsqrt":    {"fp16": 2, "fp32": 2, "fp64": 2, "default": 2, "source": "CUDA rsqrt / project"},
    "sigmoid":    {"fp16": 4, "fp32": 4, "fp64": 4, "default": 4, "source": "project (composed)"},
    "softplus":   {"fp16": 4, "fp32": 4, "fp64": 4, "default": 4, "source": "project (composed)"},
}


def budget_for(operator: str, fmt_name: str) -> float:
    """Max tolerated ULP for one function at one format. Untabulated formats inherit default."""
    if operator not in BUDGETS:
        raise KeyError(operator)
    row = BUDGETS[operator]
    return float(row.get(fmt_name, row["default"]))

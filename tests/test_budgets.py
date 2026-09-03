"""The budget table, the doc, and the operator set must agree.

SPECIFICATION.md is the human copy; budgets.py is the machine copy. This parses the doc's table and
asserts the two are identical, so a number can never be changed in one place only.
"""

import os
import re

import pytest

from scripts.verification.budgets import BUDGETS, budget_for
from scripts.verification.reference.mpfr import OPERATORS

SPEC = os.path.join(os.path.dirname(__file__), "..", "scripts", "verification", "SPECIFICATION.md")


def _spec_rows():
    """Parse the budget matrix: {function: (fp16, fp32, fp64)}."""
    rows = {}
    for line in open(SPEC):
        if not line.lstrip().startswith("|"):
            continue
        cols = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cols) != 6 or cols[0] in ("function", "") or set(cols[0]) <= set("-: "):
            continue
        fn = cols[0]
        try:
            rows[fn] = (int(cols[2]), int(cols[3]), int(cols[4]))
        except ValueError:
            continue
    return rows


def test_doc_and_table_agree():
    rows = _spec_rows()
    assert set(rows) == set(BUDGETS), "functions differ between SPECIFICATION.md and budgets.py"
    for fn, (h, s, d) in rows.items():
        assert (h, s, d) == (BUDGETS[fn]["fp16"], BUDGETS[fn]["fp32"], BUDGETS[fn]["fp64"]), fn


def test_default_is_the_float_budget():
    for fn, row in BUDGETS.items():
        assert row["default"] == row["fp32"], fn


def test_every_runnable_operator_has_a_budget():
    # smg is dropped; everything the reference can evaluate must have a budget.
    for op in OPERATORS:
        assert op in BUDGETS, f"{op} has no ULP budget"


@pytest.mark.parametrize("op, fmt, want", [
    ("sqrt", "fp32", 0.0),
    ("exp", "fp64", 1.0),
    ("exp", "fp16", 2.0),      # fp16 inherits float
    ("atan2", "fp16", 3.0),
    ("sigmoid", "bf16", 4.0),  # untabulated format inherits default
])
def test_budget_for(op, fmt, want):
    assert budget_for(op, fmt) == want


def test_unknown_operator_raises():
    with pytest.raises(KeyError):
        budget_for("nope", "fp32")


TIER1_BITEXACT = {"add", "sub", "mul", "div", "fma", "recfn_roundtrip", "sqrt", "reciprocal"}


def test_tier1_ops_are_bit_exact():
    # The correctly-rounded (IEEE §5.4.1) group is 0 ULP at every format.
    for op in TIER1_BITEXACT:
        row = BUDGETS[op]
        assert row["fp16"] == row["fp32"] == row["fp64"] == 0, op

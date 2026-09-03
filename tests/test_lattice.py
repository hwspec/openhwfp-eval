"""The TestFloat-style lattice: right size, in-domain per function, seams present."""

import math

import pytest

from scripts.verification.formats import FpFormat
from scripts.verification.reference import mpfr as ref
from scripts.verification.stimulus.lattice import (
    SIN_COS_RANGE, lattice, operand_sets, p_patterns, q_exponents,
)

FP16 = FpFormat(5, 11, "fp16")
FP32 = FpFormat(8, 24, "fp32")
FP64 = FpFormat(11, 53, "fp64")


@pytest.mark.parametrize("fmt", [FP16, FP32, FP64])
def test_level1_lattice_is_88(fmt):
    # 2 signs x 11 exponent seeds x 4 P1 patterns, exactly like TestFloat level 1.
    assert len(lattice(fmt, 1)) == 88


@pytest.mark.parametrize("fmt", [FP16, FP32, FP64])
def test_level2_is_denser(fmt):
    assert len(lattice(fmt, 2)) > len(lattice(fmt, 1))


def test_q_and_p_sizes():
    assert len(q_exponents(FP32)) == 11
    assert len(p_patterns(FP32, 1)) == 4


@pytest.mark.parametrize("op", ["sqrt", "log", "invsqrt"])
def test_nonneg_domain(op):
    for (b,) in operand_sets(op, FP32, 1, 1):
        assert (b >> 31) == 0, f"{op} operand {b:#010x} is negative"


def test_acos_domain_is_unit_interval():
    for (b,) in operand_sets("acos", FP32, 1, 1):
        assert abs(float(ref.decode(b, FP32))) <= 1.0


@pytest.mark.parametrize("op", ["sin", "cos"])
def test_trig_domain_is_bounded(op):
    for (b,) in operand_sets(op, FP32, 1, 1):
        assert abs(float(ref.decode(b, FP32))) <= SIN_COS_RANGE + 1e-6


def test_seams_are_present():
    acos = {b for (b,) in operand_sets("acos", FP32, 1, 1)}
    for v in (-1.0, 0.0, 1.0):
        assert ref.encode(v, FP32) in acos, f"acos missing seam {v}"


def test_atan2_is_pairs():
    sets = operand_sets("atan2", FP32, 1, 2)
    assert sets and all(len(s) == 2 for s in sets)

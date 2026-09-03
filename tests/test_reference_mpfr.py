"""Pin the MPFR reference before anything trusts it.

The eyeball-checkable hex cases guard the encode path; the fp16 subnormal cases guard the emin
convention, which is the one thing most likely to be silently wrong.
"""

import struct

import gmpy2
import pytest
from gmpy2 import mpfr

from scripts.verification.formats import FpFormat
from scripts.verification.reference import mpfr as ref

FP16 = FpFormat(5, 11, "fp16")
FP32 = FpFormat(8, 24, "fp32")
FP64 = FpFormat(11, 53, "fp64")


def b32(x):
    return struct.unpack("<I", struct.pack("<f", x))[0]


@pytest.mark.parametrize("op, operands, expected", [
    ("exp",        [b32(0.0)],          0x3f800000),  # 1.0
    ("sqrt",       [b32(4.0)],          0x40000000),  # 2.0
    ("log",        [b32(1.0)],          0x00000000),  # +0
    ("reciprocal", [b32(2.0)],          0x3f000000),  # 0.5
    ("exp",        [b32(1.0)],          0x402df854),  # e, rounds
    ("sigmoid",    [b32(0.0)],          0x3f000000),  # 0.5
    ("atan2",      [b32(1.0), b32(1.0)], 0x3f490fdb),  # pi/4, two operand
])
def test_known_fp32_results(op, operands, expected):
    assert ref.evaluate(op, operands, FP32) == expected


def test_softplus_zero_is_ln2():
    # log(2) is the reference for softplus(0); compare the engine to encode(log2) directly.
    assert ref.evaluate("softplus", [b32(0.0)], FP32) == ref.encode(gmpy2.log(mpfr(2)), FP32)


@pytest.mark.parametrize("value, expected", [
    (gmpy2.mpfr(2) ** -24, 0x0001),        # min subnormal survives
    (gmpy2.mpfr(2) ** -25, 0x0000),        # one step below flushes ties-to-even
    (3 * gmpy2.mpfr(2) ** -25, 0x0002),    # tie rounds up to even
    (5 * gmpy2.mpfr(2) ** -25, 0x0002),    # tie rounds down to even
    (gmpy2.mpfr("1e10"), 0x7c00),          # overflow to +inf
])
def test_fp16_emin_and_overflow(value, expected):
    assert ref.encode(value, FP16) == expected


def test_signed_zero_is_preserved():
    assert ref.encode(mpfr("-0"), FP32) == 0x80000000
    assert ref.encode(mpfr("0"), FP32) == 0x00000000


@pytest.mark.parametrize("fmt, pack, unpack", [
    (FP32, "<f", "<I"),
    (FP64, "<d", "<Q"),
])
def test_struct_parity(fmt, pack, unpack):
    # encode of a value equals the native cast's bit pattern, the strongest single guard.
    import random
    rng = random.Random(1)
    for _ in range(2000):
        x = struct.unpack(pack, rng.getrandbits(fmt.width).to_bytes(fmt.width // 8, "little"))[0]
        if x != x or x in (float("inf"), float("-inf")):
            continue
        assert ref.encode(mpfr(x), fmt) == struct.unpack(unpack, struct.pack(pack, x))[0]


@pytest.mark.parametrize("fmt", [FP16, FP32, FP64])
def test_encode_decode_round_trip(fmt):
    # Any finite pattern survives decode then encode unchanged.
    import random
    rng = random.Random(2)
    for _ in range(3000):
        bits = rng.getrandbits(fmt.width)
        if fmt.classify(bits) in ("nan", "inf"):
            continue
        assert ref.encode(ref.decode(bits, fmt), fmt) == bits


def test_rto_and_smg_are_refused():
    with pytest.raises(NotImplementedError):
        ref.build_context(FP32, "rto")
    with pytest.raises(NotImplementedError):
        ref.evaluate("smg", [b32(1.0)], FP32)

"""TestFloat-style operand lattice, format-parametric, with per-function domains.

TestFloat builds each operand as a sign+exponent seed (Q) OR-ed with a significand pattern (P). We
replicate that construction from FpFormat fields rather than shipping its C tables, so the same
corner density applies to fp16/fp32/fp64 and any custom format. On top of the lattice each function
carries a domain filter (so we test the range the hardware claims, not a wall of NaNs) and a short
list of seams (exact points that matter for that function).
"""

from __future__ import annotations

import math

from ..reference import mpfr as ref

# Rial's sincos argument reduction asserts an internal normalization invariant (sincos.scala:237)
# that can still fire outside a clean range, so this bounds the primary test range and the abort
# guard catches anything that slips through.
SIN_COS_RANGE = math.pi


def _sign_bit(fmt):
    return 1 << (fmt.width - 1)


def q_exponents(fmt):
    """~11 biased-exponent seeds: subnormal, min normal, the precision boundaries, around 1.0,
    max finite, and the inf/NaN encoding."""
    b, sm, em = fmt.bias, fmt.stored_mantissa_bits, fmt.exp_max
    return sorted({
        0, 1,
        max(1, b - sm),                 # below the precision boundary
        b - 2, b - 1, b, b + 1, b + 2,  # 1/4 1/2 1 2 4
        min(em - 1, b + sm),            # above the precision boundary
        em - 1,                         # max finite exponent
        em,                             # inf / NaN
    })


def p_patterns(fmt, level):
    """Significand patterns. P1 at level 1; P2 adds the single-bit walk and complements."""
    sm = fmt.stored_mantissa_bits
    full = (1 << sm) - 1
    p1 = [0, 1, full, full - 1]
    if level < 2:
        return p1
    walk = [1 << i for i in range(sm)]
    comp = [full ^ (1 << i) for i in range(sm)]
    return list(dict.fromkeys(p1 + walk + comp))


def lattice(fmt, level=1):
    """Q x P cross product as a set of bit patterns. 2 x 11 x 4 = 88 at level 1, like TestFloat."""
    sm = fmt.stored_mantissa_bits
    sign = _sign_bit(fmt)
    return {
        s | (e << sm) | p
        for s in (0, sign)
        for e in q_exponents(fmt)
        for p in p_patterns(fmt, level)
    }


# ---- per-function domains -------------------------------------------------------------------

def _finite(bits, fmt):
    return fmt.classify(bits) in ("zero", "subnormal", "normal")


def _nonneg(bits, fmt):
    return (bits >> (fmt.width - 1)) == 0


def _abs_le(bound):
    def f(bits, fmt):
        return _finite(bits, fmt) and abs(float(ref.decode(bits, fmt))) <= bound
    return f


def _pow2(lo, hi):
    return [2.0 ** k for k in range(lo, hi + 1)]


# operator -> (filter or None, seams(fmt) -> [real values]). None filter keeps every lattice point.
DOMAINS = {
    "sqrt":       (_nonneg, lambda fmt: [0.0, 1.0, 4.0, 9.0, 16.0, 0.25] + _pow2(-4, 4)),
    "invsqrt":    (_nonneg, lambda fmt: [1.0, 4.0, 0.25] + _pow2(-4, 4)),
    "log":        (_nonneg, lambda fmt: [1.0, 0.5, math.e, 2.0] + _pow2(-4, 4)),
    "reciprocal": (None,    lambda fmt: [1.0, -1.0, 2.0, -2.0] + _pow2(-4, 4) + [-p for p in _pow2(-4, 4)]),
    "exp":        (None,    lambda fmt: [0.0, 1.0, -1.0, math.log(2), -math.log(2), 50.0, -50.0]),
    "sigmoid":    (None,    lambda fmt: [0.0, 1.0, -1.0, 10.0, -10.0]),
    "softplus":   (None,    lambda fmt: [0.0, 1.0, -1.0, 10.0, -10.0]),
    "sin":        (_abs_le(SIN_COS_RANGE), lambda fmt: [k * math.pi / 2 for k in range(-2, 3)] + [0.0, 2 ** -5, -2 ** -5]),
    "cos":        (_abs_le(SIN_COS_RANGE), lambda fmt: [k * math.pi / 2 for k in range(-2, 3)] + [0.0, 2 ** -5, -2 ** -5]),
    "acos":       (_abs_le(1.0), lambda fmt: [-1.0, -0.5, 0.0, 0.5, 1.0]),
}


def _corner_set(fmt):
    """A small structured set of bit patterns for the 2-operand cross product (atan2)."""
    sm, sign = fmt.stored_mantissa_bits, _sign_bit(fmt)
    one = fmt.bias << sm
    maxf = ((fmt.exp_max - 1) << sm) | ((1 << sm) - 1)
    base = {0, 1 << sm, one, maxf, fmt.exp_max << sm}   # +0, min normal, 1, max finite, inf
    return base | {b | sign for b in base}


def operand_sets(function, fmt, level=1, arity=1):
    """Structured operand tuples (as bit-pattern lists) for one function: lattice ∩ domain ∪ seams."""
    if arity == 2:
        corners = sorted(_corner_set(fmt))
        return [[y, x] for y in corners for x in corners]

    filt, seams = DOMAINS.get(function, (None, None))
    pts = lattice(fmt, level)
    if filt is not None:
        pts = {b for b in pts if filt(b, fmt)}
    if seams is not None:
        pts |= {ref.encode(v, fmt) for v in seams(fmt)}
    return [[b] for b in sorted(pts)]

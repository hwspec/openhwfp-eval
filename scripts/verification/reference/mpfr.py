"""MPFR reference for tier-2 math functions.

Produces the correctly rounded expected result as a target-format bit pattern, so the same
comparator that scores tier-1 bit patterns scores tier-2. gmpy2 does the rounding; we only assemble
the fields.

Two things that bite and are pinned by tests. MPFR normalizes significands to [1/2, 1), so its
exponent runs one above IEEE's: emin/emax below are not the naive guesses. And gmpy2.frexp is
reversed from math.frexp, so field extraction goes through digits(2), which is exact and never
double-rounds.
"""

from __future__ import annotations

import gmpy2
from gmpy2 import mpfr

# Extra precision the composed operators run at before the single format rounding in encode().
GUARD = 64

# capabilities.ROUNDING names -> gmpy2 rounding. rto has no MPFR equivalent; tier-2 is rne only.
_ROUND = {
    "rne": gmpy2.RoundToNearest,
    "rtz": gmpy2.RoundToZero,
    "rdn": gmpy2.RoundDown,
    "rup": gmpy2.RoundUp,
    "rna": gmpy2.RoundAwayZero,  # not exact ties-away, but tier-2 never calls it
}


def build_context(fmt, rounding="rne"):
    """A gmpy2 context whose rounding lands values in fmt, subnormals included."""
    if rounding == "rto":
        raise NotImplementedError("round-to-odd has no MPFR equivalent; tier-2 is rne-only")
    if rounding not in _ROUND:
        raise ValueError(f"unknown rounding {rounding!r}")
    return gmpy2.context(
        precision=fmt.sig_bits,
        emax=1 << (fmt.exp_bits - 1),
        emin=4 - (1 << (fmt.exp_bits - 1)) - fmt.sig_bits,
        subnormalize=True,
        round=_ROUND[rounding],
        trap_invalid=False, trap_divzero=False,
        trap_overflow=False, trap_underflow=False,
    )


def decode(bits, fmt):
    """Bit pattern to exact mpfr. NaN/Inf come back as MPFR specials so operators propagate them."""
    sign, exp, mant = fmt.split(bits)
    if exp == fmt.exp_max:
        if mant:
            return mpfr("nan")
        return mpfr("-inf") if sign else mpfr("inf")
    with gmpy2.context(precision=fmt.sig_bits + 2):
        if exp == 0:
            v = mpfr(mant) * mpfr(2) ** (2 - fmt.bias - fmt.sig_bits)
        else:
            sig = (1 << fmt.stored_mantissa_bits) | mant
            v = mpfr(sig) * mpfr(2) ** (exp - fmt.bias - fmt.stored_mantissa_bits)
    return -v if sign else v


def encode(value, fmt, rounding="rne"):
    """mpfr (or anything mpfr() accepts) to a bit pattern, correctly rounded to fmt.

    The single rounding is the +mpfr(value) under the format context. Fields come from digits(2),
    which is exact, so nothing rounds twice.
    """
    sm = fmt.stored_mantissa_bits
    mmask = (1 << sm) - 1
    with build_context(fmt, rounding):
        r = +mpfr(value)

    top = (1 << (fmt.width - 1)) if gmpy2.is_signed(r) else 0
    if gmpy2.is_nan(r):
        return top | (fmt.exp_max << sm) | (1 << (sm - 1))  # quiet NaN
    if gmpy2.is_infinite(r):
        return top | (fmt.exp_max << sm)
    if gmpy2.is_zero(r):
        return top

    mant, exp2, _ = abs(r).digits(2)     # value = int(mant,2) * 2^(exp2 - len(mant))
    M = int(mant, 2)
    L = len(mant)
    E = exp2 - 1                          # IEEE unbiased exponent of the leading bit
    if E >= 1 - fmt.bias:                 # normal
        biased = E + fmt.bias
        if biased >= fmt.exp_max:         # overflow to inf
            return top | (fmt.exp_max << sm)
        shift = fmt.sig_bits - L
        full = (M << shift) if shift >= 0 else (M >> -shift)
        return top | (biased << sm) | (full & mmask)
    # subnormal: biased exponent 0
    e = exp2 - L - (2 - fmt.bias - fmt.sig_bits)
    field = (M << e) if e >= 0 else (M >> -e)
    return top | (field & mmask)


def _reciprocal(x): return mpfr(1) / x
def _invsqrt(x): return gmpy2.rec_sqrt(x)
def _sigmoid(x): return mpfr(1) / (mpfr(1) + gmpy2.exp(-x))


def _softplus(x):
    # log1p(exp(x)) is accurate for x<=0; fold large positive x to avoid exp overflow.
    return gmpy2.log1p(gmpy2.exp(x)) if x <= 0 else x + gmpy2.log1p(gmpy2.exp(-x))


# name -> (arity, callable over decoded mpfr operands). gmpy2 already returns IEEE specials on
# domain errors (log of <=0, acos out of range, sqrt of negative), so no manual guards.
OPERATORS = {
    "exp": (1, lambda x: gmpy2.exp(x)),
    "sin": (1, lambda x: gmpy2.sin(x)),
    "cos": (1, lambda x: gmpy2.cos(x)),
    "log": (1, lambda x: gmpy2.log(x)),
    "sqrt": (1, lambda x: gmpy2.sqrt(x)),
    "acos": (1, lambda x: gmpy2.acos(x)),
    "atan2": (2, lambda y, x: gmpy2.atan2(y, x)),
    "reciprocal": (1, _reciprocal),
    "invsqrt": (1, _invsqrt),
    "sigmoid": (1, _sigmoid),
    "softplus": (1, _softplus),
}


def arity(name):
    if name == "smg":
        raise NotImplementedError("smg semantics undefined")
    return OPERATORS[name][0]


def evaluate(name, operand_bits, fmt, rounding="rne"):
    """Expected result bits for one operand set. Composed at excess precision, rounded once."""
    if name == "smg":
        raise NotImplementedError("smg semantics undefined")
    ar, fn = OPERATORS[name]
    with gmpy2.context(precision=fmt.sig_bits + GUARD):
        args = [decode(b, fmt) for b in operand_bits]
        result = fn(*args)
    return encode(result, fmt, rounding)

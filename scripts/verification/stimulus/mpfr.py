"""Operand generator for tier-2 math functions, expected values from the MPFR reference.

Sibling of testfloat.generate. Same VectorSet, same sha256 sidecar, same cache reuse. The corpus is
the TestFloat-style Q x P lattice restricted to each function's domain, plus its seams, plus seeded
in-domain random for breadth. Line format is "operand(s) expected" in bare hex, no flags column.
"""

from __future__ import annotations

import json
import math
import os
import random

import gmpy2
from gmpy2 import mpfr

from ..reference import mpfr as ref
from .lattice import SIN_COS_RANGE, operand_sets
from .testfloat import DEFAULT_DIR, VectorSet, _sha256


def _pos_mag(rng):
    return math.exp(rng.uniform(-60.0, 60.0))


def _signed_mag(rng):
    return math.copysign(math.exp(rng.uniform(-60.0, 60.0)), rng.uniform(-1.0, 1.0))


def _random_values(function, arity, rng):
    """One domain-appropriate operand tuple of Python floats, so most rows land in-scope."""
    if arity == 2:                                   # atan2
        return _signed_mag(rng), _signed_mag(rng)
    if function in ("log", "sqrt", "invsqrt"):
        return (_pos_mag(rng),)
    if function == "reciprocal":
        return (_signed_mag(rng),)
    if function == "acos":
        return (rng.uniform(-1.0, 1.0),)
    if function in ("sin", "cos"):
        return (rng.uniform(-SIN_COS_RANGE, SIN_COS_RANGE),)
    return (rng.uniform(-100.0, 100.0),)             # exp sigmoid softplus


def _all_sets(function, fmt, arity, level, seed, count):
    """Lattice ∩ domain ∪ seams, then seeded random. Deduped, order stable."""
    seen, out = set(), []

    def emit(t):
        if t not in seen:
            seen.add(t)
            out.append(list(t))

    for s in operand_sets(function, fmt, level, arity):
        emit(tuple(s))
    rng = random.Random(seed)
    for _ in range(count):
        vals = _random_values(function, arity, rng)
        emit(tuple(ref.encode(mpfr(v), fmt) for v in vals))
    return out


def generate(function: str, fmt, rounding: str = "rne", seed: int = 1, count: int = 20000,
             level: int = 1, out_dir: str = DEFAULT_DIR, force: bool = False) -> VectorSet:
    """Build (or reuse) the tier-2 vector file for one function and format.

    count is the number of random samples on top of the structured lattice+seams. level picks the
    significand density (1 = P1, 2 = P2). Returns a VectorSet with reference="mpfr".
    """
    arity = ref.arity(function)
    os.makedirs(out_dir, exist_ok=True)
    stem = f"{function}__{fmt.name}__{rounding}__s{seed}__n{count}__L{level}__mpfr"
    path = os.path.join(out_dir, stem + ".txt")
    meta_path = path + ".json"
    generator = f"mpfr {gmpy2.version()} {gmpy2.mpfr_version()}"

    if os.path.exists(path) and os.path.exists(meta_path) and not force:
        with open(meta_path) as mfh:
            meta = json.load(mfh)
        if meta.get("sha256") == _sha256(path) and meta.get("reference") == "mpfr":
            meta["path"] = path
            return VectorSet(**meta)

    digits = (fmt.width + 3) // 4
    sets = _all_sets(function, fmt, arity, level, seed, count)
    with open(path, "w") as fh:
        for ops in sets:
            expected = ref.evaluate(function, ops, fmt, rounding)
            cols = [f"{b:0{digits}x}" for b in ops] + [f"{expected:0{digits}x}"]
            fh.write(" ".join(cols) + "\n")

    vs = VectorSet(
        path=path, function=function, rounding=rounding, tininess="after", level=level,
        seed=seed, count=len(sets), operands=arity, sha256=_sha256(path),
        generator=generator, truncated_from=None, reference="mpfr",
    )
    with open(meta_path, "w") as fh:
        json.dump(vs.sidecar(), fh, indent=2)
        fh.write("\n")
    return vs

"""One comparator for both tiers.

Tier 1 wants bit equality. Tier 2 wants a ULP bound. Both want the same equivalence rules for
NaN and signed zero, and both honour the profile's masking. Keeping them in one function is the
only way a pass on hardfloat and a pass on Rial mean comparable things.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional

from .capabilities import FLAG_BITS, Profile
from .formats import INF, NAN, SUBNORMAL, ZERO, FpFormat


@dataclass
class Mismatch:
    index: int
    kind: str
    operands: List[int]
    got: int
    expected: int
    got_flags: Optional[int]
    expected_flags: Optional[int]
    ulp: Optional[float]
    note: str = ""

    def render(self, fmt: FpFormat) -> str:
        ops = " ".join(fmt.hex(o) for o in self.operands)
        line = f"[{self.index}] {self.kind}: {ops} -> got {fmt.hex(self.got)} want {fmt.hex(self.expected)}"
        if self.got_flags is not None and self.expected_flags is not None:
            line += f" | flags got {self.got_flags:02x} want {self.expected_flags:02x}"
        if self.ulp is not None:
            line += f" | {self.ulp:g} ulp"
        return line + (f" | {self.note}" if self.note else "")


def ulp_bucket(ulp) -> str:
    """Coarse ULP band. Keeps a 1 ulp rounding slip in a different bucket from a wild answer."""
    if ulp is None:
        return "na"
    if ulp == float("inf"):
        return "inf"
    if ulp == 0:
        return "0"
    if ulp <= 1:
        return "1"
    if ulp <= 4:
        return "2-4"
    if ulp <= 1024:
        return "5-1k"
    return ">1k"


def categorize(kind, operands, got, expected, fmt, ulp):
    """Group a mismatch by the shape of the failure, not by its operands.

    A dataset that lists every failing vector is unusable; 12,062 rows of `0 + x` teach nothing
    that one row plus a count does not. The signature is what makes them collapse.
    """
    ins = [fmt.classify(o) for o in operands]
    gc = fmt.classify(got)
    ec = fmt.classify(expected)
    parts = [kind, "+".join(ins), f"{gc}->{ec}"]
    if kind in ("value", "ulp_budget"):
        parts.append(f"ulp{ulp_bucket(ulp)}")
    return {
        "signature": "|".join(parts),
        "kind": kind,
        "input_classes": ins,
        "got_class": gc,
        "expected_class": ec,
        "ulp_band": ulp_bucket(ulp) if kind in ("value", "ulp_budget") else None,
    }


def describe(cat) -> str:
    """One readable line per category. Written for whoever reads the dataset, not the debugger.

    Every line names what came out and what was wanted, in that order, so a reader never has to
    decode the signature's got->expected arrow to know which side is which.
    """
    ins = " x ".join(cat["input_classes"])
    got, exp = cat["got_class"], cat["expected_class"]
    if cat["kind"] == "nan_mismatch":
        return f"{ins}: got {got}, expected {exp}"
    if cat["kind"] == "infinity":
        if got == exp == "inf":
            return f"{ins}: got an infinity of the wrong sign"
        return f"{ins}: got {got}, expected {exp}"
    if cat["kind"] == "nan_payload":
        return f"{ins}: got a NaN with a different payload"
    if cat["kind"] == "signed_zero":
        return f"{ins}: got a zero of the wrong sign"
    if cat["kind"] == "flags":
        return f"{ins}: correct value, wrong exception flags"
    band = cat["ulp_band"]
    if band == "1":
        return f"{ins}: got {got}, expected {exp}, 1 ulp off"
    return f"{ins}: got {got}, expected {exp}, {band} ulp off"


@dataclass
class Verdict:
    ok: bool
    kind: str = ""
    ulp: Optional[float] = None
    note: str = ""


def flag_names(mask: int) -> List[str]:
    return [name for i, name in enumerate(FLAG_BITS) if mask >> i & 1]


def compare_value(got: int, expected: int, fmt: FpFormat, profile: Profile, tier: int) -> Verdict:
    got &= fmt.mask
    expected &= fmt.mask
    if got == expected:
        return Verdict(True, ulp=0.0)

    kg, ke = fmt.classify(got), fmt.classify(expected)

    if kg == NAN and ke == NAN:
        if profile.nan_payload_sensitive:
            return Verdict(False, "nan_payload", note="payload differs and profile claims ieee")
        return Verdict(True, ulp=0.0, note="nan payload ignored")
    if kg == NAN or ke == NAN:
        return Verdict(False, "nan_mismatch")

    if kg == ZERO and ke == ZERO:
        if profile.signed_zero_sensitive:
            return Verdict(False, "signed_zero", note="zero sign differs and profile claims ieee")
        return Verdict(True, ulp=0.0, note="zero sign ignored")

    if kg == INF or ke == INF:
        return Verdict(False, "infinity", ulp=float("inf"))

    ulp = fmt.ulp_distance(got, expected)
    if tier == 1:
        return Verdict(False, "value", ulp=ulp)
    budget = profile.ulp_budget
    if budget is not None and ulp <= budget:
        return Verdict(True, ulp=ulp)
    return Verdict(False, "ulp_budget", ulp=ulp, note=f"budget {budget}")


def compare_flags(got: Optional[int], expected: Optional[int], profile: Profile) -> Verdict:
    if not profile.checks_flags:
        return Verdict(True, note="flags not checked")
    if got is None or expected is None:
        return Verdict(False, "flags_missing", note="profile claims ieee5 but a value is absent")
    got &= 0x1F
    expected &= 0x1F
    if got == expected:
        return Verdict(True)
    differing = flag_names(got ^ expected)
    return Verdict(False, "flags", note="differs on " + ",".join(differing))


def in_scope(operands, expected, fmt: FpFormat, profile: Profile):
    """Is this vector inside what the DUT claims to implement?

    A flush-to-zero design has no opinion about a subnormal operand or a subnormal result, so
    holding it to the IEEE answer measures nothing. Those vectors are excluded and counted, never
    silently passed. Checking FTZ arithmetic exactly needs a reference driven with flushed inputs,
    which is a software model this tier does not have yet.
    """
    if profile.subnormals != "flushed":
        return True, None
    if any(fmt.classify(o) == SUBNORMAL for o in operands):
        return False, "subnormal_operand"
    if fmt.classify(expected) == SUBNORMAL:
        return False, "subnormal_result"
    return True, None


@dataclass
class Comparator:
    fmt: FpFormat
    profile: Profile
    tier: int = 1
    max_recorded: int = 20

    checks: int = 0
    mismatches: int = 0
    excluded: int = 0
    exclusions: dict = field(default_factory=dict)
    categories: dict = field(default_factory=dict)
    recorded: List[Mismatch] = field(default_factory=list)
    max_ulp: float = 0.0
    ulp_total: float = 0.0
    ulp_counted: int = 0
    ulp_histogram: dict = field(default_factory=dict)
    coverage: dict = field(default_factory=dict)

    def observe_inputs(self, operands):
        for op in operands:
            kind = self.fmt.classify(op)
            if kind == ZERO and (op >> (self.fmt.width - 1)):
                kind = "signed_zero"
            self.coverage[kind] = self.coverage.get(kind, 0) + 1

    def check(self, index, operands, got, expected, got_flags=None, expected_flags=None) -> bool:
        self.observe_inputs(operands)

        ok, reason = in_scope(operands, expected, self.fmt, self.profile)
        if not ok:
            self.excluded += 1
            self.exclusions[reason] = self.exclusions.get(reason, 0) + 1
            return True

        self.checks += 1

        v = compare_value(got, expected, self.fmt, self.profile, self.tier)
        f = compare_flags(got_flags, expected_flags, self.profile)

        if v.ulp is not None and v.ulp != float("inf"):
            self.max_ulp = max(self.max_ulp, v.ulp)
            self.ulp_total += v.ulp
            self.ulp_counted += 1
            bucket = "0" if v.ulp == 0 else f"<={2 ** (int(v.ulp - 1).bit_length())}"
            self.ulp_histogram[bucket] = self.ulp_histogram.get(bucket, 0) + 1

        if v.ok and f.ok:
            return True

        self.mismatches += 1
        kind = v.kind or f.kind
        m = Mismatch(
            index=index,
            kind=kind,
            operands=list(operands),
            got=got,
            expected=expected,
            got_flags=got_flags,
            expected_flags=expected_flags,
            ulp=v.ulp,
            note="; ".join(n for n in (v.note, f.note) if n),
        )
        if len(self.recorded) < self.max_recorded:
            self.recorded.append(m)

        cat = categorize(kind, operands, got, expected, self.fmt, v.ulp)
        slot = self.categories.get(cat["signature"])
        if slot is None:
            # First of its kind becomes the exemplar the dataset cites.
            self.categories[cat["signature"]] = {**cat, "count": 1, "exemplar": m}
        else:
            slot["count"] += 1
        return False

    @property
    def mean_ulp(self) -> float:
        return self.ulp_total / self.ulp_counted if self.ulp_counted else 0.0

    def category_report(self, exemplar_limit=25):
        """Every failure shape, ranked by frequency. Exemplars only for the most common ones.

        Dropping a rare shape entirely would hide a failure mode; carrying an exemplar for all of
        them bloats the record. Keeping the shape and spending the exemplar budget on the common
        ones is the compromise.
        """
        ranked = sorted(self.categories.values(), key=lambda c: -c["count"])
        out = []
        for rank, c in enumerate(ranked):
            m = c["exemplar"]
            entry = {
                "signature": c["signature"],
                "summary": describe(c),
                "kind": c["kind"],
                "input_classes": c["input_classes"],
                "got_class": c["got_class"],
                "expected_class": c["expected_class"],
                "ulp_band": c["ulp_band"],
                "count": c["count"],
            }
            if rank < exemplar_limit:
                entry["exemplar"] = {
                    "index": m.index,
                    "operands": [self.fmt.hex(o) for o in m.operands],
                    "got": self.fmt.hex(m.got),
                    "expected": self.fmt.hex(m.expected),
                    "got_flags": None if m.got_flags is None else f"{m.got_flags:02x}",
                    "expected_flags": None if m.expected_flags is None else f"{m.expected_flags:02x}",
                    "ulp": m.ulp,
                }
            out.append(entry)
        return out, max(0, len(ranked) - exemplar_limit)

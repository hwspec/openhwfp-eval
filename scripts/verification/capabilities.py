"""What a DUT claims to implement, and what that lets the comparator check.

A library that ignores exception flags is not a failure. It is a narrower claim, recorded as
such, so a pass on one row means the same thing as a pass on another only when the profiles match.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional

# testfloat_gen flag byte and hardfloat's exceptionFlags port agree bit for bit:
# invalid ## infinite ## overflow ## underflow ## inexact, LSB is inexact.
FLAG_BITS = ("inexact", "underflow", "overflow", "divzero", "invalid")

# testfloat_gen switch and the 3-bit value hardfloat's roundingMode port expects.
ROUNDING = {
    "rne": ("-rnear_even", 0),
    "rtz": ("-rminMag", 1),
    "rdn": ("-rmin", 2),
    "rup": ("-rmax", 3),
    "rna": ("-rnear_maxMag", 4),
    "rto": ("-rodd", 6),
}

TININESS = {"before": ("-tininessbefore", 0), "after": ("-tininessafter", 1)}


@dataclass(frozen=True)
class Profile:
    rounding_modes: List[str]
    rounding_control: str
    exception_flags: str
    tininess: List[str] = field(default_factory=lambda: ["after"])
    subnormals: str = "unknown"
    nan_payload: str = "ignored"
    signed_zero: str = "ignored"
    ulp_budget: Optional[float] = None
    not_evaluated_reason: Optional[str] = None

    @classmethod
    def from_manifest(cls, prof: dict) -> "Profile":
        return cls(
            rounding_modes=list(prof["rounding_modes"]),
            rounding_control=prof["rounding_control"],
            exception_flags=prof["exception_flags"],
            tininess=list(prof.get("tininess") or ["after"]),
            subnormals=prof.get("subnormals", "unknown"),
            nan_payload=prof.get("nan_payload", "ignored"),
            signed_zero=prof.get("signed_zero", "ignored"),
            ulp_budget=prof.get("ulp_budget"),
            not_evaluated_reason=prof.get("not_evaluated_reason"),
        )

    @property
    def checks_flags(self) -> bool:
        return self.exception_flags == "ieee5"

    @property
    def flag_check(self) -> str:
        return "exact" if self.checks_flags else "none"

    @property
    def nan_payload_sensitive(self) -> bool:
        return self.nan_payload == "ieee"

    @property
    def signed_zero_sensitive(self) -> bool:
        return self.signed_zero == "ieee"

    def runs(self):
        """The (rounding_mode, tininess) pairs this DUT can actually be driven through.

        A DUT with no rounding-mode port gets one run at its single declared mode. Sweeping
        would just be the same simulation several times with a different label.
        """
        tin = self.tininess if self.checks_flags else self.tininess[:1]
        for mode in self.rounding_modes:
            for t in tin:
                yield mode, t

    def summary(self) -> str:
        modes = "+".join(self.rounding_modes)
        return f"{modes}/{self.exception_flags}/{self.subnormals}"

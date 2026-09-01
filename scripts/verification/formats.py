"""Binary FP formats as (exp_bits, sig_bits), where sig_bits counts the hidden bit.

fp32 is (8, 24). Rial and OpenFloat both store 23; the descriptor normalizes that away so one
comparator serves every library.
"""

from __future__ import annotations

from dataclasses import dataclass

NAN = "nan"
INF = "inf"
ZERO = "zero"
SUBNORMAL = "subnormal"
NORMAL = "normal"


@dataclass(frozen=True)
class FpFormat:
    exp_bits: int
    sig_bits: int
    name: str = ""

    @property
    def stored_mantissa_bits(self) -> int:
        return self.sig_bits - 1

    @property
    def width(self) -> int:
        return 1 + self.exp_bits + self.stored_mantissa_bits

    @property
    def bias(self) -> int:
        return (1 << (self.exp_bits - 1)) - 1

    @property
    def mask(self) -> int:
        return (1 << self.width) - 1

    @property
    def exp_max(self) -> int:
        return (1 << self.exp_bits) - 1

    @classmethod
    def from_manifest(cls, fmt: dict) -> "FpFormat":
        return cls(exp_bits=fmt["exp_bits"], sig_bits=fmt["sig_bits"], name=fmt.get("name", ""))

    def split(self, bits: int):
        """Return (sign, biased_exponent, stored_mantissa)."""
        bits &= self.mask
        m_bits = self.stored_mantissa_bits
        sign = bits >> (self.width - 1)
        exponent = (bits >> m_bits) & self.exp_max
        mantissa = bits & ((1 << m_bits) - 1)
        return sign, exponent, mantissa

    def classify(self, bits: int) -> str:
        _, exponent, mantissa = self.split(bits)
        if exponent == self.exp_max:
            return NAN if mantissa else INF
        if exponent == 0:
            return ZERO if mantissa == 0 else SUBNORMAL
        return NORMAL

    def is_quiet_nan(self, bits: int) -> bool:
        _, exponent, mantissa = self.split(bits)
        if exponent != self.exp_max or mantissa == 0:
            return False
        return bool(mantissa >> (self.stored_mantissa_bits - 1))

    def monotone_key(self, bits: int) -> int:
        """Map a bit pattern to an integer that increases with the real value.

        Only meaningful for finite values. Adjacent representable numbers differ by one, which is
        what makes ULP distance a subtraction.
        """
        sign, _, _ = self.split(bits)
        magnitude = bits & ((1 << (self.width - 1)) - 1)
        return -magnitude if sign else magnitude

    def ulp_distance(self, a: int, b: int) -> float:
        """Representable steps between two finite patterns. inf if either is not finite."""
        if self.classify(a) in (NAN, INF) or self.classify(b) in (NAN, INF):
            return float("inf")
        return float(abs(self.monotone_key(a) - self.monotone_key(b)))

    def hex(self, bits: int) -> str:
        digits = (self.width + 3) // 4
        return f"0x{bits & self.mask:0{digits}x}"

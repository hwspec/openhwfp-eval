# ULP Budget Specification

This file is the source of truth for the per-function, per-format ULP budgets used for verification. Note that the machine copy is `scripts/verification/budgets.py`; a test (`tests/test_budgets.py`) parses the table below and asserts that the two agree.

You can read more about ULP [here](https://inria.hal.science/inria-00070503v1/file/RR2005-09.pdf), and why we can't verify correct rounding for transcedental functions, known as the Table Maker's Dilemma, [here](https://perso.ens-lyon.fr/jean-michel.muller/Intro-to-TMD.htm).

## Why ULP budgets?

IEEE 754-2019 requires **correct rounding** (0 ULP of added error) only for the algebraic
operations: `+ − × ÷ √`, fused multiply-add, remainder, `roundToIntegral`, and conversions
(§5.4.1). Those are checked bit-exactly by **tier 1**.

The transcendental functions (`exp`, `log`, `sin`, `cos`, `atan2`, …) are **recommended** to be
correctly rounded (§9.2) but not required, and ISO C Annex F leaves their accuracy unspecified. No
mandatory ULP bound exists for them anywhere in the standard. The de-facto bounds come from the
accelerator vendors; we use NVIDIA's CUDA C Programming Guide, "Mathematical Functions" appendix,
which publishes a max-ULP table per function for single and double precision:
<https://docs.nvidia.com/cuda/cuda-programming-guide/05-appendices/mathematical-functions.html#cuda-and-ieee-754-compliance>

All budgets below assume **round-to-nearest-even**, which is the mode tier 2 tests. A budget is the
maximum tolerated ULP distance between the DUT output and the MPFR reference; the record always
carries the observed `max_ulp`/`mean_ulp` regardless of the pass/fail verdict.

## Budget matrix

`ulp` is the number of representable steps between the DUT result and the correctly rounded
reference. Format columns are the three IEEE binary formats we currently target.

### Correctly rounded - 0 ULP (tier 1)

IEEE 754-2019 §5.4.1 mandates these be correctly rounded, so the budget is 0 at every format. Tier 1 checks them bit-exactly against berkeley-softfloat.

| function | definition | fp16 | fp32 | fp64 | source |
|---|---|---|---|---|---|
| add | $x + y$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 |
| sub | $x - y$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 |
| mul | $x \times y$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 |
| div | $x / y$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 |
| sqrt | $\sqrt{x}$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 (correctly rounded); CUDA |
| reciprocal | $1/x$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 (division); CUDA |
| fma | $x \times y + z$ | 0 | 0 | 0 | IEEE 754-2019 §5.4.1 |
| recfn_roundtrip | recode then decode | 0 | 0 | 0 | identity roundtrip (bit-exact) |

### Bounded - CUDA per-function ULP (tier 2)

| function | definition | fp16 | fp32 | fp64 | source |
|---|---|---|---|---|---|
| exp | $e^{x}$ | 2 | 2 | 1 | CUDA |
| log | $\ln x$ | 1 | 1 | 1 | CUDA |
| sin | $\sin x$ | 2 | 2 | 2 | CUDA |
| cos | $\cos x$ | 2 | 2 | 2 | CUDA |
| acos | $\arccos x$ | 2 | 2 | 2 | CUDA |
| atan2 | $\operatorname{atan2}(y, x)$ | 3 | 3 | 2 | CUDA |
| invsqrt | $1/\sqrt{x}$ | 2 | 2 | 2 | CUDA rsqrt intrinsic; project default (no IEEE mandate) |
| sigmoid | $\sigma(x) = \dfrac{1}{1 + e^{-x}}$ | 4 | 4 | 4 | project default (no standard); composed exp + reciprocal |
| softplus | $\zeta(x) = \ln(1 + e^{x})$ | 4 | 4 | 4 | project default (no standard); composed log + exp |

## Default rules

- **fp16.** CUDA tabulates single and double precision only. fp16 inherits the **float (fp32)**
  budget. Half-precision approximations are usually at least as accurate in ULP terms, so the float
  budget is a safe ceiling.
- **Other or custom formats** (bf16, `CustomFormat(e, m)`). Inherit the **float (fp32)** budget
  unless a descriptor overrides it. Equivalently, `default` in `budgets.py` equals the fp32 column.

## Notes

- **sqrt and reciprocal are held to 0 ULP.** The standard mandates correct rounding, and MPFR's own `sqrt` and division are correctly rounded.
- **sigmoid and softplus have no published standard.** Their budgets are a project default derived from their composition (`sigmoid` = exp + reciprocal, `softplus` = log + exp), and are labelled as such rather than cited.

# Verification

## Running

```bash
bash scripts/setup_verification.sh          # or pass --setup to the runner

python3 -m scripts.verification.run --design hardfloat/FPADD_8_24
python3 -m scripts.verification.run --library openfloat --tier 1
python3 -m scripts.verification.summarize
```

Records land in `verification_results/`, one JSON per (design, rounding mode, tininess).

## Two tiers

| Tier | Reference | Comparison |
|---|---|---|
| 1 | Berkeley SoftFloat via `testfloat_gen` | bit exact, flags included when the DUT has them |
| 2 | MPFR at excess precision | within the descriptor's `ulp_budget` |

Tier says how the expected value is computed. It says nothing about how much of the DUT gets
checked; that is the profile's job.

## Profiles

A profile is the DUT's own claim about what it implements.

```yaml
profile:
  rounding_modes: [rne]
  rounding_control: none        # port | elaboration | none
  exception_flags: none         # ieee5 | none
  subnormals: unknown
  nan_payload: ignored
  signed_zero: ignored
  not_evaluated_reason: No rounding-mode input and no exception-flag output on the module.
```

The comparator masks itself to the claim, and the record carries both the profile and a
`conformance_level` of `strict`, `reduced` or `minimal`. A pass on one row is comparable to a pass
on another only when their profiles match, which is why neither is ever omitted.

A library with no flag port is not failing. It is claiming less.

### Flush to zero

`subnormals: flushed` says the design has no opinion about subnormal operands or results. Rial
sets `disableSubnormal = true` for every format, so holding it to the IEEE answer there measures
nothing. Those vectors are **excluded and counted**, never silently passed:

```json
"checks_performed": 41895,
"vectors_excluded_by_profile": 4569,
"exclusion_reasons": {"subnormal_operand": 3342, "subnormal_result": 1227}
```

The escape hatch is narrow on purpose. It does not excuse a wrong normal result, and it does not
excuse `0 x NaN` returning zero, because no subnormal is involved. Checking flush-to-zero
arithmetic exactly, rather than skipping it, needs a reference driven with flushed inputs. That
is a software model this tier does not have yet.

## Adding a library

1. Add it as a submodule and register a factory in `GenerateAllTestModules.scala` (Chisel only;
   for existing Verilog or VHDL, point the descriptor at the file and skip elaboration).
2. Generate or copy the RTL to `generated/<library>/<stem>.sv`.
3. `python3 scripts/scaffold.py generated/<library>/` writes `descriptors/_locks/<library>__<stem>.json`
   listing every port with direction and width. You do not read the RTL.
4. Write `descriptors/<library>/<op>_<fmt>.yaml`, mapping roles to signals and declaring the profile.
5. `python3 scripts/build_manifest.py`. It will tell you precisely what is wrong.

Known roles: `a b c result flags rounding_mode tininess valid_in ready_in valid_out ready_out select`.
The handshake roles pair by channel: `valid_in`/`ready_in` on the input side, `valid_out`/`ready_out` on the output side (a decoupled ready/valid interface). Tie `ready_out` with `constant: 1` for a fixed-latency pipe that is always consumed.

## Why a wrong descriptor cannot pass quietly

1. **Schema** rejects a malformed YAML.
2. **Binding** rejects a role whose signal is absent, or whose direction or width disagrees with
   the lockfile.
3. **Coverage** rejects any module port that is neither mapped nor in `ignore_ports`, so an
   upstream rename is a build failure rather than an unbound signal.
4. **Profile** rejects `exception_flags: ieee5` with no flag port, and rejects several rounding
   modes with no rounding-mode port. This is the hole worth caring about: a claimed check with
   nothing behind it.
5. **Canary** injects one deliberately wrong expectation into every run and fails if the
   comparator does not catch it. Static checks cannot see a port that exists but is stuck at zero.
   This can.

Plus `checks_performed == vectors_run` and `vectors_run > 0`, because a run that checked nothing
is not a pass.

`tests/test_contract.py` mutates a real descriptor twelve ways and asserts each is rejected.

## Protocols

| Protocol | Used by | How it is driven |
|---|---|---|
| `combinational` | hardfloat arithmetic, Rial | set inputs, settle, sample |
| `fixed_latency` | OpenFloat add and mult | one vector per cycle, results `latency` edges behind |
| `valid_poll` | hardfloat div and sqrt, OpenFloat div and sqrt | wait for accept, strobe valid, wait for result |

`valid_poll` spends 25 to 55 edges per vector, so those descriptors carry a `max_vectors` cap.
Delete the cap for a deep run and bring a book.

## Failure reporting

Mismatches are grouped by the *shape* of the failure, with one exemplar and a count per group.
A flat list of 12,062 failing vectors teaches nothing that one row plus a count does not, and it
would dominate any corpus built from these records.

```json
"mismatch_categories": [
  {"count": 44, "summary": "normal x normal x normal was 1 ulp from the correctly rounded result",
   "kind": "value", "input_classes": ["normal","normal","normal"], "ulp_band": "1",
   "exemplar": {"operands": ["0x3effffec","0x3f800000","0xc0318244"],
                "got": "0xc0118247", "expected": "0xc0118246", "ulp": 1.0}},
  {"count": 21, "summary": "normal x normal x inf returned normal where inf is required", ...},
  {"count": 20, "summary": "normal x normal x inf returned an infinity of the wrong sign", ...}
]
```

The signature is `kind | input classes | got->expected class | ulp band`. Operand order is
preserved, so `nan x zero` and `zero x nan` stay separate; in a non-commutative implementation
they are different bugs. The ULP band keeps a one-ulp rounding slip apart from a wild answer.
The report is capped at 25 categories with `mismatch_categories_omitted` recording the remainder.

## Vectors

`testfloat_gen` output is materialized under `vectors/` with a sidecar recording function,
rounding mode, tininess, level, seed, count and sha256. Runs replay byte for byte. Level 1 is
46,464 vectors for binary ops at every format width, because the structured seed tables are the
same size for all of them. `f32_mulAdd` is 6.1 million, which is what `max_vectors` is for.

Every operand comes from a corner-case lattice: 22 sign-and-exponent seeds covering subnormal,
minimum normal, the precision boundary, the maximum finite exponent and the infinity or NaN
encoding, crossed with significands at zero, one ULP and all-ones. Even the operands TestFloat
calls random are drawn from that lattice rather than uniformly.

**The profile does not affect which vectors are generated.** A reduced-profile DUT gets the same
NaN, infinity and subnormal cases as a strict one; it is judged on less of the answer, not asked
an easier question.

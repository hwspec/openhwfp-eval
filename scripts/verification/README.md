# Verification

Use `make verify` to run the complete flow over every design, or `make verify DESIGN=library/stem` for one. `run.py` picks tier 1 (TestFloat, bit-exact) or tier 2 (MPFR, ULP-bounded) per design; `TIER=1|2` restricts it to a single tier.

## Running manually

```bash
bash scripts/setup_verification.sh          # or pass --setup to the runner

python3 -m scripts.verification.run --design hardfloat/FPADD_8_24
python3 -m scripts.verification.run --library openfloat --tier 1
python3 -m scripts.verification.summarize
```

Records land in `verification_results/`, one JSON per (design x rounding mode x tininess) combination.

## Tiers

| Tier | Reference | Comparison |
|---|---|---|
| 1 | Berkeley SoftFloat via `testfloat_gen` | bit exact, flags included when the DUT has them |
| 2 | MPFR at excess precision | within the descriptor's `ulp_budget` |

Tiers describe how the expected value is computed. This is different than the profile, which constrains much of the DUT gets
checked.

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

### Flush to zero

`subnormals: flushed` says the design has no opinion about subnormal operands or results. E.g. RIAL
sets `disableSubnormal = true` for every format, so holding it to the IEEE standard will still exclude certain vectors. The output produced will look like this as a result:
```json
"checks_performed": 41895,
"vectors_excluded_by_profile": 4569,
"exclusion_reasons": {"subnormal_operand": 3342, "subnormal_result": 1227}
```

## Coherency checks

Before verification is run, we verify the descriptors are syntactically correct, I/O maps are coherent with the RTL, all I/O ports are constrained, profiles are non-contradictory, and that the module reacts to basic tests. 

1. **Schema** rejects a malformed YAML, checked against `descriptors/schema.json`.
2. **Binding** rejects a role whose signal is absent, or whose direction or width disagrees with the lockfile.
3. **Coverage** rejects any module port that is neither mapped nor in `ignore_ports`, so an upstream rename is a build failure rather than an unbound signal.
4. **Profile** rejects `exception_flags: ieee5` with no flag port, and rejects several rounding modes with no rounding-mode port.
5. **Canary** injects one deliberately wrong expectation into every run and fails if the comparator does not catch it.

## Protocols

FP modules typically have three types of protcols, which are reflected in the protcols supported in the descriptors.

| Protocol | Used by | How it is driven |
|---|---|---|
| `combinational` | hardfloat arithmetic, Rial | set inputs, settle, sample |
| `fixed_latency` | OpenFloat add and mult | one vector per cycle, results `latency` edges behind |
| `valid_poll` | hardfloat div and sqrt, OpenFloat div and sqrt | wait for accept, strobe valid, wait for result |

`valid_poll` spends 25 to 55 edges per vector, so those descriptors carry a `max_vectors` cap. You can delete the cap for a deep run, just make sure to bring a book.

## Failure reporting

Mismatches are grouped by the *shape* of the failure, with one exemplar and a count per group.

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
preserved, so `nan x zero` and `zero x nan` stay separate.
The ULP band keeps a one-ulp rounding slip apart from a wild answer.
The report is capped at 25 categories with `mismatch_categories_omitted` recording the remainder.

## Vectors

berkeley-testfloat's `testfloat_gen` output is populated in `vectors/` with a sidecar recording function,
rounding mode, tininess, level, seed, count and sha256; this allows for seeded runs to replay byte for byte.

Every operand comes from a corner-case lattice: 22 sign-and-exponent seeds covering subnormal, minimum normal, the precision boundary, the maximum finite exponent and the infinity or NaN encoding, crossed with significands at zero, one ULP and all-ones.

**The profile does not affect which vectors are generated.** A reduced-profile DUT gets the same NaN, infinity and subnormal cases as a strict one, we simply hold its answers to a lower standard.

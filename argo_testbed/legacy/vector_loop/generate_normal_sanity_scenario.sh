#!/usr/bin/env bash
#
# Generates a deterministic "normal-only" FP32 add scenario.json for sanity checking.
# This avoids NaN/Inf/subnormal/zero inputs so oracle + DUTs should match.
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="${1:-$SCRIPT_DIR/llm_runs/sanity_normal}"
OUT_SCENARIO="$OUT_DIR/scenario.json"

mkdir -p "$OUT_DIR"

cat > "$OUT_SCENARIO" <<'EOF'
{
  "scenario": {
    "targets": ["openfloat", "hardfloat", "rial"],
    "op": "add",
    "fp_format": "fp32",
    "rounding_mode": "nearest_even"
  },
  "vectors": [
    { "a_hex": "0x3f800000", "b_hex": "0x40000000" },
    { "a_hex": "0xbf800000", "b_hex": "0x3f800000" },
    { "a_hex": "0x40600000", "b_hex": "0xc0100000" },
    { "a_hex": "0x40490fdb", "b_hex": "0x402df854" },
    { "a_hex": "0x7f7fffff", "b_hex": "0x3f800000" },
    { "a_hex": "0x00800000", "b_hex": "0x3f800000" },
    { "a_hex": "0x3f000000", "b_hex": "0x3f800000" },
    { "a_hex": "0xc1200000", "b_hex": "0x41200000" }
  ]
}
EOF

echo "Wrote: $OUT_SCENARIO"


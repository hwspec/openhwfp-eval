#!/usr/bin/env bash
# Builds the software reference stack and the Python toolchain
# Run once after cloning with "make setup", but rerunning is safe
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

# default to RISCV specialization
SPECIALIZE_TYPE="${SPECIALIZE_TYPE:-RISCV}"

# Berkeley ships no macOS or ARM build dir but the x86_64 GCC will do
PLATFORM="${PLATFORM:-Linux-x86_64-GCC}"

SOFTFLOAT_BUILD="berkeley-softfloat-3/build/$PLATFORM"
TESTFLOAT_BUILD="berkeley-testfloat-3/build/$PLATFORM"

if [ ! -d "$SOFTFLOAT_BUILD" ]; then
  echo "error: $SOFTFLOAT_BUILD missing. Run: git submodule update --init" >&2
  exit 1
fi

echo "==> building softfloat ($SPECIALIZE_TYPE)"
make -C "$SOFTFLOAT_BUILD" SPECIALIZE_TYPE="$SPECIALIZE_TYPE" -j"$(getconf _NPROCESSORS_ONLN)"

echo "==> building testfloat"
make -C "$TESTFLOAT_BUILD" \
  SPECIALIZE_TYPE="$SPECIALIZE_TYPE" \
  SOFTFLOAT_DIR="$ROOT/berkeley-softfloat-3" \
  PLATFORM="$PLATFORM" \
  -j"$(getconf _NPROCESSORS_ONLN)"

echo "==> python toolchain"
PYTHON="${PYTHON:-python3.13}"
if ! command -v "$PYTHON" >/dev/null 2>&1; then
  echo "error: $PYTHON not found. cocotb doesn't work on python >3.13; set PYTHON=python3.12 or 3.13." >&2
  exit 1
fi
[ -d .venv ] || "$PYTHON" -m venv .venv
.venv/bin/pip install -q --upgrade pip
.venv/bin/pip install -q -r requirements.txt

echo
echo "==> verifying"
GEN="$ROOT/$TESTFLOAT_BUILD/testfloat_gen"
# generate one vector with testfloat_gen
sample="$("$GEN" f32_add | head -1)" || true
[ -n "$sample" ] || { echo "error: testfloat_gen produced no vectors" >&2; exit 1; }
echo "$sample"
.venv/bin/python -c "import cocotb, gmpy2; print('cocotb', cocotb.__version__, '| mpfr', gmpy2.mpfr_version())"

cat <<EOF

Done.

EOF

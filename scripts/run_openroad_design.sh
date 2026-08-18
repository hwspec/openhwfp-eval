#!/usr/bin/env bash
# Run one prepared design through ORFS ASAP7.
#
# Docker (default):
#   bash scripts/run_openroad_design.sh openfloat_FP_add_32_1
#
# Local build (after setup_openroad.sh --local):
#   export ORFS_MODE=local
#   bash scripts/run_openroad_design.sh openfloat_FP_add_32_1
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

NICKNAME="${1:-}"
ORFS_DIR="${ORFS_DIR:-$HOME/OpenROAD-flow-scripts}"
OR_IMAGE="${OR_IMAGE:-openroad/orfs:latest}"
ORFS_MODE="${ORFS_MODE:-docker}"
DOCKER_SHELL="$ORFS_DIR/flow/util/docker_shell"

if [[ -z "$NICKNAME" ]]; then
  echo "Usage: bash scripts/run_openroad_design.sh <nickname>" >&2
  echo "Example: bash scripts/run_openroad_design.sh openfloat_FP_add_32_1" >&2
  exit 1
fi

CFG="orfs_designs/asap7/${NICKNAME}/config.mk"
if [[ ! -f "$CFG" ]]; then
  echo "ERROR: $CFG not found." >&2
  echo "Prepare first, e.g.:" >&2
  echo "  python3 scripts/prepare_orfs_design.py generated/openfloat/FP_add_32_1.sv" >&2
  exit 1
fi

# Prefer a local install if env.sh exists and the user asked for local,
# or if docker is unavailable.
if [[ "$ORFS_MODE" == "local" ]] || [[ "${1:-}" == "--local" ]]; then
  ORFS_MODE="local"
fi
if [[ "$ORFS_MODE" != "local" && -f "$ORFS_DIR/env.sh" && ! -x "$DOCKER_SHELL" ]]; then
  ORFS_MODE="local"
fi

mkdir -p openroad_results

if [[ "$ORFS_MODE" == "local" ]]; then
  if [[ ! -f "$ORFS_DIR/env.sh" ]]; then
    echo "ERROR: $ORFS_DIR/env.sh not found. Run: bash scripts/setup_openroad.sh --local" >&2
    exit 1
  fi
  # shellcheck disable=SC1091
  source "$ORFS_DIR/env.sh"
  echo "Running ORFS ASAP7 for $NICKNAME (local)"
  echo "  yosys=$(command -v yosys)"
  echo "  openroad=$(command -v openroad)"
  echo "  results -> $REPO_ROOT/openroad_results"
  make -C "$ORFS_DIR/flow" \
    WORK_HOME="$REPO_ROOT/openroad_results" \
    OPENHWFP_ROOT="$REPO_ROOT" \
    DESIGN_CONFIG="$REPO_ROOT/$CFG"
else
  if [[ ! -x "$DOCKER_SHELL" ]]; then
    echo "ERROR: docker_shell not found at $DOCKER_SHELL" >&2
    echo "Run: bash scripts/setup_openroad.sh" >&2
    echo "Or build locally: bash scripts/setup_openroad.sh --local" >&2
    exit 1
  fi
  export OR_IMAGE
  echo "Running ORFS ASAP7 for $NICKNAME (docker)"
  echo "  image=$OR_IMAGE"
  echo "  config=/work/$CFG"
  echo "  results -> $REPO_ROOT/openroad_results"
  "$DOCKER_SHELL" make \
    WORK_HOME=/work/openroad_results \
    OPENHWFP_ROOT=/work \
    DESIGN_CONFIG=/work/$CFG
fi

echo
echo "ORFS finished (check logs even if make failed — failures are dataset rows)."
echo "Extract metrics with:"
echo "  python3 scripts/extract_orfs_metrics.py --nickname $NICKNAME"

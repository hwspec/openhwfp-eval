#!/usr/bin/env bash
# Small ASAP7 sweep (paper §7.3 / path to §8.2). Sequential — host is ~8 GB RAM.
#
#   2000 ps FP32 mul × OpenFloat / HardFloat / Rial
#   FP32 add at 1000 ps and 4000 ps (2000 ps add already exists; do not overwrite)
#
# Usage (compute host):
#   source ~/OpenROAD-flow-scripts/env.sh
#   export ORFS_MODE=local
#   bash scripts/run_orfs_sweep.sh
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

export ORFS_MODE="${ORFS_MODE:-local}"
ORFS_DIR="${ORFS_DIR:-$HOME/OpenROAD-flow-scripts}"
LOG="${SWEEP_LOG:-$REPO_ROOT/openroad_results/sweep.log}"
mkdir -p openroad_results
: >"$LOG"

log() { echo "$@" | tee -a "$LOG"; }

if [[ ! -f "$ORFS_DIR/env.sh" ]]; then
  echo "ERROR: $ORFS_DIR/env.sh not found" >&2
  exit 1
fi
# shellcheck disable=SC1091
source "$ORFS_DIR/env.sh"

log "ORFS sweep start $(date -Iseconds)"
log "  yosys=$(command -v yosys)"
log "  openroad=$(command -v openroad)"
free -h | awk 'NR==2 {print "  mem: total="$2" used="$3" avail="$7}' | tee -a "$LOG"

# nickname period sv
JOBS=(
  "openfloat_FP_mult_32_1 2000 generated/openfloat/FP_mult_32_1.sv"
  "hardfloat_FPMUL_8_24 2000 generated/hardfloat/FPMUL_8_24.sv"
  "rial_RialMultFP32 2000 generated/rial/RialMultFP32.sv"
  "openfloat_FP_add_32_1_p1000 1000 generated/openfloat/FP_add_32_1.sv"
  "hardfloat_FPADD_8_24_p1000 1000 generated/hardfloat/FPADD_8_24.sv"
  "rial_RialAddFP32_p1000 1000 generated/rial/RialAddFP32.sv"
  "openfloat_FP_add_32_1_p4000 4000 generated/openfloat/FP_add_32_1.sv"
  "hardfloat_FPADD_8_24_p4000 4000 generated/hardfloat/FPADD_8_24.sv"
  "rial_RialAddFP32_p4000 4000 generated/rial/RialAddFP32.sv"
)

fail=0
i=0
n=${#JOBS[@]}
for job in "${JOBS[@]}"; do
  i=$((i + 1))
  # shellcheck disable=SC2086
  set -- $job
  nick="$1"
  period="$2"
  sv="$3"
  log ""
  log "======== [$i/$n] $nick  period=${period}ps  $(date -Iseconds) ========"
  if [[ ! -f "$sv" ]]; then
    log "ERROR: missing $sv (do not rerun run_ppa_estimation.sh — it deletes generated/)"
    fail=$((fail + 1))
    continue
  fi
  python3 scripts/prepare_orfs_design.py "$sv" --period "$period" --nickname "$nick" 2>&1 | tee -a "$LOG"
  if bash scripts/run_openroad_design.sh "$nick" 2>&1 | tee -a "$LOG"; then
    log "RUN_OK $nick"
  else
    log "RUN_FAIL $nick (keep logs — still a dataset row)"
    fail=$((fail + 1))
  fi
  python3 scripts/extract_orfs_metrics.py --nickname "$nick" \
    --also-jsonl saved_results/phase1-20260817-214624/flow_instances.jsonl 2>&1 | tee -a "$LOG"
done

log ""
log "ORFS sweep done $(date -Iseconds)  prepare/run failures=$fail"
exit 0

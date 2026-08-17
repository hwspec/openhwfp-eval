#!/usr/bin/env bash
#
# NOTE (2026-03):
# This is the legacy "vector scenario smoke-loop" workflow.
# The newer direction is testbench generation via:
#   argo_testbed/generate_llm_testbench.sh
#
# Automates the LLM-assisted verification loop for the current FP32 add harness:
#   1) Ensure smoke1/scenario.json exists (generate if missing)
#   2) Run the Scala harness against the current scenario
#   3) If failures.json was NOT written (no mismatches), stop
#   4) Otherwise generate the next scenario using intent.txt + failures.json
#
# Stop condition:
#   - success when smoke<i>/failures.json does not exist (or mismatch counts are 0)
#
# Usage:
#   export ARGO_USER="svc_..."
#   bash argo_testbed/run_llm_verify_loop.sh [MAX_ITERS]
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MAX_ITERS="${1:-5}"
START_ITER="${2:-}"

if [[ -z "${ARGO_USER:-}" ]]; then
  echo "ERROR: ARGO_USER env var not set (your svc/ANL username)" >&2
  exit 1
fi

VECTORS_COUNT="${VECTORS_COUNT:-8}"
MODEL_ID="${MODEL_ID:-gpt41nano}"

SCALA_TEST='testOnly argo_testbed.ArgoLLMAddFp32ScenarioSpec'

ensure_smoke1() {
  local smoke1_dir="$SCRIPT_DIR/llm_runs/smoke1"
  local scenario_path="$smoke1_dir/scenario.json"
  if [[ ! -f "$scenario_path" ]]; then
    echo "scenario.json missing; generating smoke1 baseline..."
    bash "$SCRIPT_DIR/generate_llm_scenario_smoke.sh"
  else
    echo "Using existing smoke1 scenario: $scenario_path"
  fi
}

find_last_smoke_with_failures() {
  local last=0
  local d
  for d in "$SCRIPT_DIR/llm_runs"/smoke*; do
    [[ -d "$d" ]] || continue
    local base
    base="$(basename "$d")"
    local num="${base#smoke}"
    if [[ "$num" =~ ^[0-9]+$ ]]; then
      if [[ -f "$d/failures.json" ]]; then
        if (( num > last )); then last="$num"; fi
      fi
    fi
  done
  echo "$last"
}

run_harness() {
  local smoke_dir="$1"
  local scenario_path="$smoke_dir/scenario.json"
  local scenario_abs="$scenario_path"

  echo "Running harness on: $scenario_abs"
  (cd "$REPO_ROOT" && sbt "$SCALA_TEST" -Dargo.llm.scenario="$scenario_abs")
}

parse_failure_counts() {
  local failures_path="$1"
  python3 - <<'PY'
import json, os, sys
path = os.environ["FAIL_PATH"]
with open(path, "r", encoding="utf-8") as f:
    obj = json.load(f)
print("oracle_mismatch_count=" + str(obj.get("oracle_mismatch_count", -1)))
print("differential_mismatch_count=" + str(obj.get("differential_mismatch_count", -1)))
PY
}

generate_next() {
  local in_dir="$1"
  local out_dir="$2"
  echo "Generating next scenario from failures..."
  export VECTORS_COUNT MODEL_ID
  bash "$SCRIPT_DIR/generate_llm_scenario_from_failures.sh" "$in_dir" "$out_dir"
}

ensure_smoke1

if [[ -z "$START_ITER" ]]; then
  last_smoke="$(find_last_smoke_with_failures)"
  if (( last_smoke > 0 )); then
    START_ITER="$last_smoke"
  else
    START_ITER=1
  fi
fi

if (( START_ITER > MAX_ITERS )); then
  echo "START_ITER ($START_ITER) > MAX_ITERS ($MAX_ITERS); nothing to do."
  exit 0
fi

echo "Verify loop starting at smoke$START_ITER (MAX_ITERS=$MAX_ITERS)"

for iter in $(seq "$START_ITER" "$MAX_ITERS"); do
  smoke_dir="$SCRIPT_DIR/llm_runs/smoke$iter"
  failures_path="$smoke_dir/failures.json"

  if [[ ! -f "$smoke_dir/scenario.json" ]]; then
    echo "ERROR: Missing scenario.json for smoke$iter: $smoke_dir/scenario.json" >&2
    exit 1
  fi

  run_harness "$smoke_dir"

  if [[ ! -f "$failures_path" ]]; then
    echo "No failures.json written for smoke$iter; stopping (assumed no mismatches)."
    exit 0
  fi

  # Read mismatch counts
  export FAIL_PATH="$failures_path"
  counts="$(python3 - <<'PY'
import json, os
with open(os.environ["FAIL_PATH"],"r",encoding="utf-8") as f:
    o=json.load(f)
print(o.get("oracle_mismatch_count",-1), o.get("differential_mismatch_count",-1))
PY
)"
  oracle_count="$(echo "$counts" | awk '{print $1}')"
  diff_count="$(echo "$counts" | awk '{print $2}')"

  echo "Mismatch counts for smoke$iter: oracle=$oracle_count differential=$diff_count"

  if [[ "$oracle_count" == "0" && "$diff_count" == "0" ]]; then
    echo "Both mismatch counts are 0; stopping."
    exit 0
  fi

  next_iter=$((iter+1))
  next_dir="$SCRIPT_DIR/llm_runs/smoke$next_iter"
  generate_next "$smoke_dir" "$next_dir"
done

echo "Reached MAX_ITERS=$MAX_ITERS without achieving zero mismatches."
exit 1


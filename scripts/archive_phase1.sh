#!/usr/bin/env bash
# Freeze Phase-1 artifacts (Yosys XML/HTML, logs, tool versions, flow-instance records).
# Run from repo root after scripts/run_ppa_estimation.sh
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

STAMP="${1:-$(date +%Y%m%d-%H%M%S)}"
OUT="results/phase1-${STAMP}"
mkdir -p "$OUT"

copy_if() {
  local src="$1"
  if [[ -f "$src" ]]; then
    cp -a "$src" "$OUT/"
    echo "  copied $src"
  else
    echo "  missing $src (skip)"
  fi
}

echo "Archiving Phase-1 results -> $OUT"
copy_if generated/cell_count_report.xml
copy_if generated/ppa_report.html
copy_if yosys_output.log

{
  echo "timestamp: $(date -Iseconds)"
  echo "host: $(hostname)"
  echo "repo: $REPO_ROOT"
  echo "git_head: $(git rev-parse HEAD 2>/dev/null || echo unknown)"
  echo "git_status:"
  git status --short || true
  echo
  echo "submodules:"
  git submodule status || true
  echo
  echo "java:"; java -version 2>&1 || true
  echo "sbt:"; sbt --version 2>&1 | head -n 5 || true
  echo "verilator:"; verilator --version 2>&1 || true
  echo "yosys:"; yosys -V 2>&1 || true
} > "$OUT/environment.txt"
echo "  wrote $OUT/environment.txt"

python3 scripts/export_flow_instances.py \
  --xml generated/cell_count_report.xml \
  --generated generated \
  --out "$OUT/flow_instances.jsonl" \
  --json "$OUT/flow_instances.json"

cp -a "$OUT/flow_instances.jsonl" dataset/flow_instances.jsonl
cp -a "$OUT/flow_instances.json" dataset/flow_instances.json
echo "  also copied records to dataset/"

echo
echo "Done. Keep $OUT (gitignored). Commit dataset/flow_instances.jsonl if you want the records in git."
echo "Next: bash scripts/setup_openroad.sh"

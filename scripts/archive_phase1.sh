#!/usr/bin/env bash
# Freeze Phase-1 artifacts (Yosys XML/HTML, logs, tool versions, flow-instance records).
# Run from repo root after scripts/run_ppa_estimation.sh
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# --no-export: snapshot only, do not regenerate records at all.
EXPORT_RECORDS=1
ARGS=()
for a in "$@"; do
  case "$a" in
    --no-export) EXPORT_RECORDS=0 ;;
    *) ARGS+=("$a") ;;
  esac
done
set -- ${ARGS[@]+"${ARGS[@]}"}

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

impl_count() {
  [[ -f "$1" ]] || { echo 0; return; }
  python3 - "$1" <<'EOF'
import json, sys
n = 0
for line in open(sys.argv[1], encoding="utf-8"):
    if line.strip():
        st = (json.loads(line).get("implementation") or {}).get("status")
        if st not in (None, "", "not_run"):
            n += 1
print(n)
EOF
}

if [[ "$EXPORT_RECORDS" == "1" ]]; then
  # Export into dataset/ FIRST so export_flow_instances.py can merge with what is
  # already there, then snapshot the merged result into $OUT.
  #
  # The old order was the reverse: export into a fresh $OUT (nothing to merge
  # against, so every implementation block came back "not_run") and then cp -a
  # over dataset/. That silently destroyed all recorded OpenROAD results.
  BEFORE="$(impl_count dataset/flow_instances.jsonl)"
  BACKUP=""
  if [[ -f dataset/flow_instances.jsonl ]]; then
    BACKUP="$(mktemp)"
    cp -a dataset/flow_instances.jsonl "$BACKUP"
  fi

  python3 scripts/export_flow_instances.py \
    --xml generated/cell_count_report.xml \
    --generated generated \
    --out dataset/flow_instances.jsonl \
    --json dataset/flow_instances.json

  AFTER="$(impl_count dataset/flow_instances.jsonl)"
  if (( AFTER < BEFORE )); then
    echo "ERROR: export dropped implementation records ($BEFORE -> $AFTER)." >&2
    if [[ -n "$BACKUP" ]]; then
      cp -a "$BACKUP" dataset/flow_instances.jsonl
      echo "ERROR: dataset/flow_instances.jsonl restored from backup. Aborting." >&2
    fi
    exit 1
  fi
  [[ -n "$BACKUP" ]] && rm -f "$BACKUP"
  echo "  records merged into dataset/ ($AFTER implementation record(s) intact)"

  cp -a dataset/flow_instances.jsonl "$OUT/flow_instances.jsonl"
  cp -a dataset/flow_instances.json "$OUT/flow_instances.json"
  echo "  snapshotted records to $OUT/"
else
  echo "  --no-export: dataset/ untouched; snapshotting existing records"
  copy_if dataset/flow_instances.jsonl
  copy_if dataset/flow_instances.json
fi

echo
echo "Done. Keep $OUT (gitignored). Commit dataset/flow_instances.jsonl if you want the records in git."
echo "Next: bash scripts/setup_openroad.sh"

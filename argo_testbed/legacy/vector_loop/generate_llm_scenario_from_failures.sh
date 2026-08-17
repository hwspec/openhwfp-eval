#!/usr/bin/env bash
#
# Generate the next LLM verification scenario using:
# - argo_testbed/intent.txt
# - previous failures.json (produced by the Scala harness)
#
# The output is written to:
#   <out_dir>/scenario.json
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

IN_DIR="${1:-$SCRIPT_DIR/llm_runs/smoke1}"
OUT_DIR="${2:-$SCRIPT_DIR/llm_runs/smoke2}"
VECTORS_COUNT="${VECTORS_COUNT:-8}"
MODEL_ID="${MODEL_ID:-gpt41nano}"

INTENT_PATH="$SCRIPT_DIR/intent.txt"
FAILURES_PATH="$IN_DIR/failures.json"

export SCRIPT_DIR IN_DIR OUT_DIR VECTORS_COUNT MODEL_ID INTENT_PATH FAILURES_PATH

if [[ ! -f "$INTENT_PATH" ]]; then
  echo "ERROR: intent.txt not found: $INTENT_PATH" >&2
  exit 1
fi

if [[ ! -f "$FAILURES_PATH" ]]; then
  echo "ERROR: failures.json not found: $FAILURES_PATH" >&2
  exit 1
fi

if [[ -z "${ARGO_USER:-}" ]]; then
  echo "ERROR: ARGO_USER env var not set (must be your svc/ANL username)." >&2
  exit 1
fi

# If ARGO_URI is not set (or appears to be a local dry-run endpoint), reset it
# to the expected Argonne DEV OpenAI-compatible chat completion endpoint.
if [[ -z "${ARGO_URI:-}" || "${ARGO_URI}" == *"127.0.0.1"* || "${ARGO_URI}" == *"localhost"* ]]; then
  export ARGO_URI="https://apps-dev.inside.anl.gov/argoapi/v1/chat/completions"
fi

mkdir -p "$OUT_DIR"

payload_json="$OUT_DIR/argo_payload.json"
resp_json="$OUT_DIR/argo_response.json"

python3 - <<'PY'
import json, os

script_dir = os.environ["SCRIPT_DIR"]
intent_path = os.environ["INTENT_PATH"]
failures_path = os.environ["FAILURES_PATH"]
in_dir = os.environ["IN_DIR"]
out_dir = os.environ["OUT_DIR"]
vectors_count = int(os.environ["VECTORS_COUNT"])
model_id = os.environ["MODEL_ID"]

with open(intent_path, "r", encoding="utf-8") as f:
    intent = f.read().strip()

with open(failures_path, "r", encoding="utf-8") as f:
    failures = f.read().strip()

def load_json(path):
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh)

previous_scenario_path = os.path.join(in_dir, "scenario.json")
previous_scenario = ""
if os.path.exists(previous_scenario_path):
    with open(previous_scenario_path, "r", encoding="utf-8") as f:
        previous_scenario = f.read().strip()

# History-aware feedback:
# Include mismatch evidence from smoke1..smokeK (where K is the current in_dir).
in_dir_base = os.path.basename(in_dir)  # e.g. "smoke6"
k_str = "".join([c for c in in_dir_base if c.isdigit()])
history_k = int(k_str) if k_str else 1
runs_root = os.path.dirname(in_dir)  # e.g. ".../llm_runs"

history_oracle = []
history_diff = []
history_counts = []
history_oracle_cat_counts = {}
history_diff_cat_counts = {}

def add_first_n(arr, n):
    return arr[:n] if len(arr) > n else arr

for i in range(1, history_k + 1):
    fi = os.path.join(runs_root, f"smoke{i}", "failures.json")
    if not os.path.exists(fi):
        continue
    try:
        obj = load_json(fi)
    except Exception:
        continue
    history_counts.append({
        "smoke": i,
        "oracle_mismatch_count": obj.get("oracle_mismatch_count", None),
        "differential_mismatch_count": obj.get("differential_mismatch_count", None)
    })
    # Store mismatch strings (cap to keep prompt size reasonable)
    history_oracle.extend(obj.get("oracle_mismatches", []))
    history_diff.extend(obj.get("differential_mismatches", []))

    # Category summaries (coarse input classes) so the LLM can focus on the worst bins.
    for k, v in (obj.get("oracle_mismatch_categories", {}) or {}).items():
        try:
            history_oracle_cat_counts[k] = history_oracle_cat_counts.get(k, 0) + int(v)
        except Exception:
            pass
    for k, v in (obj.get("differential_mismatch_categories", {}) or {}).items():
        try:
            history_diff_cat_counts[k] = history_diff_cat_counts.get(k, 0) + int(v)
        except Exception:
            pass

history_oracle = add_first_n(history_oracle, 50)
history_diff = add_first_n(history_diff, 50)

def top_categories(cat_map, topn=5):
    items = []
    for k, v in (cat_map or {}).items():
        try:
            items.append((str(k), int(v)))
        except Exception:
            pass
    items.sort(key=lambda x: x[1], reverse=True)
    return items[:topn]

top_oracle_cats = top_categories(history_oracle_cat_counts, 5)
top_diff_cats = top_categories(history_diff_cat_counts, 5)

# We keep the schema expected by the Scala harness:
# { scenario: { targets, op, fp_format, rounding_mode }, vectors: [ {a_hex, b_hex}, ... ] }
user_prompt = {
  "intent": intent,
  "previous_failures_json": json.loads(failures),
  "history_failures_json": {
    "smoke_up_to": history_k,
    "history_counts": history_counts,
    "top_oracle_categories": top_oracle_cats,
    "top_differential_categories": top_diff_cats,
    "oracle_mismatches_sample": history_oracle,
    "differential_mismatches_sample": history_diff
  },
  "previous_scenario_json": json.loads(previous_scenario) if previous_scenario else None,
  "requirements": {
    "keep_op": "add",
    "keep_fp_format": "fp32",
    "keep_rounding_mode": "nearest_even",
    "vector_schema": {"a_hex": "0xXXXXXXXX", "b_hex": "0xXXXXXXXX"},
    "targets": ["openfloat", "hardfloat", "rial"],
    "vector_count": vectors_count,
    "focus": "Generate vectors that reproduce/extend mismatches observed in both previous_failures_json and history_failures_json, especially for NaN/Inf/subnormal/signed-zero and near-overflow boundaries.",
    "avoid_duplicate_vectors": "Prefer vectors whose (a_hex,b_hex) pairs have NOT appeared in earlier scenario.json files (smoke1..smokeK). Avoid reusing the exact same input pairs."
  }
}

payload = {
  "user": os.environ["ARGO_USER"],
  "model": model_id,
  "messages": [
    {
      "role": "system",
      "content": (
        "You are a directed verification test-vector generator for floating-point hardware. "
        "Return ONLY valid JSON that matches the required schema. "
        "No markdown, no explanations, no trailing commas, no extra keys."
      )
    },
    {
      "role": "user",
      "content": "Generate the next scenario JSON for the FP32 add harness. "
                 "Required top-level keys: scenario, vectors. "
                 "scenario keys: targets, op, fp_format, rounding_mode. "
                 "vectors is an array of length vector_count; each element has a_hex and b_hex."
                 "\n\nRequirements JSON:\n" + json.dumps(user_prompt, indent=2)
    }
  ],
  "temperature": 0,
  "top_p": 0.9,
  "max_tokens": 1000
}

out_payload_path = os.path.join(out_dir, "argo_payload.json")
with open(out_payload_path, "w", encoding="utf-8") as f:
    json.dump(payload, f, indent=2)

print("Wrote", out_payload_path)
PY

MAX_LLM_TRIES="${MAX_LLM_TRIES:-3}"
try_num=1
success=0
while [ "$try_num" -le "$MAX_LLM_TRIES" ]; do
  echo "Argo scenario gen attempt $try_num/$MAX_LLM_TRIES"
  curl -sS -X POST "$ARGO_URI" \
    -H "Authorization: Bearer $ARGO_USER" \
    -H "Content-Type: application/json" \
    --data-binary "@$payload_json" > "$resp_json"

  if python3 - <<'PY'
import json, os
import sys

out_dir = os.environ["OUT_DIR"]
in_dir = os.environ["IN_DIR"]
resp_json = os.path.join(out_dir, "argo_response.json")
payload_json = os.path.join(out_dir, "argo_payload.json")

def load_json(path):
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh)

resp = load_json(resp_json)
content = resp["choices"][0]["message"]["content"]
obj = json.loads(content)

vectors_count = int(os.environ["VECTORS_COUNT"])
if "scenario" not in obj or "vectors" not in obj:
    raise SystemExit("ERROR: LLM output missing required keys scenario/vectors")

scenario = obj["scenario"]
for k in ("targets", "op", "fp_format", "rounding_mode"):
    if k not in scenario:
        raise SystemExit(f"ERROR: scenario missing key {k}")

vectors = obj["vectors"]
if not isinstance(vectors, list) or len(vectors) != vectors_count:
    raise SystemExit(f"ERROR: expected {vectors_count} vectors, got {len(vectors)}")

for i,v in enumerate(vectors):
    for k in ("a_hex","b_hex"):
        if k not in v:
            raise SystemExit(f"ERROR: vector {i} missing {k}")

#
# Novelty enforcement: require at least ~25% of vectors to be new
# vs any previous smoke*/scenario.json up to the current IN_DIR smokeK.
#
in_dir_base = os.path.basename(in_dir)  # e.g. "smoke6"
k_str = "".join([c for c in in_dir_base if c.isdigit()])
history_k = int(k_str) if k_str else 1
runs_root = os.path.dirname(in_dir)      # e.g. ".../llm_runs"

used_pairs = set()
for i in range(1, history_k + 1):
    sp = os.path.join(runs_root, f"smoke{i}", "scenario.json")
    if not os.path.exists(sp):
        continue
    try:
        sobj = load_json(sp)
        for vv in sobj.get("vectors", []):
            a = vv.get("a_hex")
            b = vv.get("b_hex")
            if isinstance(a, str) and isinstance(b, str):
                used_pairs.add((a, b))
    except Exception:
        continue

new_vectors = []
for v in vectors:
    pair = (v["a_hex"], v["b_hex"])
    if pair not in used_pairs:
        new_vectors.append(v)

min_new_env = os.environ.get("MIN_NEW_VECTORS", "").strip()
if min_new_env != "":
    # If set, allow you to relax the de-dup constraint to avoid the generator getting stuck.
    # Example: MIN_NEW_VECTORS=1 for VECTORS_COUNT=8.
    min_new = max(0, int(min_new_env))
else:
    min_new = max(1, vectors_count // 4)  # at least 25% new, but never less than 1
if len(new_vectors) < min_new:
    print(f"WARN: generated too many duplicate vectors (new={len(new_vectors)} min_new={min_new}). Retrying...")
    sys.exit(2)

out_scenario_path = os.path.join(out_dir, "scenario.json")
with open(out_scenario_path, "w", encoding="utf-8") as f:
    json.dump(obj, f, indent=2)

print("Wrote", out_scenario_path)
print("vectors:", len(vectors))
print("new_vectors:", len(new_vectors))
PY
  then
    success=1
    break
  fi
  try_num=$((try_num+1))
done

if [ "$success" -ne 1 ]; then
  echo "ERROR: failed to generate a sufficiently novel scenario after $MAX_LLM_TRIES attempts" >&2
  exit 1
fi

echo "Done. Next run command:"
echo "  sbt -no-colors \"testOnly argo_testbed.ArgoLLMAddFp32ScenarioSpec\" -Dargo.llm.scenario=$OUT_DIR/scenario.json"


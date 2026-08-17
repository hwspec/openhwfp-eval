export ARGO_USER="svc_v80agent"
export ARGO_URI="https://apps-dev.inside.anl.gov/argoapi/v1/chat/completions"

set -euo pipefail

# NOTE (2026-03):
# Legacy vector-scenario smoke generation script.
# Kept for reproducibility against historical smoke runs.
# New primary flow: argo_testbed/generate_llm_testbench.sh

# Always write outputs relative to this script's location (not the current cwd).
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUT_DIR="$SCRIPT_DIR/llm_runs/smoke1"
mkdir -p "$OUT_DIR"
export OUT_PATH="$OUT_DIR/scenario.json"

resp="$(curl -sS -X POST "$ARGO_URI" \
  -H "Authorization: Bearer $ARGO_USER" \
  -H "Content-Type: application/json" \
  -d "{\"user\":\"$ARGO_USER\",\"model\":\"gpt41nano\",\"messages\":[{\"role\":\"system\",\"content\":\"You are a test-vector generator for floating-point hardware verification. Return ONLY valid JSON; no markdown; no extra keys.\"},{\"role\":\"user\",\"content\":\"Generate a JSON object exactly like: {\\\"scenario\\\": {\\\"targets\\\":[\\\"openfloat\\\",\\\"hardfloat\\\",\\\"rial\\\"], \\\"op\\\":\\\"add\\\", \\\"fp_format\\\":\\\"fp32\\\", \\\"rounding_mode\\\":\\\"nearest_even\\\"}, \\\"vectors\\\":[{\\\"a_hex\\\":\\\"0x????????\\\",\\\"b_hex\\\":\\\"0x????????\\\"}]} . Include exactly 8 vectors focusing on NaN/Inf/subnormal/signed-zero/near-overflow.\"}],\"temperature\":0,\"top_p\":0.9,\"max_tokens\":500}")"

export RESP="$resp"

python3 - <<'PY'
import json, os
r = json.loads(os.environ["RESP"])
s = r["choices"][0]["message"]["content"]
obj = json.loads(s)
assert "scenario" in obj and "vectors" in obj
assert len(obj["vectors"]) == 8, len(obj["vectors"])
out_path = os.environ["OUT_PATH"]
with open(out_path, "w", encoding="utf-8") as f:
    json.dump(obj, f, indent=2)
print("Wrote", out_path)
print("vectors:", len(obj["vectors"]))
PY

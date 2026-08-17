#!/usr/bin/env bash
#
# Generate a Scala/Chisel testbench using Argo (OpenAI-compatible endpoint).
# - Reads a natural-language intent from argo_testbed/user_chat.txt (fallback: intent_testbench.txt)
# - Calls Argo (model=gpt52 by default) to generate a ScalaTest spec as JSON
# - Writes the Scala code into src/test/scala/generated/<TestClassName>.scala
#
# Usage:
#   export ARGO_USER="svc_yourservice"
#   # optional:
#   # export MODEL_ID="gpt52"
#   bash argo_testbed/generate_llm_testbench.sh
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

INTENT_TB_PATH="$SCRIPT_DIR/intent_testbench.txt"
USER_CHAT_PATH="${USER_CHAT_PATH:-$SCRIPT_DIR/user_chat.txt}"
USER_FEEDBACK_PATH="${USER_FEEDBACK_PATH:-$SCRIPT_DIR/user_feedback.txt}"
OUT_DIR="$SCRIPT_DIR/generated_testbenches"
MODEL_ID="${MODEL_ID:-gpt52}"
MAX_TOKENS="${MAX_TOKENS:-4096}"
MAX_LLM_TRIES="${MAX_LLM_TRIES:-3}"
FEEDBACK_LOOP="${FEEDBACK_LOOP:-0}"          # set to 1 to enable: generate -> run sbt -> feed failure back -> regenerate
MAX_FEEDBACK_ITERS="${MAX_FEEDBACK_ITERS:-3}" # how many feedback regeneration attempts (on failing tests)
APPEND_SBT_TO_USER_CHAT="${APPEND_SBT_TO_USER_CHAT:-1}" # when FEEDBACK_LOOP=1, append sbt evidence to user_chat.txt
SBT_TEE_TO_CONSOLE="${SBT_TEE_TO_CONSOLE:-1}"         # when FEEDBACK_LOOP=1, stream sbt output to console

# Normalize common env-var formatting issues (e.g. "1 " from copy/paste, or accidental CRLF).
APPEND_SBT_TO_USER_CHAT="$(echo "${APPEND_SBT_TO_USER_CHAT}" | tr -d '[:space:]')"
case "${APPEND_SBT_TO_USER_CHAT,,}" in
  true) APPEND_SBT_TO_USER_CHAT="1" ;;
  false) APPEND_SBT_TO_USER_CHAT="0" ;;
esac

if [[ -z "${ARGO_USER:-}" ]]; then
  echo "ERROR: ARGO_USER env var not set (must be your ANL username or service account, e.g. svc_v80agent)." >&2
  exit 1
fi

# Use OpenAI-compatible dev endpoint (Argo-native)
if [[ -z "${ARGO_URI:-}" ]]; then
  export ARGO_URI="https://apps-dev.inside.anl.gov/argoapi/v1/chat/completions"
fi

# Prefer `user_chat.txt` when present; otherwise prefer `user_feedback.txt`.
# If neither is present, fall back to `intent_testbench.txt` for backward compatibility.
if [[ -s "$USER_CHAT_PATH" ]]; then
  INTENT_TB_PATH="$USER_CHAT_PATH"
elif [[ -s "$USER_FEEDBACK_PATH" ]]; then
  INTENT_TB_PATH="$USER_FEEDBACK_PATH"
fi

# Ensure USER_CHAT_PATH always points at the active chat/evidence file
# (so appending evidence writes to the same file you used as input).
if [[ "$INTENT_TB_PATH" != "$USER_CHAT_PATH" ]]; then
  USER_CHAT_PATH="$INTENT_TB_PATH"
fi

if [[ ! -f "$INTENT_TB_PATH" ]]; then
  cat >&2 <<EOF
ERROR: No input file found.

Tried:
  - $USER_CHAT_PATH
  - $USER_FEEDBACK_PATH
  - $INTENT_TB_PATH

Create `user_chat.txt` or `user_feedback.txt` (recommended) with your goal and any appended fixes.
For example:
  - write: "THE GOAL: Generate a FP32 ADD testbench for OpenFloat/Hardfloat/Rial ..."
EOF
  exit 1
fi

mkdir -p "$OUT_DIR"

PAYLOAD_JSON="$OUT_DIR/argo_testbench_payload.json"
RESP_JSON="$OUT_DIR/argo_testbench_response.json"

export SCRIPT_DIR REPO_ROOT INTENT_TB_PATH OUT_DIR MODEL_ID PAYLOAD_JSON RESP_JSON USER_CHAT_PATH USER_FEEDBACK_PATH
export MAX_TOKENS MAX_LLM_TRIES FEEDBACK_LOOP MAX_FEEDBACK_ITERS
export APPEND_SBT_TO_USER_CHAT
export USER_CHAT_PATH

python3 - <<'PY'
import json, os, textwrap

script_dir = os.environ["SCRIPT_DIR"]
intent_path = os.environ["INTENT_TB_PATH"]
out_dir = os.environ["OUT_DIR"]
model_id = os.environ["MODEL_ID"]
max_tokens = int(os.environ.get("MAX_TOKENS", "4096"))

with open(intent_path, "r", encoding="utf-8") as f:
    intent = f.read().strip()

# If the selected input file is empty (e.g., user_chat contains only whitespace),
# fall back to user_feedback.txt, and then to intent_testbench.txt.
if not intent:
    uf = os.environ.get("USER_FEEDBACK_PATH", "")
    if uf and os.path.exists(uf):
        with open(uf, "r", encoding="utf-8", errors="ignore") as f2:
            intent = f2.read().strip()
if not intent:
    fallback_path = os.path.join(script_dir, "intent_testbench.txt")
    if os.path.exists(fallback_path):
        with open(fallback_path, "r", encoding="utf-8", errors="ignore") as f3:
            intent = f3.read().strip()

# If we already generated a testbench previously, keep the class name/package stable.
last_meta_path = os.path.join(out_dir, "last_generated_meta.json")
last_test_class_name = ""
last_file_package = ""
if os.path.exists(last_meta_path):
    try:
        with open(last_meta_path, "r", encoding="utf-8") as mf:
            m = json.load(mf)
            last_test_class_name = str(m.get("test_class_name", "")).strip()
            last_file_package = str(m.get("file_package", "")).strip()
    except Exception:
        pass

repo_root = os.environ["REPO_ROOT"]
include_ref_specs = os.environ.get("INCLUDE_REF_SPECS", "1") == "1"

def read_excerpt(path, markers, max_chars):
    try:
        txt = open(path, "r", encoding="utf-8", errors="ignore").read()
    except Exception:
        return ""
    best_idx = None
    best_marker = None
    for m in markers:
        i = txt.find(m)
        if i != -1 and (best_idx is None or i < best_idx):
            best_idx = i
            best_marker = m
    if best_idx is None:
        return ""
    start = max(0, best_idx - 800)
    end = min(len(txt), best_idx + max_chars)
    chunk = txt[start:end]
    return f"\n--- excerpt from {os.path.basename(path)} (marker: {best_marker!r}) ---\n{chunk}\n"

ref_block = ""
if include_ref_specs:
    openfloat_path = os.path.join(repo_root, "src/test/scala/OpenFloatSpec.scala")
    hardfloat_path = os.path.join(repo_root, "src/test/scala/HardfloatSpec.scala")
    rial_path = os.path.join(repo_root, "src/test/scala/RialSpec.scala")
    crosslib_path = os.path.join(repo_root, "src/test/scala/generated/FP32AddCrossLibSpec.scala")
    crosslib_excerpt = read_excerpt(crosslib_path, ["class FP32AddCrossLibSpec"], 2200) if os.path.exists(crosslib_path) else ""
    ref_block = (
        "REFERENCE KNOWN-GOOD TESTS (trimmed excerpts; follow these APIs/signals):\n"
        + read_excerpt(openfloat_path, ["object OpenFloatTolerance", "simulate(new FP_add"], 2400)
        + read_excerpt(hardfloat_path, ["def testFPOPTest", "simulate(new FPOPTest"], 2000)
        + read_excerpt(rial_path, ["simulate(new AddFPGeneric", "dut.io.x.poke", "dut.io.z.peek"], 2000)
        + crosslib_excerpt
    )

user_prompt = textwrap.dedent(f"""
You are an expert Chisel/Scala verification engineer.

Repository context (high-level):
- This repo contains open-source floating-point hardware libraries:
  - OpenFloat:
    - The Scala module `FP_add` is defined in package `FloatingPoint.fpu` (see existing tests in `src/test/scala/OpenFloatSpec.scala`).
    - In Scala/Chisel, import with `import FloatingPoint.fpu._` (or `import FloatingPoint.fpu.FP_add`) and instantiate with `new FP_add(bw, pd)`, e.g. `new FP_add(32, 1)` or `new FP_add(32, pd=1)`.
    - Expected IO signals (as used in this repo): `io.in_a`, `io.in_b`, `io.in_en`, `io.in_valid`, `io.out_s`, `io.out_valid`.
  - Berkeley Hardfloat:
    - Wrapper `FPOPTest(8, 24, FPOPTestMode.ADD)` converting IEEE-754 to recFN (see `src/test/scala/HardfloatSpec.scala`).
    - IO naming (as used there): `dut.io.in_a`, `dut.io.in_b`, `dut.io.out`.
  - Rial:
    - `AddFPGeneric` for Float32 addition using `dut.io.x`, `dut.io.y`, `dut.io.z` (see `src/test/scala/RialSpec.scala`).
    - Typically requires `dut.clock.step()` before sampling `dut.io.z.peek()`.
- ScalaTest + chisel3.simulator.scalatest.ChiselSim are used for testbenches.

Your job:
- Read the user's TESTBENCH_INTENT below.
- Propose a single ScalaTest spec (one class) that can be compiled into this repo.
- The spec MUST test ALL THREE DUTs (OpenFloat, Hardfloat, Rial) in the same ScalaTest class:
  - OpenFloat: `new FP_add(32, 1)` from `FloatingPoint.fpu`
  - Hardfloat: `new FPOPTest(8, 24, FPOPTestMode.ADD)`
  - Rial: `new AddFPGeneric(Float32Spec, Float32Spec, Float32Spec, RoundSpec.roundToEven, PipelineStageConfig.none)` (or equivalent to how it appears in `RialSpec.scala`)
- Use the same input vectors for all three DUTs.
- Start with NORMAL-FINITE FP32 additions only:
  - no NaN, no Inf, no signed-zero, no subnormals
  - avoid large cancellations (e.g. a ~= -b)
  - keep vector count small (8-12 pairs)
- OpenFloat DUT sequencing rules (MUST match the reference spec style):
  - drive `in_en` = true for the whole simulation
  - drive `in_valid` = true for each cycle of inputs
  - assume pipeline delay `pd = 1`
  - sample `out_s` for output index `cycles - pd` (use a `cycles` counter) when `cycles >= pd`
  - do NOT rely on `out_valid` for correctness; OpenFloat may not assert it exactly as you expect
  - after the input loop, deassert `in_valid` and flush `pd` cycles using the same sampling rule
- Rial DUT rules (MUST avoid ScalaUtil bit/slice conversions):
  - for conversions use direct bit packing:
    - `fp32BitsToBigInt(bits:Int): BigInt = BigInt(bits & 0xffffffffL)`
    - `bigIntToFp32Bits(b: BigInt): Int = (b & BigInt("ffffffff", 16)).toInt`
  - poke `dut.io.x` and `dut.io.y` with those BigInt values
  - call `dut.clock.step()` then sample `dut.io.z.peek().litValue`
  - do NOT use `rial.util.ScalaUtil.bit`/`slice` or RealGeneric bit-field decomposition
- The generated code MUST include capped debug printing:
  - define `val debugPrint: Boolean = true`
  - define `val debugMax: Int = 6` (or similar small number)
  - print the first `debugMax` vectors and computed oracle vs DUT results for each DUT
  - if a check fails, include `aBits/bBits/expBits/gotBits` in the assertion message
- Assertion messages / ScalaTest syntax:
  - Do NOT use invalid ScalaTest syntax like `gotBits shouldBe expBits withClue ("...")`.
  - If you want a custom message, use `assert(condition, "message")` OR `withClue("message"){{ /* assertions */ }}` (correct parentheses/braces).
  - Prefer plain `gotBits shouldBe expBits` when a message isn't required.
- Use the same software oracle `oracleAddBits(aBits,bBits)` using Java Float.
- Comparison policy (MUST):
  - NaN: if expected is NaN, accept any NaN output.
  - Signed zero: if expected is +/-0.0, accept any +/-0.0 output.
  - Otherwise:
    - OpenFloat: compare numerically using `openfloat.OpenFloatTolerance.nearlyEqual(dutDouble, refDouble)` (convert FP32 bits to `Float` then to `Double`).
    - Hardfloat and Rial: compare against the oracle `Float` value.
- Instantiate each DUT in its own `simulate(new <DUT>) {{ dut => ... }}` block inside the same spec.
- Use these imports (or equivalent):
  - `import FloatingPoint.fpu._`
  - `import hardfloat._`
  - `import rial.arith.AddFPGeneric`
  - `import rial.arith.RealSpec`
  - `import rial.arith.RoundSpec`
  - `import rial.util.PipelineStageConfig`
- Be self-contained: it should compile with no extra helper files.

TESTBENCH_INTENT (from user):
\"\"\"{intent}\"\"\"

Important repo-specific behavior:
- OpenFloat arithmetic may not be bit-exact for all cancellation corner-cases across all implementations.
- Therefore, prefer tolerance-based comparisons using `openfloat.OpenFloatTolerance.nearlyEqual(dut: Double, ref: Double)` (see `src/test/scala/OpenFloatSpec.scala`) rather than strict bit equality for all non-NaN cases.
  - Convert FP32 bit patterns to `Float`, then to `Double`, then call `nearlyEqual(...)`.
- Do NOT call non-existent helper APIs such as `OpenFloatTolerance.withinTolerance` or `OpenFloatTolerance.isClose`; only use `OpenFloatTolerance.nearlyEqual`.
- Treat +0.0 and -0.0 as equivalent when the expected mathematical result is zero (signed-zero differences are implementation-dependent).

{ref_block}

Return ONLY valid JSON with this exact schema (no extra keys, no markdown):
{{
  "test_class_name": "CamelCaseNameLikeThis",
  "file_package": "generated",
  "scala_test_code": "FULL_SCALATEST_SOURCE_HERE"
}}

Constraints:
- scala_test_code must be a complete .scala file:
  - package <file_package>
  - imports
  - class <test_class_name> extends AnyFlatSpec with ChiselSim with Matchers (or similar)
  - at least one \"should\" test body that runs the DUT and asserts correctness.
- Use only libraries already in this repo: chisel3, chisel3.simulator.scalatest.ChiselSim, ScalaTest.
""").strip()

payload = {
    "user": os.environ["ARGO_USER"],
    "model": model_id,
    "messages": [
        {
            "role": "system",
            "content": (
                "You are an expert Scala/Chisel verification engineer. "
                "Return ONLY valid JSON as requested, no markdown, no explanations."
            ),
        },
        {
            "role": "user",
            "content": user_prompt,
        },
    ],
    "temperature": 0,
    "top_p": 0.9,
    "max_tokens": max_tokens,
}

payload_path = os.path.join(out_dir, "argo_testbench_payload.json")
with open(payload_path, "w", encoding="utf-8") as f:
    json.dump(payload, f, indent=2)

print("Wrote", payload_path)
PY


try_num=1
success=0
while [ "$try_num" -le "$MAX_LLM_TRIES" ]; do
  echo "Argo testbench gen attempt $try_num/$MAX_LLM_TRIES (max_tokens=$MAX_TOKENS) ..."

  http_code="$(
    curl -sS -o "$RESP_JSON" -w "%{http_code}" -X POST "$ARGO_URI" \
      -H "Authorization: Bearer $ARGO_USER" \
      -H "Content-Type: application/json" \
      --data-binary "@$PAYLOAD_JSON"
  )"

  if [[ "$http_code" != "200" ]]; then
    echo "ERROR: Argo HTTP status $http_code (see $RESP_JSON for body)." >&2
    exit 1
  fi

  if python3 - <<'PY'
import json, os
from pathlib import Path

out_dir = os.environ["OUT_DIR"]
resp_json = os.path.join(out_dir, "argo_testbench_response.json")
repo_root = os.environ["REPO_ROOT"]

with open(resp_json, "r", encoding="utf-8") as f:
    resp = json.load(f)

choices = resp.get("choices") or []
if not choices:
    raise SystemExit(f"ERROR: unexpected Argo response: no choices\n{resp}")

choice0 = choices[0] or {}
finish_reason = choice0.get("finish_reason", None)
msg = choice0.get("message") or {}
content = msg.get("content", None)

if content is None:
    raise SystemExit(f"ERROR: unexpected Argo response structure: message.content missing\n{resp}")

content_str = content.strip()
if not content_str:
    # This commonly happens when the model fails to emit a final answer (e.g. max_tokens too small).
    raise SystemExit(
        "ERROR: LLM returned empty message.content "
        f"(finish_reason={finish_reason})."
    )

try:
    obj = json.loads(content_str)
except Exception as e:
    # Print a short preview to help debugging prompt/token issues.
    preview = content_str[:200]
    raise SystemExit(f"ERROR: LLM content is not valid JSON: {e}\ncontent_preview={preview!r}")

for key in ("test_class_name", "file_package", "scala_test_code"):
    if key not in obj:
        raise SystemExit(f"ERROR: missing key '{key}' in LLM JSON output")

test_class_name = str(obj["test_class_name"]).strip()
file_package = str(obj["file_package"]).strip()
scala_code = obj["scala_test_code"]

if not test_class_name or any(c.isspace() for c in test_class_name):
    raise SystemExit(f"ERROR: invalid test_class_name: {test_class_name!r}")
if not file_package:
    raise SystemExit("ERROR: empty file_package")

if not isinstance(scala_code, str) or not scala_code.strip():
    raise SystemExit("ERROR: scala_test_code missing/empty in LLM JSON output")

# Guardrails: fix common repo-specific OpenFloat import mistakes.
# (The LLM may hallucinate `openfloat.FP_add`, but in this repo `FP_add` is under `FloatingPoint.fpu`.)
scala_code = scala_code.replace("import openfloat.FP_add", "import FloatingPoint.fpu.FP_add")
scala_code = scala_code.replace("openfloat.FP_add", "FloatingPoint.fpu.FP_add")

rel_dir = Path("src/test/scala") / file_package.replace(".", "/")
abs_dir = Path(repo_root) / rel_dir
abs_dir.mkdir(parents=True, exist_ok=True)

out_path = abs_dir / f"{test_class_name}.scala"
with open(out_path, "w", encoding="utf-8") as f:
    f.write(scala_code)

print(f"Wrote Scala testbench: {out_path}")
print(f"To run it:\n  sbt \"testOnly {file_package}.{test_class_name}\"")

# Write metadata for optional feedback loop.
meta_path = os.path.join(out_dir, "last_generated_meta.json")
with open(meta_path, "w", encoding="utf-8") as mf:
    json.dump(
        {"test_class_name": test_class_name, "file_package": file_package, "out_path": str(out_path)},
        mf,
        indent=2,
    )
PY
  then
    success=1
    break
  fi

  try_num=$((try_num+1))
done

if [[ "$success" -ne 1 ]]; then
  echo "ERROR: failed to generate a valid Scala testbench after $MAX_LLM_TRIES attempts." >&2
  echo "Inspect: $RESP_JSON" >&2
  exit 1
fi

if [[ "${FEEDBACK_LOOP}" == "1" ]]; then
  # Load the generated class/package so we can run it and enforce stability across iterations.
  meta_path="$OUT_DIR/last_generated_meta.json"
  if [[ ! -f "$meta_path" ]]; then
    echo "ERROR: missing feedback meta file: $meta_path" >&2
    exit 1
  fi

  test_class_name="$(
    META_PATH="$meta_path" python3 - <<'PY'
import json, os
m = json.load(open(os.environ["META_PATH"], "r", encoding="utf-8"))
print(m["test_class_name"])
PY
  )"
  file_package="$(
    META_PATH="$meta_path" python3 - <<'PY'
import json, os
m = json.load(open(os.environ["META_PATH"], "r", encoding="utf-8"))
print(m["file_package"])
PY
  )"

  echo "Feedback loop enabled: targeting ${file_package}.${test_class_name}"

  for fb_iter in $(seq 1 "$MAX_FEEDBACK_ITERS"); do
    sbt_log="$OUT_DIR/sbt_test_output_fb${fb_iter}.txt"
    echo "Running sbt testOnly (feedback iter $fb_iter/$MAX_FEEDBACK_ITERS) ..."

    # Run sbt but capture output; don't let `set -e` abort the feedback loop.
    set +e
    if [[ "${SBT_TEE_TO_CONSOLE}" == "1" ]]; then
      sbt "testOnly ${file_package}.${test_class_name}" 2>&1 | tee "$sbt_log"
      sbt_rc=${PIPESTATUS[0]}
    else
      sbt "testOnly ${file_package}.${test_class_name}" >"$sbt_log" 2>&1
      sbt_rc=$?
    fi
    set -e

    if [[ "$sbt_rc" -eq 0 ]]; then
      echo "SUCCESS: test passed at feedback iter $fb_iter."
      if [[ "${APPEND_SBT_TO_USER_CHAT}" == "1" ]]; then
        # Append a short sbt tail to the chat even on success (useful for history).
        success_evidence_path="$OUT_DIR/feedback_evidence_success_fb${fb_iter}.txt"
        SBT_LOG="$sbt_log" EVIDENCE_PATH="$success_evidence_path" python3 - <<'PY'
import os
src = os.environ["SBT_LOG"]
out = os.environ["EVIDENCE_PATH"]
with open(src, "r", encoding="utf-8", errors="ignore") as f:
    txt = f.read()
lines = txt.splitlines()
snippet = "\n".join(lines[-200:])
with open(out, "w", encoding="utf-8") as ff:
    ff.write(snippet[:12000])
PY

        USER_CHAT_PATH="$USER_CHAT_PATH" EVIDENCE_PATH="$success_evidence_path" FB_ITER="$fb_iter" python3 - <<'PY'
import os
chat_path = os.environ["USER_CHAT_PATH"]
evidence_path = os.environ["EVIDENCE_PATH"]
fb_iter = os.environ["FB_ITER"]

os.makedirs(os.path.dirname(chat_path), exist_ok=True)
if not os.path.exists(chat_path):
    open(chat_path, "w", encoding="utf-8").close()

with open(evidence_path, "r", encoding="utf-8", errors="ignore") as f:
    evidence = f.read().strip()

with open(chat_path, "a", encoding="utf-8") as f:
    f.write("\n\n" + "=" * 20 + f"\nSBT SUCCESS ITER {fb_iter}\n" + "=" * 20 + "\n")
    f.write(evidence + "\n")
print(f"APPEND_OK success chat_path={chat_path} evidence_chars={len(evidence)}")
PY
      else
        echo "NOTE: skipping sbt evidence append (APPEND_SBT_TO_USER_CHAT=${APPEND_SBT_TO_USER_CHAT}, USER_CHAT_PATH=$USER_CHAT_PATH)" >&2
      fi
      exit 0
    fi

    # Summarize evidence to keep the next prompt smaller.
    evidence_path="$OUT_DIR/feedback_evidence_fb${fb_iter}.txt"
    SBT_LOG="$sbt_log" EVIDENCE_PATH="$evidence_path" python3 - <<'PY'
import os
src = os.environ["SBT_LOG"]
out = os.environ["EVIDENCE_PATH"]
with open(src, "r", encoding="utf-8", errors="ignore") as f:
    txt = f.read()

# Capture the tail of the sbt output so we include both compile errors
# (`[error] ...`) and test failures (`Vector #...` mismatch evidence).
lines = txt.splitlines()
snippet = "\n".join(lines[-400:])
with open(out, "w", encoding="utf-8") as f:
    f.write(snippet[:12000])
PY

    if [[ "${APPEND_SBT_TO_USER_CHAT}" == "1" ]]; then
      USER_CHAT_PATH="$USER_CHAT_PATH" EVIDENCE_PATH="$evidence_path" FB_ITER="$fb_iter" python3 - <<'PY'
import os
chat_path = os.environ["USER_CHAT_PATH"]
evidence_path = os.environ["EVIDENCE_PATH"]
fb_iter = os.environ["FB_ITER"]

os.makedirs(os.path.dirname(chat_path), exist_ok=True)
if not os.path.exists(chat_path):
    open(chat_path, "w", encoding="utf-8").close()

with open(evidence_path, "r", encoding="utf-8", errors="ignore") as f:
    evidence = f.read().strip()

with open(chat_path, "a", encoding="utf-8") as f:
    f.write("\n\n" + "=" * 20 + f"\nSBT FAILURE ITER {fb_iter}\n" + "=" * 20 + "\n")
    f.write(evidence + "\n")
print(f"APPEND_OK failure chat_path={chat_path} evidence_chars={len(evidence)}")
PY
    else
      echo "NOTE: skipping sbt evidence append (APPEND_SBT_TO_USER_CHAT=${APPEND_SBT_TO_USER_CHAT}, USER_CHAT_PATH=$USER_CHAT_PATH)" >&2
    fi

    # Read current Scala code so the LLM can edit it.
    current_scala_path="$OUT_DIR/last_generated_meta.json"
    current_scala_abs="$(
      META_PATH="$current_scala_path" python3 - <<'PY'
import json, os
m = json.load(open(os.environ["META_PATH"], "r", encoding="utf-8"))
print(m["out_path"])
PY
    )"
    current_scala_abs_escaped="$current_scala_abs"

    echo "Regenerating Scala test code using sbt evidence ..."

    OUT_DIR="$OUT_DIR" FB_ITER="$fb_iter" CURRENT_SCALA_ABS="$current_scala_abs" \
      INTENT_TB_PATH="$INTENT_TB_PATH" ARGO_USER="$ARGO_USER" MODEL_ID="$MODEL_ID" \
      TARGET_TEST_CLASS_NAME="$test_class_name" TARGET_FILE_PACKAGE="$file_package" \
      python3 - <<'PY'
import json, os, textwrap

intent_path = os.environ["INTENT_TB_PATH"]
with open(intent_path, "r", encoding="utf-8") as f:
    intent = f.read().strip()

evidence_path = os.path.join(os.environ["OUT_DIR"], f"feedback_evidence_fb{os.environ['FB_ITER']}.txt")
with open(evidence_path, "r", encoding="utf-8", errors="ignore") as f:
    evidence = f.read().strip()

with open(os.environ["CURRENT_SCALA_ABS"], "r", encoding="utf-8", errors="ignore") as f:
    current_scala = f.read()

test_class_name = os.environ["TARGET_TEST_CLASS_NAME"]
file_package = os.environ["TARGET_FILE_PACKAGE"]

user_prompt = textwrap.dedent(f"""
You are an expert Chisel/Scala verification engineer helping to make an auto-generated testbench PASS in this repo.

Repository context:
- The repo contains three OpenFloat/Hardfloat/Rial FP32 adders:
  - OpenFloat: `FloatingPoint.fpu.FP_add(32, pd=1)`
  - Hardfloat: `hardfloat.FPOPTest(8, 24, FPOPTestMode.ADD)`
  - Rial: `rial.arith.AddFPGeneric(Float32Spec, ..., RoundSpec.roundToEven, PipelineStageConfig.none)` (or matching how `RialSpec.scala` instantiates it)
- OpenFloat comparison:
  - use `openfloat.OpenFloatTolerance.nearlyEqual(dutDouble, refDouble)` after converting FP32 bits -> `Float` -> `Double`.
  - Do NOT call non-existent helper APIs such as `OpenFloatTolerance.withinTolerance` or `OpenFloatTolerance.isClose`; only use `OpenFloatTolerance.nearlyEqual`.
- OpenFloat DUT sequencing rules (MUST match reference spec):
  - drive `in_en` true for whole simulation
  - drive `in_valid` true for each input cycle
  - assume `pd = 1`
  - sample `out_s` for output index `cycles - pd` when `cycles >= pd`
  - do NOT rely on `out_valid` for correctness
- Rial DUT rules (MUST avoid ScalaUtil bit/slice):
  - conversions use direct bit packing with `fp32BitsToBigInt` / `bigIntToFp32Bits` as in reference
  - poke `dut.io.x` and `dut.io.y` and step once, then sample `dut.io.z.peek().litValue`
  - do NOT use `rial.util.ScalaUtil.bit`/`slice` or RealGeneric decomposition
- Hardfloat/Rial comparison:
  - handle NaN-ness and signed-zero equivalence; otherwise compare against the same Java Float oracle.
- Do NOT call non-existent Chisel/ChiselSim APIs like `dut.clock.setTimeout`; only use `dut.clock.step(...)` and `peek()/poke()`.
- Treat +0.0 and -0.0 as equivalent when expected is zero.

Original test intent:
\"\"\"{intent}\"\"\"

SBT failure evidence (most relevant excerpt):
\"\"\"{evidence}\"\"\"

Current Scala test code (edit it to address the failing cases):
\"\"\"{current_scala}\"\"\"

Update requirements (MUST follow):
1. Return ONLY valid JSON with this exact schema (no extra keys, no markdown):
{{
  "test_class_name": "{test_class_name}",
  "file_package": "{file_package}",
  "scala_test_code": "FULL_UPDATED_SCALATEST_SOURCE_HERE"
}}
2. Keep the same class name and package exactly as above.
3. Fix the specific assertion/polling/comparison logic causing the mismatches (do not change the DUT interface signals).
4. Prefer tolerance-based comparison for OpenFloat results and signed-zero equivalence rules.
5. Keep `mkVectors()` NORMAL-FINITE only (same policy as initial prompt) and keep debug printing enabled/capped.
6. Use the correct imports for each DUT (OpenFloat: `FloatingPoint.fpu._`, Hardfloat: `hardfloat._`, Rial: `rial.arith.AddFPGeneric` + `rial.arith.RealSpec` + `rial.arith.RoundSpec` and `rial.util.PipelineStageConfig`).
7. Do NOT generate the invalid ScalaTest syntax `X shouldBe Y withClue (...)`.
   - For custom messages: use `assert(condition, "message")` or the correct wrapper form `withClue("message"){{ ... }}`.
""").strip()

payload = {
    "user": os.environ["ARGO_USER"],
    "model": os.environ["MODEL_ID"],
    "messages": [
        {
            "role": "system",
            "content": (
                "You are an expert Scala/Chisel verification engineer. "
                "Return ONLY valid JSON as requested, no markdown, no explanations."
            ),
        },
        {"role": "user", "content": user_prompt},
    ],
    "temperature": 0,
    "top_p": 0.9,
    "max_tokens": int(os.environ.get("MAX_TOKENS", "4096")),
}

payload_path = os.path.join(os.environ["OUT_DIR"], f"argo_testbench_payload_fb{os.environ['FB_ITER']}.json")
resp_json = os.path.join(os.environ["OUT_DIR"], f"argo_testbench_response_fb{os.environ['FB_ITER']}.json")
with open(payload_path, "w", encoding="utf-8") as f:
    json.dump(payload, f, indent=2)

print("Wrote", payload_path)
print(resp_json)
PY
    export OUT_DIR="${OUT_DIR}" FB_ITER="$fb_iter" INTENT_TB_PATH="${INTENT_TB_PATH}" MODEL_ID="${MODEL_ID}" MAX_TOKENS="${MAX_TOKENS}" \
      CURRENT_SCALA_ABS="$current_scala_abs" TARGET_TEST_CLASS_NAME="$test_class_name" TARGET_FILE_PACKAGE="$file_package" \
      ARGO_USER="$ARGO_USER"

    resp_json_path="$OUT_DIR/argo_testbench_response_fb${fb_iter}.json"
    payload_json_path="$OUT_DIR/argo_testbench_payload_fb${fb_iter}.json"

    curl -sS -X POST "$ARGO_URI" \
      -H "Authorization: Bearer $ARGO_USER" \
      -H "Content-Type: application/json" \
      --data-binary "@$payload_json_path" > "$resp_json_path"

    python3 - <<'PY'
import json, os
from pathlib import Path

out_dir = os.environ["OUT_DIR"]
resp_json = os.path.join(out_dir, f"argo_testbench_response_fb{os.environ['FB_ITER']}.json")
repo_root = os.environ["REPO_ROOT"]

with open(resp_json, "r", encoding="utf-8") as f:
    resp = json.load(f)

choices = resp.get("choices") or []
if not choices:
    raise SystemExit(f"ERROR: unexpected Argo response: no choices\n{resp}")

content = (choices[0].get("message") or {}).get("content", None)
if not content or not str(content).strip():
    raise SystemExit(f"ERROR: LLM returned empty content in feedback iter {os.environ['FB_ITER']}.")

obj = json.loads(str(content).strip())
for key in ("test_class_name", "file_package", "scala_test_code"):
    if key not in obj:
        raise SystemExit(f"ERROR: missing key '{key}' in LLM JSON output")

test_class_name = str(obj["test_class_name"]).strip()
file_package = str(obj["file_package"]).strip()
scala_code = obj["scala_test_code"]

if test_class_name != os.environ["TARGET_TEST_CLASS_NAME"] or file_package != os.environ["TARGET_FILE_PACKAGE"]:
    raise SystemExit(
        f"ERROR: feedback LLM changed target class/package. "
        f"expected={os.environ['TARGET_FILE_PACKAGE']}.{os.environ['TARGET_TEST_CLASS_NAME']} "
        f"got={file_package}.{test_class_name}"
    )

if not isinstance(scala_code, str) or not scala_code.strip():
    raise SystemExit("ERROR: scala_test_code missing/empty in LLM JSON output")

scala_code = scala_code.replace("import openfloat.FP_add", "import FloatingPoint.fpu.FP_add")
scala_code = scala_code.replace("openfloat.FP_add", "FloatingPoint.fpu.FP_add")

rel_dir = Path("src/test/scala") / file_package.replace(".", "/")
abs_dir = Path(repo_root) / rel_dir
abs_dir.mkdir(parents=True, exist_ok=True)

out_path = abs_dir / f"{test_class_name}.scala"
with open(out_path, "w", encoding="utf-8") as f:
    f.write(scala_code)

with open(os.path.join(out_dir, "last_generated_meta.json"), "w", encoding="utf-8") as mf:
    json.dump({"test_class_name": test_class_name, "file_package": file_package, "out_path": str(out_path)}, mf, indent=2)

print("Wrote updated Scala testbench:", out_path)
PY

  done

  echo "ERROR: feedback loop exhausted without a passing test. Latest sbt output: $OUT_DIR/sbt_test_output_fb${MAX_FEEDBACK_ITERS}.txt" >&2
  exit 1
fi


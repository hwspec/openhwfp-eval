# Argo Testbed Workflows

This folder now has two distinct workflows:

## 1) New Direction: Generate Scala/Chisel testbenches from NL intent

Use this for your current goal (Cursor-like flow):

- `intent_testbench.txt`: natural-language requirements for a generated testbench.
- `generate_llm_testbench.sh`: calls Argo and writes a Scala test class under `src/test/scala/generated/`.

Quick start:

```bash
export ARGO_USER="svc_v80agent"
export MODEL_ID="gpt52"   # optional, defaults to gpt52
bash argo_testbed/generate_llm_testbench.sh
```

Then run the generated test:

```bash
sbt "testOnly generated.<GeneratedClassName>"
```

## 2) Legacy Direction: LLM generates vector scenarios (smoke loop)

Legacy vector-loop assets were moved to:

- `legacy/vector_loop/`

Inside that folder, scripts are still available for historical/benchmark use:

- `legacy/vector_loop/generate_llm_scenario_smoke.sh`
- `legacy/vector_loop/generate_llm_scenario_from_failures.sh`
- `legacy/vector_loop/run_llm_verify_loop.sh`
- `legacy/vector_loop/llm_runs/smoke*/...` outputs

This flow generates input vectors (`scenario.json`) and re-runs the fixed harness
(`ArgoLLMAddFp32ScenarioSpec`) iteratively. Keep it if you want comparison
against previous experiments, but it is not required for the new testbench-gen flow.

## Should legacy scripts be deleted?

Not required. Recommendation:

- Keep them for now (reproducibility and baseline comparisons).
- Remove `legacy/vector_loop` only once you no longer need historical comparisons.


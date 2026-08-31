SHELL := /bin/bash
.DEFAULT_GOAL := help

# Prefer the project venv once it exists; fall back so `make setup` works before it does.
PYTHON := $(shell [ -x .venv/bin/python ] && echo .venv/bin/python || echo python3)

PLATFORM        ?= Linux-x86_64-GCC
SPECIALIZE_TYPE ?= RISCV
SOFTFLOAT_BUILD := berkeley-softfloat-3/build/$(PLATFORM)
TESTFLOAT_BUILD := berkeley-testfloat-3/build/$(PLATFORM)

.PHONY: help setup plan manifest rtl locks verify summary test check ppa \
        clean clean-vectors clean-sim extraclean

help:
	@echo "openhwfp-eval"
	@echo
	@echo "  setup          build SoftFloat + TestFloat, create .venv"
	@echo "  plan           schema-check descriptors, write the elaboration plan"
	@echo "  rtl            elaborate every design in the plan to generated/"
	@echo "  locks          regenerate port lockfiles in descriptors/_locks (fail if changed)"
	@echo "  manifest       validate descriptors against lockfiles, write the manifest"
	@echo "  verify         run tier 1 verification over every design"
	@echo "  summary        print the coverage matrix from existing records"
	@echo "  test           run unit tests and check descriptor YAML syntax"
	@echo "  check          run entire frontend: plan -> rtl -> locks -> manifest -> test"
	@echo
	@echo "  ppa            Yosys cell counts and the HTML report (destroys generated/)"
	@echo
	@echo "  clean          remove generated RTL, records, sim builds, dataset exports"
	@echo "  clean-vectors  remove the cached TestFloat vector files"
	@echo "  clean-sim      remove Verilator build trees only"
	@echo "  extraclean     clean + vectors + .venv + native builds + sbt output"
	@echo
	@echo "  PLATFORM=$(PLATFORM)  SPECIALIZE_TYPE=$(SPECIALIZE_TYPE)"

# ---------------------------------------------------------------- build

setup:
	bash scripts/setup_verification.sh

# When onboarding a brand new design, we need skeleton manifest so the elaborator can run, just for its first pass
# After the first time, _locks/ should be committed and stay as an artifact to detect when RTL drifts from the descriptor contract
plan:
	$(PYTHON) scripts/build_manifest.py --plan-only

rtl: plan
	sbt -batch "runMain Generate.GenerateAllTestModules"

manifest:
	$(PYTHON) scripts/build_manifest.py

locks:
	$(PYTHON) scripts/scaffold.py generated/ --quiet
	@git diff --quiet --exit-code descriptors/_locks/ \
	  || { echo "error: port lockfiles changed since last run. Review the diff, then update the descriptors."; exit 1; }
	@echo "port lockfiles match the elaborated RTL"

# ---------------------------------------------------------------- run

verify:
	$(PYTHON) -m scripts.verification.run --tier 1

summary:
	$(PYTHON) -m scripts.verification.summarize

test:
	$(PYTHON) scripts/build_manifest.py --check
	$(PYTHON) -m pytest tests/ -q

check: rtl locks manifest test
	@echo
	@echo "frontend is consistent: descriptors, RTL and lockfiles are in sync"

ppa:
	bash scripts/run_ppa_estimation.sh

# ---------------------------------------------------------------- clean

clean:
	@echo "removing generated RTL, verification records, sim builds, dataset exports"
	rm -rf generated verification_results sim_build
	rm -f dataset/flow_instances.jsonl dataset/flow_instances.json results.xml yosys_output.log
	find . -name '__pycache__' -type d \
	  -not -path './berkeley-*' -not -path './OpenFloat/*' -not -path './rial-tmpfix/*' \
	  -exec rm -rf {} + 2>/dev/null || true

clean-sim:
	rm -rf sim_build

clean-vectors:
	@echo "removing cached TestFloat vectors"
	rm -rf vectors

extraclean: clean clean-vectors
	@echo "removing .venv, native builds and sbt output"
	rm -rf .venv target project/target project/project
	@# Both packages ship a clean target that also removes their executables.
	-$(MAKE) -C $(SOFTFLOAT_BUILD) clean >/dev/null
	-$(MAKE) -C $(TESTFLOAT_BUILD) clean >/dev/null

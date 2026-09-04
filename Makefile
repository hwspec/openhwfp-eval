SHELL := /bin/bash
.DEFAULT_GOAL := help

# Prefer the project venv once it exists
PYTHON := $(shell [ -x .venv/bin/python ] && echo .venv/bin/python || echo python3)

# DESIGN=library/stem targets one design across verify/ppa/impl/all;
# leaving it empty targets all designs
DESIGN  ?=
PERIOD  ?= 2000
TIER    ?=
CONFIRM ?=
_LIB  = $(word 1,$(subst /, ,$(DESIGN)))
_STEM = $(word 2,$(subst /, ,$(DESIGN)))
_SV   = generated/$(_LIB)/$(_STEM).sv
_NICK = $(_LIB)_$(_STEM)

PLATFORM        ?= Linux-x86_64-GCC
SPECIALIZE_TYPE ?= RISCV
SOFTFLOAT_BUILD := berkeley-softfloat-3/build/$(PLATFORM)
TESTFLOAT_BUILD := berkeley-testfloat-3/build/$(PLATFORM)

.PHONY: help setup plan manifest rtl locks locks-check locks-update verify summary selftest build ppa impl all \
        clean clean-vectors clean-sim clean-dataset extraclean

help:
	@echo "openhwfp-eval"
	@echo
	@echo "  setup          build SoftFloat + TestFloat, create .venv"
	@echo "  plan           schema-check descriptors, write the elaboration plan"
	@echo "  rtl            elaborate every design in the plan to generated/"
	@echo "  locks          compares the RTL-extracted port map against the lockfile, writes lockfile if non-existent"
	@echo "  locks-check    write nothing, fail when lockfile and RTL aren't coherent, or a lockfile doesn't exist yet; good for CI-checks"
	@echo "  locks-update   accept current RTL as the new truth and rewrite every lockfile"
	@echo "  manifest       compares descriptor against the lockfiles, then compiles manifest from descriptors to prepare for verification"
	@echo "  verify         run verification (both tiers) over every design"
	@echo "  summary        print the coverage matrix from existing records"
	@echo "  selftest       run unit tests, YAML schema syntactic correctness, and descriptor vs.lockfile checks"
	@echo "  build          run entire frontend: plan -> rtl -> locks -> manifest -> selftest"
	@echo
	@echo "  ppa            Yosys cell counts and the HTML report (destroys generated/)"
	@echo "  impl           OpenROAD ASAP7 implementation (needs ORFS)"
	@echo "  all            one design (or all) through verify -> ppa -> impl"
	@echo
	@echo "  To tune design knobs (verify/ppa/impl/all):"
	@echo "    DESIGN=library/stem   run one design, e.g. DESIGN=hardfloat/FPADD_8_24"
	@echo "    PERIOD=<ps>           impl clock period (default $(PERIOD))"
	@echo "    TIER=1|2              verify: restrict to one tier"
	@echo "    CONFIRM=1             all: skip the confirmation prompt"
	@echo
	@echo "  clean          remove generated RTL, records, sim builds (keeps dataset/flow_instances.jsonl)"
	@echo "  clean-vectors  remove the cached TestFloat vector files"
	@echo "  clean-sim      remove Verilator build trees only"
	@echo "  clean-dataset  blank the flow-instance dataset for a fresh run"
	@echo "  extraclean     clean + vectors + dataset + .venv + native builds + sbt output"
	@echo
	@echo "  PLATFORM=$(PLATFORM)  SPECIALIZE_TYPE=$(SPECIALIZE_TYPE)"

# ---------------------------------------------------------------- build

setup:
	bash scripts/setup_verification.sh

# Develops an elaboration plan from descriptors
plan:
	$(PYTHON) scripts/build_manifest.py --plan-only

rtl: plan
	sbt -batch "runMain Generate.GenerateAllTestModules"

# Compares the RTL-extracted port map against the lockfile
# Writes lockfile if non-existent
locks:
	$(PYTHON) scripts/scaffold.py generated/ --quiet

# Write nothing, fail when lockfile and RTL aren't coherent, or a lockfile doesn't exist yet
# Good for CI-checks
locks-check:
	$(PYTHON) scripts/scaffold.py generated/ --check --quiet

# Accept current RTL as the new truth and rewrite every lockfile
locks-update:
	$(PYTHON) scripts/scaffold.py generated/ --update --quiet

# Compares descriptor against the lockfiles, then compiles manifest from descriptors to prepare for verification
manifest:
	$(PYTHON) scripts/build_manifest.py

# Unit tests, YAML schema syntactic correctness, and descriptor vs. lockfile checks
selftest:
	$(PYTHON) scripts/build_manifest.py --check
	$(PYTHON) -m pytest tests/ -q

build: rtl locks manifest selftest
	@echo
	@echo "Frontend is consistent: descriptors, RTL and lockfiles are in sync."
	@echo "Next, run make verify to simulate test vectors."

# ---------------------------------------------------------------- run

# run.py handles picking tier 1 (testfloat) or tier 2 (MPFR) per design.
# DESIGN=library/stem runs one design
# TIER=1|2 restricts to a tier
verify:
	$(PYTHON) -m scripts.verification.run $(if $(DESIGN),--design $(DESIGN)) $(if $(TIER),--tier $(TIER))

summary:
	$(PYTHON) -m scripts.verification.summarize

ppa:
	DESIGN="$(DESIGN)" bash scripts/run_ppa_estimation.sh

# OpenROAD ASAP7 physical implementation
impl:
ifeq ($(strip $(DESIGN)),)
	bash scripts/run_orfs_sweep.sh
else
	$(PYTHON) scripts/prepare_orfs_design.py $(_SV) --period $(PERIOD) --nickname $(_NICK)
	bash scripts/run_openroad_design.sh $(_NICK)
	$(PYTHON) scripts/extract_orfs_metrics.py --nickname $(_NICK)
endif

# Whole flow: verify -> ppa -> impl
# Prompts before running unless CONFIRM=1 (or a non-Y/y answer aborts)
all:
	@if [ -n "$(strip $(DESIGN))" ]; then n=1; else \
	   n=$$($(PYTHON) -c "import json;print(len(json.load(open('generated/descriptor_manifest.json'))['designs']))"); fi; \
	 echo "make all: $$n design(s) through verify + ppa + impl."; \
	 [ -z "$(strip $(DESIGN))" ] && echo "  (no DESIGN: impl runs the run_orfs_sweep.sh job list, not all $$n)"; \
	 if [ "$(CONFIRM)" != "1" ]; then read -p "Proceed? [Y/n] " a; case "$$a" in ""|Y|y) ;; *) echo aborted; exit 1;; esac; fi; \
	 $(MAKE) verify DESIGN=$(DESIGN) && $(MAKE) ppa DESIGN=$(DESIGN) && $(MAKE) impl DESIGN=$(DESIGN) PERIOD=$(PERIOD)

# ---------------------------------------------------------------- clean

clean:
	@echo "removing generated RTL, verification records, sim builds (keeps dataset/flow_instances.jsonl)"
	rm -rf generated verification_results sim_build
	rm -f results.xml yosys_output.log
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

clean-dataset:
	@echo "removing the flow-instance dataset"
	rm -f dataset/flow_instances.jsonl dataset/flow_instances.json

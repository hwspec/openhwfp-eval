=======================

This repository is under construction.

It will include testbenches and use-case examples for open-source hardware floating-point libraries, along with scripts to perform area estimation using OpenROAD and to generate a web-based report.

Contents will be migrated from our private repository shortly.

# Setup

## Cloning 

You can clone this repository **with all submodules** in one step (note `--shallow-submodules`; multiple libraries have unnecessary nested submodules):

```bash
git clone --recurse-submodules --shallow-submodules https://github.com/hwspec/openhwfp-eval.git
```
or, if you already cloned the repo w/o submodules, initialize and update them with (again note `--recursive`):
```bash
git submodule update --init --recursive
```

## Dependencies

### JDK 8 or newer

We recommend LTS releases Java 8 and Java 11. You can install the JDK as recommended by your operating system, or use the prebuilt binaries from [AdoptOpenJDK](https://adoptopenjdk.net/).

### SBT

SBT is the most common built tool in the Scala community. You can download it [here](https://www.scala-sbt.org/index.html).  


Official sbt Debian Package Setup
```
sudo apt update
sudo apt install openjdk-17-jdk
```
``` 
   echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
   echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
   curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
   sudo apt-get update
   
   sudo apt-get install sbt

   sbt --version
```
   
### Verilator
Start clean
```
make distclean || true
```

Tested with v5.010. Below is a local build instruction:

```bash
git clone https://github.com/verilator/verilator.git && cd verilator
git checkout tags/v5.010  -b v5.010build
autoconf
./configure --prefix=__INSTALLDIR__  # replace __INSTALLDIR__
make
make install
```
________________________________________________________________________________________________________
If using Verilator 5.020 2024-01-01 rev (Debian 5.020-1) you may run into a a thread pool cleanup bug
where ChiselSim calls Verilator with -j 0 (use all threads) if this is the case, v5.044 will fix this!!! 
(*when using v5.044 the test FP_COS from openFloat will hang due to a width error in the source code ***Implementing fix***)
```
#Dependencies needed:
sudo apt install -y \
  git autoconf automake libtool \
  make g++ flex bison \
  libfl-dev zlib1g-dev \
  help2man

# Clone and Checkout
git clone https://github.com/verilator/verilator.git
cd verilator

git checkout tags/v5.044 -b v5.044build

# Build and Install 
autoconf
./configure --prefix=$HOME/verilator-5.044
make -j$(nproc)
make install

# Add to PATH 
echo 'export PATH=$HOME/verilator-5.044/bin:$PATH' >> ~/.bashrc
source ~/.bashrc

#Verify 
which verilator
verilator --version
```
________________________________________________________________________________________________________

### Python Setup

Run `make setup` the first time you run the repo to build dependencies necessary for the verification flow (e.g. testfloat, various Python libs in `requirements.txt`).

## Adding a library

1. Add it as a Git submodule and register a factory in `GenerateAllTestModules.scala` (Chisel only;
   for existing Verilog or VHDL, point the descriptor at the file and skip elaboration).
2. Generate or copy the RTL to `generated/<library>/<stem>.sv` (`make rtl` elaborates the  Chisel factories).
3. `make locks` writes `descriptors/_locks/<library>__<stem>.json` listing every port with direction and width.
4. Write `descriptors/<library>/<op>_<fmt>.yaml`, mapping roles to signals and declaring the profile.
5. `make manifest` runs final coherency checks and builds a manifest to prepare for dataset generation.

Known roles: `a b c result flags rounding_mode tininess valid_in ready_in valid_out ready_out select`.
The handshake roles pair by channel: `valid_in`/`ready_in` on the input side, `valid_out`/`ready_out` on the output side (a decoupled ready/valid interface). Tie `ready_out` with `constant: 1` for a fixed-latency pipe that is always consumed.

# Running

## Architectural overview

This repo utilizes a descriptor-centric approach to streamline external library integration into the dataset (it may be helpful to look at the YAMLs in `descriptors/` for examples). When the dataset generation flow is run, each module's parameters and configuration as defined in the descriptor YAML is:

1. Verified against the **profile** declared in the descriptor,
2. Synthesized and run through Yosys for PPA evaluation, and 
3. Implemented and run through OpenROAD (ASAP7) for further evaluation.

## Generating the dataset from scratch

Dataset generation is primarily automated via the `Makefile`; you should not have to run individual scripts in `scripts/` manually.
The default flow has 4 stages. To go from hardware design to results in `dataset/flow_instances.jsonl` run the following commands in the repo root:
```bash
make build
make verify
make ppa
make impl
```
Or, alternatively, `make all`. 
To run just one design, you can add `DESIGN=lib/stem` to the end of any command above, e.g. `make verify DESIGN=openfloat/FP_add_32_1`.
The commands above are detailed in the below sections which dive into each part of the flow.

#### 1. Build: Elaboration and coherence checks

`make build`

This step takes each descriptor and elaborates the corresponding hardware module either from Chisel to SV, or simply uses the SV in the library. One important feature of this flow are the lockfiles stored in the `descriptors/_locks` folder. These are automatically generated from the RTL port mapping (via `verilator --json-only`) when a new library is initially onboarded; thereafter the lockfiles should be committed to Git are used as a contract to ensure the elaborated I/O map is coherent with the descriptor's I/O map.

#### 2. Verification: Generated RTL + berkeley-testfloat/GPFR --> cocotb + verilator

`make verify`

This step takes the descriptor's profile declaration, e.g. 
```yaml
profile:
  rounding_modes:
  - rne
  - rtz
  - rdn
  - rup
  - rna
  - rto
  rounding_control: port
  exception_flags: ieee5
  tininess:
  - before
  - after
  subnormals: supported
  nan_payload: canonical
  signed_zero: ieee
```
and generates the corresponding test vector suite. There are two tiers of verification: tier 1 is bit-exact for non-transcendental functions and FP16, FP32, or FP64 via berkeley-testfloat, and tier 2 is testing with ULP bounds for transcendentals and other FP formats against the GNU MPFR reference library. Read more about the verification strategy in `scripts/verification/README.md`.

#### 3. PPA: Yosys (generic cell-count / Phase-1 PPA)

`make ppa`

Or, alternatively, to run each step manually then store results separately:

```bash
sudo apt install -y yosys
bash scripts/run_ppa_estimation.sh
bash scripts/archive_phase1.sh
```

This writes Verilog under `generated/`, Yosys cell counts, `generated/ppa_report.html`, and flow-instance records in `dataset/flow_instances.jsonl`. Archive copies XML/HTML/logs/records into `results/phase1-<timestamp>/`.

Those cell counts are generic Yosys estimates, not routed ASAP7 area.

#### 4. Implementation: OpenROAD / ASAP7 (physical implementation)

`make impl`

This stage requires OpenROAD-flow-scripts on the host.

**Docker is the default** (pull a prebuilt image). **Local source build** works on Ubuntu 24.04 but takes 1-3 hours and needs `sudo ./setup.sh`. Skip Bazel; that path is for OpenROAD developers.

```bash
# Docker (recommended unless you cannot use Docker)
bash scripts/setup_openroad.sh

# or local build
bash scripts/setup_openroad.sh --local
```

Then, with Verilog already in `generated/`:

```bash
python3 scripts/prepare_orfs_design.py generated/openfloat/FP_add_32_1.sv --period 2000
# if you built locally:
export ORFS_MODE=local
bash scripts/run_openroad_design.sh openfloat_FP_add_32_1
python3 scripts/extract_orfs_metrics.py --nickname openfloat_FP_add_32_1
```

`setup_openroad.sh` clones OpenROAD-flow-scripts and runs the ASAP7 `gcd` smoke test. Start with one FP32 adder; a failed route or timing miss is still a valid dataset row. After a local build, always `source ~/OpenROAD-flow-scripts/env.sh` so you do not pick up Ubuntu’s Yosys 0.33.

See `orfs_designs/README.md` for the small add/mul sweep.

## Targeting one design (`DESIGN=`)

Every stage runs over every design by default, or over one design when you pass
`DESIGN=library/stem`. This is the same identifier `dataset/flow_instances.jsonl` will later use as `design_id`. From it:

- RTL source: `generated/<library>/<stem>.sv`
- ORFS nickname: `<library>_<stem>` (the `orfs_designs/` and `saved_results/orfs_*` directory name)

List the valid ids:

```bash
.venv/bin/python -c "import json;[print(d['design_id']) for d in json.load(open('generated/descriptor_manifest.json'))['designs']]"
```

Example usage:
```bash
make verify DESIGN=hardfloat/FPADD_8_24              # both tiers auto-selected per design
make verify DESIGN=rial/RialExpFP32 TIER=2           # restrict to a tier if you want
make ppa    DESIGN=hardfloat/FPADD_8_24              # Yosys synthesis --> update/inserts a row
make impl   DESIGN=hardfloat/FPADD_8_24 PERIOD=2000  # OpenROAD ASAP7
make all    DESIGN=hardfloat/FPADD_8_24 CONFIRM=1    # verify -> ppa -> impl, CONFIRM=1 for no confirmation prompt
```

- `ppa DESIGN=...` synthesizes just that design into a scoped XML (`generated/cell_count_<lib>_<stem>.xml`) and update/inserts (upserts!) its `synthesis` row, leaving every other row, and that row's existing `implementation` data, untouched `make ppa` with no `DESIGN` runs the full sweep.
- `impl DESIGN=...` runs `prepare_orfs_design.py` -> `run_openroad_design.sh` -> `extract_orfs_metrics.py` for that one design at `PERIOD` ps. With no `DESIGN` it falls back to the `scripts/run_orfs_sweep.sh` batch.
- `make all` prints how many designs it will run. It will ask you for confirmation (press Enter / `y`), or set `CONFIRM=1` to skip the prompt (needed in non-interactive shells). It runs verify -> ppa -> **impl last**, because the full ppa rebuild creates a stub for  `implementation: not_run`.

## Keeping and reproducing runs

- To keep a run, since `dataset/` is untracked, move the JSON-lines file into `saved_results/` (tracked by Git). On a fresh checkout with no `dataset/flow_instances.jsonl`, the impl stage bootstraps the dataset from the newest `saved_results/*/flow_instances.jsonl` (`seed_jsonl` in `extract_orfs_metrics.py`).

If you have any question please contact:
Kazutomo Yoshii <kazutomo@anl.gov> 
Connor Bohannon <cbohannon@anl.gov>


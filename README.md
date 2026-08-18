=======================

This repository is under construction.

It will include testbenches and use-case examples for open-source hardware floating-point libraries, along with scripts to perform area estimation using OpenROAD and to generate a web-based report.

Contents will be migrated from our private repository shortly.

### Cloning the Repository with Submodules

You can clone this repository **with all submodules** in one step:

```bash
git clone --recurse-submodules https://github.com/hwspec/openhwfp-eval.git
```
or, if you alreadt cloned the repo w/o submodules, initialize and update them with:
```bash
git submodule update --init --recursive
```

### Dependencies

#### JDK 8 or newer

We recommend LTS releases Java 8 and Java 11. You can install the JDK as recommended by your operating system, or use the prebuilt binaries from [AdoptOpenJDK](https://adoptopenjdk.net/).

#### SBT

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
   
#### Verilator
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

### To run tests

```bash
$ sbt test
```

#### Yosys (generic cell-count / Phase-1 PPA)

```bash
sudo apt install -y yosys
bash scripts/run_ppa_estimation.sh
bash scripts/archive_phase1.sh
```

This writes Verilog under `generated/`, Yosys cell counts, `generated/ppa_report.html`, and flow-instance records in `dataset/flow_instances.jsonl`. Archive copies XML/HTML/logs/records into `results/phase1-<timestamp>/`.

Those cell counts are **generic Yosys estimates**, not routed ASAP7 area.

#### OpenROAD / ASAP7 (physical implementation)

**Docker is the default** (pull a prebuilt image). **Local source build** works on Ubuntu 24.04 but takes 1–3 hours and needs `sudo ./setup.sh`. Skip Bazel; that path is for OpenROAD developers.

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

If you have any question please contact:
Kazutomo Yoshii <kazutomo@anl.gov> 
Connor Bohannon <cbohannon@anl.gov>


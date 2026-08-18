# ORFS / ASAP7 designs

This folder is filled by `scripts/prepare_orfs_design.py`. Generated `src/` and `asap7/` trees are gitignored.

## First physical run (one design)

From the repo root, after Yosys Verilog exists in `generated/`:

```bash
bash scripts/setup_openroad.sh
python3 scripts/prepare_orfs_design.py generated/openfloat/FP_add_32_1.sv --period 2000
bash scripts/run_openroad_design.sh openfloat_FP_add_32_1
python3 scripts/extract_orfs_metrics.py --nickname openfloat_FP_add_32_1
```

Clock period is in ASAP7 picoseconds. `2000` is a first-pass target (~500 MHz), not a claim about closure.

## Small sweep (after the first design works)

```bash
for src in \
  generated/openfloat/FP_add_32_1.sv \
  generated/hardfloat/FPADD_8_24.sv \
  generated/rial/RialAddFP32.sv \
  generated/openfloat/FP_mult_32_1.sv \
  generated/hardfloat/FPMUL_8_24.sv \
  generated/rial/RialMultFP32.sv
do
  python3 scripts/prepare_orfs_design.py "$src" --period 2000
done
```

Then run each nickname from `orfs_designs/asap7/*/config.mk`. Keep failures.

## Docker notes

`run_openroad_design.sh` uses `$HOME/OpenROAD-flow-scripts/flow/util/docker_shell` with the eval repo mounted at `/work`. Results land in `openroad_results/` (gitignored).

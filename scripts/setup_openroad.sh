#!/usr/bin/env bash
# Set up OpenROAD-flow-scripts.
#
# Docker (default, recommended for first paper runs):
#   bash scripts/setup_openroad.sh
#
# Local source build (no Docker; 1–3 hours, ~20GB):
#   bash scripts/setup_openroad.sh --local
set -euo pipefail

ORFS_DIR="${ORFS_DIR:-$HOME/OpenROAD-flow-scripts}"
OR_IMAGE="${OR_IMAGE:-openroad/orfs:latest}"
MODE="docker"

for arg in "$@"; do
  case "$arg" in
    --local) MODE="local" ;;
    --docker) MODE="docker" ;;
    -h|--help)
      sed -n '2,12p' "$0"
      exit 0
      ;;
    *)
      echo "Unknown argument: $arg" >&2
      echo "Usage: bash scripts/setup_openroad.sh [--docker|--local]" >&2
      exit 1
      ;;
  esac
done

echo "OpenROAD setup"
echo "  MODE=$MODE"
echo "  ORFS_DIR=$ORFS_DIR"

clone_orfs() {
  if [[ ! -d "$ORFS_DIR/.git" ]]; then
    echo "Cloning OpenROAD-flow-scripts into $ORFS_DIR ..."
    if [[ "$MODE" == "local" ]]; then
      git clone --recursive https://github.com/The-OpenROAD-Project/OpenROAD-flow-scripts.git "$ORFS_DIR"
    else
      git clone --depth 1 https://github.com/The-OpenROAD-Project/OpenROAD-flow-scripts.git "$ORFS_DIR"
    fi
  else
    echo "ORFS already present at $ORFS_DIR"
  fi
}

if [[ "$MODE" == "docker" ]]; then
  if ! command -v docker >/dev/null 2>&1; then
    cat >&2 <<'EOF'
ERROR: docker is not installed.

On Ubuntu:
  sudo apt update
  sudo apt install -y docker.io
  sudo usermod -aG docker "$USER"
  newgrp docker

Or skip Docker and build locally:
  bash scripts/setup_openroad.sh --local
EOF
    exit 1
  fi
  if ! docker info >/dev/null 2>&1; then
    cat >&2 <<'EOF'
ERROR: docker is installed but this user cannot talk to the daemon.

  sudo usermod -aG docker "$USER"
  newgrp docker
  sudo service docker start

Or skip Docker:
  bash scripts/setup_openroad.sh --local
EOF
    exit 1
  fi

  clone_orfs
  echo "Pulling $OR_IMAGE (several GB; first time only) ..."
  docker pull "$OR_IMAGE"

  echo
  echo "Running ASAP7 gcd smoke test inside the ORFS image ..."
  cd "$ORFS_DIR/flow"
  OR_IMAGE="$OR_IMAGE" ./util/docker_shell make DESIGN_CONFIG=./designs/asap7/gcd/config.mk
else
  clone_orfs
  if [[ ! -f "$ORFS_DIR/tools/OpenROAD/CMakeLists.txt" ]]; then
    echo "Initializing ORFS submodules (required for a local build) ..."
    git -C "$ORFS_DIR" submodule update --init --recursive
  fi

  echo
  echo "Installing ORFS dependencies via sudo ./setup.sh ..."
  echo "This uses apt and needs your password."
  ( cd "$ORFS_DIR" && sudo ./setup.sh )

  echo
  echo "Building OpenROAD + Yosys locally (often 1–3 hours) ..."
  echo "Log: $ORFS_DIR/build_openroad.log"
  ( cd "$ORFS_DIR" && ./build_openroad.sh --local )

  # shellcheck disable=SC1091
  source "$ORFS_DIR/env.sh"
  echo
  echo "Verifying binaries from env.sh (not the Ubuntu apt Yosys 0.33) ..."
  command -v yosys
  command -v openroad
  yosys -help >/dev/null
  openroad -help >/dev/null
  yosys -m slang -p "slang_version" || echo "WARNING: yosys slang plugin check failed"

  echo
  echo "Running default gcd/nangate45 smoke test ..."
  ( cd "$ORFS_DIR/flow" && make )

  echo
  echo "Running ASAP7 gcd smoke test ..."
  ( cd "$ORFS_DIR/flow" && make DESIGN_CONFIG=./designs/asap7/gcd/config.mk )
fi

echo
echo "Smoke test finished. Next, from the eval repo:"
echo "  python3 scripts/prepare_orfs_design.py generated/openfloat/FP_add_32_1.sv --period 2000"
if [[ "$MODE" == "local" ]]; then
  echo "  export ORFS_MODE=local"
  echo "  bash scripts/run_openroad_design.sh openfloat_FP_add_32_1"
else
  echo "  bash scripts/run_openroad_design.sh openfloat_FP_add_32_1"
fi
echo
echo "If gcd failed, paste the last 80 lines of the log before running FP designs."
echo "Do not use Bazel for this paper flow."

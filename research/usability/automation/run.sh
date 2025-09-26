#!/usr/bin/env bash
set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
OUTPUT_DIR="$ROOT/research/usability/automation/output"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +%Y%m%d-%H%M%S)
LOG_FILE="$OUTPUT_DIR/$TIMESTAMP.log"
LATEST_LINK="$OUTPUT_DIR/latest.log"

exec > >(tee "$LOG_FILE") 2>&1

start_section() {
  echo "\n=== $1 ==="
  date -u
}

run_cmd() {
  local name="$1"
  shift
  start_section "$name"
  if "$@"; then
    echo "[$name] OK"
  else
    local status=$?
    echo "[$name] WARN (exit $status)" >&2
  fi
}

if command -v flake8 >/dev/null 2>&1; then
  run_cmd "Environment" flake8 --version
else
  start_section "Environment"
  echo "flake8 not installed; skipping"
fi
run_cmd "CI Lint" env AURORA_CI_MODE=lint "$ROOT/tools/ci.sh"

run_cmd "Sample: Actor Pipeline" \
  cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  compiler/prototypes/examples/actor_pipeline.aur

run_cmd "Sample: GPU Image Filter" \
  cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  --emit=llvm examples/gpu_image_filter/image_filter.aur

run_cmd "Sample: Realtime Controller" \
  cargo run -p aurora-compiler-prototypes --bin aurora_cli -- \
  examples/realtime_controller/realtime_controller.aur

start_section "Documentation Scan"
rg "TODO" docs || true

run_cmd "Benchmark Collation" \
  cargo run -p aurora-benchmarks --bin collate -- --commit "$(git rev-parse HEAD)"

ln -sfn "$LOG_FILE" "$LATEST_LINK"

echo "\nSelf-assessment completed: $LOG_FILE"

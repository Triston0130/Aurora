#!/usr/bin/env bash
set -euo pipefail

if ! command -v cargo >/dev/null 2>&1; then
  echo "error: cargo not found" >&2
  exit 1
fi

rustc --version
cargo --version

mode="${AURORA_CI_MODE:-full}"

run_fmt_clippy() {
  echo "==> cargo fmt --all -- --check"
  cargo fmt --all -- --check

  echo "==> cargo clippy --workspace --all-targets --all-features -- -D warnings"
  cargo clippy --workspace --all-targets --all-features -- -D warnings
}

run_tests() {
  echo "==> cargo test --workspace --all-targets --all-features"
  cargo test --workspace --all-targets --all-features -- --nocapture

  if [[ "${AURORA_SKIP_DOC_TESTS:-0}" != "1" ]]; then
    echo "==> cargo test --workspace --doc"
    cargo test --workspace --doc -- --nocapture
  else
    echo "==> skipping doc tests (AURORA_SKIP_DOC_TESTS=${AURORA_SKIP_DOC_TESTS})"
  fi
}

case "${mode}" in
  lint)
    run_fmt_clippy
    ;;
  test)
    run_tests
    ;;
  full)
    run_fmt_clippy
    run_tests
    ;;
  *)
    echo "error: unknown AURORA_CI_MODE='${mode}'" >&2
    exit 1
    ;;
esac

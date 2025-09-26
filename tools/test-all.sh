#!/usr/bin/env bash
set -euo pipefail

if ! command -v cargo >/dev/null 2>&1; then
  echo "error: cargo not found" >&2
  exit 1
fi

echo "==> cargo fmt --all"
cargo fmt --all

echo "==> cargo clippy --workspace --all-targets --all-features"
cargo clippy --workspace --all-targets --all-features

echo "==> cargo test --workspace --all-targets --all-features"
cargo test --workspace --all-targets --all-features -- --nocapture

echo "==> cargo test --workspace --doc"
cargo test --workspace --doc -- --nocapture

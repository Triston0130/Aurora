#!/usr/bin/env bash
set -euo pipefail

ROOT=$(git rev-parse --show-toplevel)
"$ROOT/research/usability/automation/run.sh"

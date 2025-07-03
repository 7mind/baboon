#!/usr/bin/env bash

set -euo pipefail

function do-build() {
  step_run_cond run-fmt
  step_run_cond run-flake-refresh
  step_run_cond run-build
  step_run_cond run-test
  step_run_cond run-mkdist
  step_run_cond run-cache-update
}
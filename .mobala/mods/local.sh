#!/usr/bin/env bash

set -euo pipefail

step_enable run-fmt
step_enable run-build
step_enable run-test
step_enable run-flake-refresh

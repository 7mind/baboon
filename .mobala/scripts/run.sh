#!/usr/bin/env bash

set -euo pipefail

source ./${MOBALA_SUBDIR}/scripts/run-build.sh
source ./${MOBALA_SUBDIR}/scripts/run-flake-refresh.sh
source ./${MOBALA_SUBDIR}/scripts/run-fmt.sh
source ./${MOBALA_SUBDIR}/scripts/run-mkdist.sh
source ./${MOBALA_SUBDIR}/scripts/run-test.sh



function steps_register() {
  step_register run-fmt
  step_register run-flake-refresh
  step_register run-build
  step_register run-test  
  step_register run-mkdist
}
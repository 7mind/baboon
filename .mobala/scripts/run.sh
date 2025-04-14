#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

source ./${MOBALA_SUBDIR}/scripts/run-build.sh
source ./${MOBALA_SUBDIR}/scripts/run-flake-refresh.sh
source ./${MOBALA_SUBDIR}/scripts/run-fmt.sh
source ./${MOBALA_SUBDIR}/scripts/run-mkdist.sh
source ./${MOBALA_SUBDIR}/scripts/run-test.sh

function run() {
  run-fmt
  run-flake-refresh
  run-build
  run-test  
  run-mkdist
}
#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

function run-build() {
if [[ "$DO_BUILD" == 1  ]]; then
  sbt GraalVMNativeImage/packageBin
fi
}
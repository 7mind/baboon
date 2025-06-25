#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

export DO_FLAKE_VALIDATE="${DO_FLAKE_VALIDATE:-0}"

set_jvm_options
debug_env

# this script receives all the CLI args from the main script and may decide which flows should be enabled
flow_enable do-build
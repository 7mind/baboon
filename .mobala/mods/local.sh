#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

export DO_BUILD=1
export DO_TEST=1
export DO_FMT=1
export DO_FLAKE_REFRESH=1

for arg in "$@" ; do case $arg in
    *)
        ;;
esac done
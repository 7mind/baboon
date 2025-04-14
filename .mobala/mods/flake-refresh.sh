#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

export DO_FLAKE_REFRESH=1

for arg in "$@" ; do case $arg in
    --validate)
        export DO_FLAKE_VALIDATE=1
        ;;
    *)
        ;;
esac done
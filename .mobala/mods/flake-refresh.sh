#!/usr/bin/env bash

set -euo pipefail

step_enable run-flake-refresh

export DO_FLAKE_VALIDATE=0

for arg in "$@" ; do case $arg in
    --validate)
        export DO_FLAKE_VALIDATE=1
        ;;
    *)
        ;;
esac done
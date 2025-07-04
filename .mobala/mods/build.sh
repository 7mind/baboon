#!/usr/bin/env bash

set -euo pipefail

step_enable run-build

for arg in "$@" ; do case $arg in
    --update-cache|-u)
        step_enable run-cache-update
        ;;
    *)
        ;;
esac done
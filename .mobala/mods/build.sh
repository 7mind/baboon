#!/usr/bin/env bash

set -euo pipefail

step_enable run-build

for arg in "$@" ; do case $arg in
    *)
        ;;
esac done
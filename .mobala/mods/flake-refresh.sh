#!/usr/bin/env bash

set -euo pipefail

step_enable run-flake-refresh

for arg in "$@" ; do case $arg in
    *)
        ;;
esac done
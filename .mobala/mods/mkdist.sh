#!/usr/bin/env bash

set -euo pipefail

step_enable run-mkdist
export MKDIST_SOURCE=./target/graalvm-native-image
export MKDIST_TARGET=./target/dist

for arg in "$@" ; do case $arg in
    --source=*)
        export MKDIST_SOURCE="${arg#*=}"
        ;;
    --target=*)
        export MKDIST_TARGET="${arg#*=}"
        ;;      
    *)
        ;;
esac done
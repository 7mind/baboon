#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

export DO_MKDIST=1
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
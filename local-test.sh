#!/usr/bin/env bash

set -x
set -e

sbt GraalVMNativeImage/packageBin

./local-test.sh
#!/usr/bin/env bash

set -x
set -e

#export NATIVE_IMAGE_DEPRECATED_BUILDER_SANITATION=true
sbt GraalVMNativeImage/packageBin

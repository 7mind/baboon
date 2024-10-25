#!/usr/bin/env bash

set -e
set -x

sbt GraalVMNativeImage/packageBin

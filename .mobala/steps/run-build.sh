#!/usr/bin/env bash

set -euo pipefail

function run-build() {
  sbt baboonJVM/GraalVMNativeImage/packageBin
}
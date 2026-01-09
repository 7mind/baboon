#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

echo "=== Building native image ==="
cd ../..
sbt 'baboonJVM/GraalVMNativeImage/packageBin'

echo ""
echo "=== Installing npm dependencies ==="
cd test/test-lsp
npm install

echo ""
echo "=== Running LSP tests ==="
npm test

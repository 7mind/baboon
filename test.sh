#!/usr/bin/env bash

set -x
set -e

sbt GraalVMNativeImage/packageBin

target/graalvm-native-image/baboon \
          --model-dir ./src/test/resources/baboon/ \
          --output ./test/cs-stub/ConversionsTest/Generated \
          --test-output ./test/cs-stub/ConversionsTest/Generated
pushd .

cd ./test/cs-stub
dotnet build
dotnet test ConversionsTest/ConversionsTest.csproj

popd
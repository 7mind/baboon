#!/usr/bin/env bash

set -x
set -e

sbt GraalVMNativeImage/packageBin

pushd .

target/graalvm-native-image/baboon \
          --model-dir ./src/test/resources/baboon/ \
          --output ./test/cs-stub/ConversionsTest/Generated \
          --test-output ./test/cs-stub/ConversionsTest/Generated

cd ./test/cs-stub
dotnet build
dotnet test ConversionsTest/ConversionsTest.csproj

popd

pushd .

target/graalvm-native-image/baboon \
          --model-dir ./src/test/resources/baboon/ \
          --output ./test/cs-stub/ConversionsTest/Generated \
          --test-output ./test/cs-stub/ConversionsTest/Generated \
          --cs-use-compact-adt-form false \
          --cs-wrapped-adt-branch-codecs false


cd ./test/cs-stub
dotnet build
dotnet test ConversionsTest/ConversionsTest.csproj

popd
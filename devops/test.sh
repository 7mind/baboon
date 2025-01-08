#!/usr/bin/env bash

set -e
set -x

pushd .

target/graalvm-native-image/baboon \
          --model-dir ./src/test/resources/baboon/ \
          --output ./test/cs-stub/BaboonDefinitions/Generated \
          --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
          --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
          --cs-use-compact-adt-form true \
          --cs-wrapped-adt-branch-codecs false \
          --meta-write-evolution-json baboon-meta.json \
          --cs-write-evolution-dict true

cd ./test/cs-stub
dotnet build
dotnet test BaboonTests/BaboonTests.csproj

popd

pushd .

target/graalvm-native-image/baboon \
          --model-dir ./src/test/resources/baboon/ \
          --output ./test/cs-stub/BaboonDefinitions/Generated \
          --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
          --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
          --cs-use-compact-adt-form false \
          --cs-wrapped-adt-branch-codecs true \
          --meta-write-evolution-json baboon-meta.json \
          --cs-write-evolution-dict true


# workaround for https://github.com/NixOS/nixpkgs/issues/350806
# export PATH=`echo $PATH | tr ":" "\n" | grep -v "dotnet-runtime-6" | tr "\n" ":"`

cd ./test/cs-stub
dotnet build
dotnet test BaboonTests/BaboonTests.csproj

popd
#!/usr/bin/env bash

set -euo pipefail

function run-test() {
  pushd .

  target/graalvm-native-image/baboon \
            --model-dir ./src/test/resources/baboon/ \
            :cs \
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
            :cs \
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
  
  pushd .
  rm -rf ./test/conv-test-cs/ConvTest/Generated 

#  sbt "run --model-dir ./test/conv-test  --output ./test/conv-test-cs/ConvTest/Generated"
  
  target/graalvm-native-image/baboon  \
    --model-dir ./test/conv-test \
    :cs \
    --output ./test/conv-test-cs/ConvTest/Generated


      
  cd ./test/conv-test-cs
  dotnet build
  dotnet test 
  
  popd 
}

#!/usr/bin/env bash

set -euo pipefail

function run-test() {
  pushd .

  target/graalvm-native-image/baboon \
            --model-dir ./src/test/resources/baboon/ \
            --meta-write-evolution-json baboon-meta.json \
            \
            :cs \
            --output ./test/cs-stub/BaboonDefinitions/Generated \
            --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
            --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
            --cs-use-compact-adt-form true \
            --cs-wrapped-adt-branch-codecs false \
            --cs-write-evolution-dict true \
            \
            :scala \
            --output ./test/sc-stub/src/main/scala/generated-main \
            --test-output ./test/sc-stub/src/test/scala/generated-tests \
            --fixture-output ./test/sc-stub/src/main/scala/generated-fixtures \
            --sc-write-evolution-dict true \
            --sc-wrapped-adt-branch-codecs false
             
  pushd .
  cd ./test/cs-stub
  dotnet build -c Release
  dotnet test -c Release BaboonTests/BaboonTests.csproj
  popd
  
  pushd .
  cd ./test/sc-stub
  sbt +clean +test
  popd
  
  popd

  pushd .

  target/graalvm-native-image/baboon \
            --model-dir ./src/test/resources/baboon/ \
            --meta-write-evolution-json baboon-meta.json \
            \
            :cs \
            --output ./test/cs-stub/BaboonDefinitions/Generated \
            --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
            --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
            --cs-use-compact-adt-form false \
            --cs-wrapped-adt-branch-codecs true \
            --cs-write-evolution-dict true \
            --generate-ueba-codecs-by-default=true \
            --generate-json-codecs-by-default=true \
            \
            :scala \
            --output ./test/sc-stub/src/main/scala/generated-main \
            --test-output ./test/sc-stub/src/test/scala/generated-tests \
            --fixture-output ./test/sc-stub/src/main/scala/generated-fixtures \
            --sc-write-evolution-dict true \
            --sc-wrapped-adt-branch-codecs true


  # workaround for https://github.com/NixOS/nixpkgs/issues/350806
  # export PATH=`echo $PATH | tr ":" "\n" | grep -v "dotnet-runtime-6" | tr "\n" ":"`

  pushd .
  cd ./test/cs-stub
  dotnet build -c Debug
  dotnet test -c Debug BaboonTests/BaboonTests.csproj
  popd 
  
  pushd .
  cd ./test/sc-stub
  sbt +clean +test
  popd
  
  popd
  
  pushd .
  rm -rf ./test/conv-test-cs/ConvTest/Generated 

#  sbt "run --model-dir ./test/conv-test  --output ./test/conv-test-cs/ConvTest/Generated"
  
  target/graalvm-native-image/baboon  \
    --model-dir ./test/conv-test \
    :cs \
    --output ./test/conv-test-cs/ConvTest/Generated \
    :scala \
    --output ./test/conv-test-sc/src/main/scala/generated-main    

  pushd .
  cd ./test/conv-test-cs
  dotnet build
  dotnet test 
  popd 
  
  pushd . 
  cd ./test/conv-test-sc
  sbt +clean +test
  popd
    
  popd 
}

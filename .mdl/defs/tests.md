# Baboon Test Actions

This file defines test-related actions for the Baboon project.

# action: test-gen-regular-adt

Generate code with regular (non-wrapped) ADT branch codecs.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-regular"

# Create temporary test directories
mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs false \
  --cs-write-evolution-dict true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict true \
  --sc-wrapped-adt-branch-codecs false

ret success:bool=true
```

# action: test-cs-regular

Run C# tests with regular ADT codecs (Release configuration).

```bash
dep action.test-gen-regular-adt

pushd ./target/test-regular/cs-stub
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-regular

Run Scala tests with regular ADT codecs.

```bash
dep action.test-gen-regular-adt

pushd ./target/test-regular/sc-stub
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-wrapped-adt

Generate code with wrapped ADT branch codecs.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-wrapped"

# Create temporary test directories
mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs true \
  --cs-write-evolution-dict true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict true \
  --sc-wrapped-adt-branch-codecs true

ret success:bool=true
```

# action: test-cs-wrapped

Run C# tests with wrapped ADT codecs (Debug configuration).

```bash
dep action.test-gen-wrapped-adt

pushd ./target/test-wrapped/cs-stub
dotnet build -c Debug
dotnet test -c Debug BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-wrapped

Run Scala tests with wrapped ADT codecs.

```bash
dep action.test-gen-wrapped-adt

pushd ./target/test-wrapped/sc-stub
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-manual

Generate code for manual test projects.

```bash
BABOON_BIN="${action.build.binary}"

rm -rf ./test/conv-test-cs/ConvTest/Generated

$BABOON_BIN \
  --model-dir ./test/conv-test \
  :cs \
  --output ./test/conv-test-cs/ConvTest/Generated \
  :scala \
  --output ./test/conv-test-sc/src/main/scala/generated-main

ret success:bool=true
```

# action: test-gen-compat-scala

Generate compatibility test files using Scala.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-sc
sbt "runMain example.CompatMain"
popd

ret success:bool=true
```

# action: test-gen-compat-cs

Generate compatibility test files using C#.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-cs
dotnet run --project ConvTest/ConvTest.csproj
popd

ret success:bool=true
```

# action: test-manual-cs

Run manual C# compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs

pushd ./test/conv-test-cs
dotnet build
dotnet test
popd

ret success:bool=true
```

# action: test-manual-scala

Run manual Scala compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs

pushd ./test/conv-test-sc
sbt +clean +test
popd

ret success:bool=true
```

# action: test

Run complete test suite (orchestrator action).

```bash
dep action.test-gen-regular-adt
dep action.test-cs-regular
dep action.test-scala-regular
dep action.test-gen-wrapped-adt
dep action.test-cs-wrapped
dep action.test-scala-wrapped
dep action.test-gen-manual
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-manual-cs
dep action.test-manual-scala

ret success:bool=true
```

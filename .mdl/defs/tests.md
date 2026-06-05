# Baboon Test Actions

This file defines test-related actions for the Baboon project.

# action: restore-dotnet

Restore all .NET dependencies once to avoid parallel restores.

```bash
pushd ./test/cs-stub
dotnet restore BaboonDefinitions/BaboonDefinitions.csproj
dotnet restore BaboonTests/BaboonTests.csproj
popd

pushd ./test/conv-test-cs
dotnet restore ConvTest/ConvTest.csproj
popd

ret success:bool=true
```

# action: test-gen-regular-adt

Generate code with regular (non-wrapped) ADT branch codecs.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-regular"

# Create temporary test directories
mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub" "$TEST_DIR/py-stub" "$TEST_DIR/rs-stub" "$TEST_DIR/ts-stub" "$TEST_DIR/kt-stub" "$TEST_DIR/kt-stub-kmp" "$TEST_DIR/jv-stub" "$TEST_DIR/dt-stub" "$TEST_DIR/sw-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/rs-stub/ "$TEST_DIR/rs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub-kmp/ "$TEST_DIR/kt-stub-kmp/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"
rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"
rsync -a --exclude='.build' --exclude='.swiftpm' \
  ./test/sw-stub/ "$TEST_DIR/sw-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --test-output "$TEST_DIR/rs-stub/src" \
  --fixture-output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/generated" \
  --test-output "$TEST_DIR/ts-stub/src/baboontests/generatedtests" \
  --fixture-output "$TEST_DIR/ts-stub/src/baboontests/generatedfixtures" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :kotlin \
  --output "$TEST_DIR/kt-stub/src/main/kotlin/generated-main" \
  --test-output "$TEST_DIR/kt-stub/src/test/kotlin/generated-tests" \
  --fixture-output "$TEST_DIR/kt-stub/src/main/kotlin/generated-fixtures" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :java \
  --output "$TEST_DIR/jv-stub/src/main/java/generated-main" \
  --test-output "$TEST_DIR/jv-stub/src/test/java/generated-tests" \
  --fixture-output "$TEST_DIR/jv-stub/src/main/java/generated-fixtures" \
  --jv-write-evolution-dict=true \
  --jv-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :dart \
  --output "$TEST_DIR/dt-stub/lib" \
  --test-output "$TEST_DIR/dt-stub/test" \
  --fixture-output "$TEST_DIR/dt-stub/lib" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --test-output "$TEST_DIR/sw-stub/Tests/BaboonTests" \
  --fixture-output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true

# Re-rsync hand-written Dart runtime tests under test/runtime/ — the baboon `--test-output`
# pass erases the entire test/ directory before writing generated tests (see
# BaboonCompiler.cleanupTargetPaths via IzFiles.erase), so the original rsync of
# test/dt-stub/test/runtime/ (the only checked-in subdir of test/) gets wiped. Restoring
# it after codegen keeps hand-written runtime-side tests (e.g. domain_facade_test.dart)
# discoverable by `dart test`.
if [ -d "./test/dt-stub/test/runtime" ]; then
  rsync -a ./test/dt-stub/test/runtime/ "$TEST_DIR/dt-stub/test/runtime/"
fi

# Move Dart runtime files into the baboon_runtime package
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_fixture.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_any_opaque.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_codecs_facade.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_identifier_repr.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

# PR-68 (M23.4): Swift SPM splits BaboonTests into per-module .testTarget()s
# (one per Tests/BaboonTests/<Module>/ subdirectory) to avoid `.o` filename
# collisions on duplicate basenames (e.g. holder_test.swift in both
# MyOkM19Direct and MyOkM19Foreign). Each per-module test target is its own
# Swift module with path Tests/BaboonTests/<Module>; the codegen-emitted
# CrossLanguageFixturePath.swift sits at Tests/BaboonTests/ top level and is
# referenced (no import) from generated tests as if it were in the same
# module. We materialize that by copying the helper into every per-module
# subdirectory so each per-module target compiles its own copy.
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

# Generate KMP Kotlin code (separate invocation with --kt-multiplatform=true)
$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :kotlin \
  --output "$TEST_DIR/kt-stub-kmp/src/main/kotlin/generated-main" \
  --test-output "$TEST_DIR/kt-stub-kmp/src/test/kotlin/generated-tests" \
  --fixture-output "$TEST_DIR/kt-stub-kmp/src/main/kotlin/generated-fixtures" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --kt-multiplatform=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-regular

Run C# tests with regular ADT codecs (Release configuration).

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-regular

Run Scala tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-python-regular

Run Python tests with regular adt codecs. 

```bash
dep action.test-cs-regular

TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests/GeneratedTests/testpkg/pkg0
python3 -m unittest discover -s BaboonTests/RuntimeTests
popd

ret success:bool=true
```

# action: test-rust-regular

Run Rust tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test
popd

ret success:bool=true
```

# action: test-typescript-regular

Run TypeScript tests with regular ADT codecs.

```bash
dep action.test-cs-regular

TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npm test
popd

ret success:bool=true
```

# action: test-kotlin-regular

Run Kotlin tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test
popd

ret success:bool=true
```

# action: test-kotlin-kmp-regular

Run Kotlin Multiplatform tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/kt-stub-kmp"
gradle --no-daemon clean test
popd

ret success:bool=true
```

# action: test-java-regular

Run Java tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/jv-stub"
mvn clean test
popd

ret success:bool=true
```

# action: test-dart-regular

Run Dart tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/dt-stub"
dart pub get
dart analyze --fatal-warnings
dart test
popd

ret success:bool=true
```

# action: test-swift-regular

Run Swift tests with regular ADT codecs.

```bash
if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

TEST_DIR="${action.test-gen-regular-adt.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-wrapped-adt

Generate code with wrapped ADT branch codecs.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-wrapped"

# Create temporary test directories
mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub" "$TEST_DIR/py-stub" "$TEST_DIR/rs-stub" "$TEST_DIR/ts-stub" "$TEST_DIR/kt-stub" "$TEST_DIR/kt-stub-kmp" "$TEST_DIR/jv-stub" "$TEST_DIR/dt-stub" "$TEST_DIR/sw-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/rs-stub/ "$TEST_DIR/rs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub-kmp/ "$TEST_DIR/kt-stub-kmp/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"
rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"
rsync -a --exclude='.build' --exclude='.swiftpm' \
  ./test/sw-stub/ "$TEST_DIR/sw-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs=true \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=true \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --test-output "$TEST_DIR/rs-stub/src" \
  --fixture-output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/generated" \
  --test-output "$TEST_DIR/ts-stub/src/baboontests/generatedtests" \
  --fixture-output "$TEST_DIR/ts-stub/src/baboontests/generatedfixtures" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :kotlin \
  --output "$TEST_DIR/kt-stub/src/main/kotlin/generated-main" \
  --test-output "$TEST_DIR/kt-stub/src/test/kotlin/generated-tests" \
  --fixture-output "$TEST_DIR/kt-stub/src/main/kotlin/generated-fixtures" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :java \
  --output "$TEST_DIR/jv-stub/src/main/java/generated-main" \
  --test-output "$TEST_DIR/jv-stub/src/test/java/generated-tests" \
  --fixture-output "$TEST_DIR/jv-stub/src/main/java/generated-fixtures" \
  --jv-write-evolution-dict=true \
  --jv-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :dart \
  --output "$TEST_DIR/dt-stub/lib" \
  --test-output "$TEST_DIR/dt-stub/test" \
  --fixture-output "$TEST_DIR/dt-stub/lib" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --test-output "$TEST_DIR/sw-stub/Tests/BaboonTests" \
  --fixture-output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true

# Re-rsync hand-written Dart runtime tests under test/runtime/ — same rationale as in
# test-gen-regular-adt: baboon's --test-output cleanup erases the entire test/ directory
# before emitting generated tests, so the rsync of test/dt-stub/test/runtime/ is lost.
if [ -d "./test/dt-stub/test/runtime" ]; then
  rsync -a ./test/dt-stub/test/runtime/ "$TEST_DIR/dt-stub/test/runtime/"
fi

# Move Dart runtime files into the baboon_runtime package
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_fixture.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_any_opaque.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_codecs_facade.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_identifier_repr.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

# PR-68 (M23.4): see test-gen-regular-adt for rationale.
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

# Generate KMP Kotlin code (separate invocation with --kt-multiplatform=true)
$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :kotlin \
  --output "$TEST_DIR/kt-stub-kmp/src/main/kotlin/generated-main" \
  --test-output "$TEST_DIR/kt-stub-kmp/src/test/kotlin/generated-tests" \
  --fixture-output "$TEST_DIR/kt-stub-kmp/src/main/kotlin/generated-fixtures" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --kt-multiplatform=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wrapped

Run C# tests with wrapped ADT codecs (Debug configuration).

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Debug
dotnet test -c Debug BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-wrapped

Run Scala tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-python-wrapped

Run Python tests with wrapped ADT codecs

```bash
dep action.test-cs-wrapped

TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests/GeneratedTests/testpkg/pkg0
python3 -m unittest discover -s BaboonTests/RuntimeTests
popd

ret success:bool=true
```

# action: test-rust-wrapped

Run Rust tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test
popd

ret success:bool=true
```

# action: test-typescript-wrapped

Run TypeScript tests with wrapped ADT codecs.

```bash
dep action.test-cs-wrapped

TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npm test
popd

ret success:bool=true
```

# action: test-kotlin-wrapped

Run Kotlin tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test
popd

ret success:bool=true
```

# action: test-kotlin-kmp-wrapped

Run Kotlin Multiplatform tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/kt-stub-kmp"
gradle --no-daemon clean test
popd

ret success:bool=true
```

# action: test-java-wrapped

Run Java tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/jv-stub"
mvn clean test
popd

ret success:bool=true
```

# action: test-dart-wrapped

Run Dart tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/dt-stub"
dart pub get
dart analyze --fatal-warnings
dart test
popd

ret success:bool=true
```

# action: test-swift-wrapped

Run Swift tests with wrapped ADT codecs.

```bash
if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-manual

Generate code for manual test projects.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"

rm -rf ./test/conv-test-cs/ConvTest/Generated

$BABOON_BIN \
  --model-dir ./test/conv-test \
  :cs \
  --output ./test/conv-test-cs/ConvTest/Generated \
  :scala \
  --output ./test/conv-test-sc/src/main/scala/generated-main \
  :python  \
  --output ./test/conv-test-py/Generated \
  :rust \
  --output ./test/conv-test-rs/src/generated \
  :typescript \
  --output ./test/conv-test-ts/src/generated \
  :kotlin \
  --output ./test/conv-test-kt/src/main/kotlin/generated-main \
  :java \
  --output ./test/conv-test-jv/src/main/java/generated-main \
  :dart \
  --output ./test/conv-test-dt/lib/generated \
  :swift \
  --output ./test/conv-test-sw/Generated

# Generate KMP Kotlin conv-test code
$BABOON_BIN \
  --model-dir ./test/conv-test \
  :kotlin \
  --output ./test/conv-test-kt-kmp/src/main/kotlin/generated-main \
  --kt-multiplatform=true

# Move Dart runtime files into the baboon_runtime package.
# NOTE: test-gen-manual deliberately does NOT pass --fixture-output to the
# Baboon compiler for any backend (the manual-flavour conv-test exercises
# hand-crafted compat fixtures, not generated random fixtures). Therefore
# baboon_fixture.dart is not produced and not moved here, asymmetric to
# test-gen-regular-adt and test-gen-wrapped-adt. (Closes PR-22-D07.)
mv ./test/conv-test-dt/lib/generated/baboon_runtime.dart ./test/conv-test-dt/packages/baboon_runtime/lib/
mv ./test/conv-test-dt/lib/generated/baboon_any_opaque.dart ./test/conv-test-dt/packages/baboon_runtime/lib/
mv ./test/conv-test-dt/lib/generated/baboon_codecs_facade.dart ./test/conv-test-dt/packages/baboon_runtime/lib/
mv ./test/conv-test-dt/lib/generated/baboon_identifier_repr.dart ./test/conv-test-dt/packages/baboon_runtime/lib/

ret success:bool=true
```

# action: test-gen-compat-java

Generate compatibility test files using Java.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-jv
mvn compile exec:java
popd

ret success:bool=true
```

# action: test-gen-compat-kotlin

Generate compatibility test files using Kotlin.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-kt
gradle --no-daemon run
popd

ret success:bool=true
```

# action: test-gen-compat-kotlin-kmp

Generate compatibility test files using Kotlin Multiplatform.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-kt-kmp
gradle --no-daemon run
popd

ret success:bool=true
```

# action: test-gen-compat-python

Generate compatibility test files using Python. 

```bash
dep action.test-gen-manual

pushd ./test/conv-test-py
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 compat_main.py
popd

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

# action: test-gen-compat-rust

Generate compatibility test files using Rust.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-rs
cargo run
popd

ret success:bool=true
```

# action: test-gen-compat-typescript

Generate compatibility test files using TypeScript.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-ts
npm install
npm run compat
popd

ret success:bool=true
```

# action: test-gen-compat-dart

Generate compatibility test files using Dart.

```bash
dep action.test-gen-manual

pushd ./test/conv-test-dt
dart pub get
dart run bin/compat_main.dart
popd

ret success:bool=true
```

# action: test-gen-compat-swift

Generate compatibility test files using Swift.

```bash
dep action.test-gen-manual

# Eliminate SwiftPM .build/ cache foot-gun: stale PCH paths from other worktree
# checkouts and missed new-test-file pickups have reproduced twice in 24h.
# Regen rewrites Swift sources anyway, so the incremental graph is mostly
# invalidated; cleaning here is cheap and deterministic. Do NOT replicate this
# in :test-manual-swift — its cache value across runs is high.
rm -rf ./test/conv-test-sw/.build

if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping compat gen"
  ret success:bool=true
  exit 0
fi

./scripts/swift-xcode.sh ./test/conv-test-sw run CompatMain

ret success:bool=true
```

# action: test-manual-cs

Run manual C# compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

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
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-sc
sbt +clean +test
popd

ret success:bool=true
```

# action: test-sbt-basic

Run basic SBT tests. Depends on test-cs-regular because RTCodecTest reads
JSON/UEBA files produced by C# tests.

```bash
dep action.build
dep action.test-cs-regular
sbt +test

ret success:bool=true
```

# action: test-manual-python

Run Python conversion test

```bash
dep action.test-gen-compat-python
pushd ./test/conv-test-py
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m unittest discover -s .
popd

ret success:bool=true
```

# action: test-manual-rust

Run Rust cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-rs
cargo test
popd

ret success:bool=true
```

# action: test-manual-typescript

Run TypeScript cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-ts
npm install
npm test
popd

ret success:bool=true
```

# action: test-manual-kotlin

Run Kotlin cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-kt
gradle --no-daemon test
popd

ret success:bool=true
```

# action: test-manual-kotlin-kmp

Run Kotlin Multiplatform cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-kotlin-kmp
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-kt-kmp
gradle --no-daemon test
popd

ret success:bool=true
```

# action: test-manual-java

Run Java cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-jv
mvn clean test
popd

ret success:bool=true
```

# action: test-manual-dart

Run Dart cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

pushd ./test/conv-test-dt
dart pub get
dart test
popd

ret success:bool=true
```

# action: test-manual-swift

Run Swift cross-language compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python
dep action.test-gen-compat-rust
dep action.test-gen-compat-typescript
dep action.test-gen-compat-kotlin
dep action.test-gen-compat-java
dep action.test-gen-compat-dart
dep action.test-gen-compat-swift

if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

./scripts/swift-xcode.sh ./test/conv-test-sw test

ret success:bool=true
```

# action: test-gen-cs-wiring-either

Generate code for C# wiring tests with built-in Either container.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-wiring-either"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a ./test/cs-stub-either-overlay/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-either.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  --service-result-pattern="<\$error, \$success>"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wiring-either

Run C# Either wiring tests.

```bash
TEST_DIR="${action.test-gen-cs-wiring-either.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-gen-cs-wiring-result

Generate code for C# wiring tests with custom Result container (reversed param order).

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-wiring-result"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a ./test/cs-stub-result-overlay/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-result.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="CustomContainers.Result" \
  --service-result-pattern="<\$success, \$error>"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wiring-result

Run C# Result wiring tests.

```bash
TEST_DIR="${action.test-gen-cs-wiring-result.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-gen-cs-wiring-outcome

Generate code for C# wiring tests with single-param Outcome container.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-wiring-outcome"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a ./test/cs-stub-outcome-overlay/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-outcome.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/cs-stub/BaboonTests/GeneratedFixtures" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="CustomContainers.Outcome" \
  --service-result-pattern="<\$success>"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wiring-outcome

Run C# Outcome wiring tests.

```bash
TEST_DIR="${action.test-gen-cs-wiring-outcome.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-gen-sc-wiring-either

Generate code for Scala wiring tests with built-in Either container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sc-wiring-either"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sc-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a ./test/sc-stub-either-overlay/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-sc-either.lock \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  --service-result-pattern="[\$error, \$success]"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sc-wiring-either

Run Scala Either wiring tests.

```bash
TEST_DIR="${action.test-gen-sc-wiring-either.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-sc-wiring-result

Generate code for Scala wiring tests with custom Result container (reversed param order).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sc-wiring-result"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sc-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a ./test/sc-stub-result-overlay/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-sc-result.lock \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="containers.Result" \
  --service-result-pattern="[\$success, \$error]"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sc-wiring-result

Run Scala Result wiring tests.

```bash
TEST_DIR="${action.test-gen-sc-wiring-result.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-sc-wiring-outcome

Generate code for Scala wiring tests with single-param Outcome container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sc-wiring-outcome"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sc-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a ./test/sc-stub-outcome-overlay/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-sc-outcome.lock \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="containers.Outcome" \
  --service-result-pattern="[\$success]"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sc-wiring-outcome

Run Scala Outcome wiring tests.

```bash
TEST_DIR="${action.test-gen-sc-wiring-outcome.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-sc-wiring-hkt

Generate code for Scala wiring tests with HKT (higher-kinded type parameter F[+_, +_]).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sc-wiring-hkt"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sc-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a ./test/sc-stub-hkt-overlay/ "$TEST_DIR/sc-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-sc-hkt.lock \
  :scala \
  --output "$TEST_DIR/sc-stub/src/main/scala/generated-main" \
  --test-output "$TEST_DIR/sc-stub/src/test/scala/generated-tests" \
  --fixture-output "$TEST_DIR/sc-stub/src/main/scala/generated-fixtures" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="custom.MyBi" \
  --service-result-pattern="[\$error, \$success]" \
  --service-result-hkt=true \
  --service-result-hkt-name="F" \
  --service-result-hkt-signature="[+_, +_]"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sc-wiring-hkt

Run Scala HKT wiring tests.

```bash
TEST_DIR="${action.test-gen-sc-wiring-hkt.test_dir}"
pushd "$TEST_DIR/sc-stub"
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-ts-wiring-either

Generate code for TypeScript wiring tests with built-in BaboonEither container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-wiring-either"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-ts-either.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/baboondefinitions/generated" \
  --test-output "$TEST_DIR/ts-stub/src/baboontests/generatedtests" \
  --fixture-output "$TEST_DIR/ts-stub/src/baboontests/generatedfixtures" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>'

rsync -a ./test/ts-stub-either-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-wiring-either

Run TypeScript BaboonEither wiring tests.

```bash
TEST_DIR="${action.test-gen-ts-wiring-either.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/wiring.test.ts
popd

ret success:bool=true
```

# action: test-gen-ts-wiring-result

Generate code for TypeScript wiring tests with custom Result container (reversed param order).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-wiring-result"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-ts-result.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/baboondefinitions/generated" \
  --test-output "$TEST_DIR/ts-stub/src/baboontests/generatedtests" \
  --fixture-output "$TEST_DIR/ts-stub/src/baboontests/generatedfixtures" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Result" \
  '--service-result-pattern=<$success, $error>'

rsync -a ./test/ts-stub-result-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-wiring-result

Run TypeScript Result wiring tests.

```bash
TEST_DIR="${action.test-gen-ts-wiring-result.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/wiring.test.ts
popd

ret success:bool=true
```

# action: test-gen-ts-wiring-outcome

Generate code for TypeScript wiring tests with single-param Outcome container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-wiring-outcome"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-ts-outcome.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/baboondefinitions/generated" \
  --test-output "$TEST_DIR/ts-stub/src/baboontests/generatedtests" \
  --fixture-output "$TEST_DIR/ts-stub/src/baboontests/generatedfixtures" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Outcome" \
  '--service-result-pattern=<$success>'

rsync -a ./test/ts-stub-outcome-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-wiring-outcome

Run TypeScript Outcome wiring tests.

```bash
TEST_DIR="${action.test-gen-ts-wiring-outcome.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/wiring.test.ts
popd

ret success:bool=true
```

# action: test-gen-ts-mcp

Generate code for the TypeScript MCP round-trip overlay test (T9).
Uses the mcp-stub-ok model + `--ts-generate-mcp-server=true` and overlays
`test/ts-stub-mcp-overlay/` on top of a ts-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-ts-mcp.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/baboondefinitions/generated" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --ts-generate-mcp-server=true

rsync -a ./test/ts-stub-mcp-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-mcp

Run the TypeScript MCP round-trip overlay tests (T9).
Validates initialize/tools-list/tools-call + error paths; runs AJV Draft-2020-12
validation on every returned inputSchema.

```bash
TEST_DIR="${action.test-gen-ts-mcp.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/mcp.test.ts
popd

ret success:bool=true
```

# action: test-gen-cs-mcp

Generate code for the C# MCP round-trip overlay test (T11).
Uses the mcp-stub-ok model + `--cs-generate-mcp-server=true` (Either mode) and
overlays `test/cs-stub-mcp-overlay/` on top of a cs-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-cs-mcp.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  --service-result-pattern="<\$error, \$success>" \
  --cs-generate-mcp-server=true

rsync -a ./test/cs-stub-mcp-overlay/ "$TEST_DIR/cs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-mcp

Run the C# MCP round-trip overlay tests (T11).
Validates initialize/tools-list/tools-call + error paths; runs NJsonSchema
validation on every returned inputSchema (with `$defs` → `definitions`
normalisation for NJsonSchema compatibility).

```bash
TEST_DIR="${action.test-gen-cs-mcp.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release McpTests/McpTests.csproj
dotnet test -c Release McpTests/McpTests.csproj
popd

ret success:bool=true
```

# action: test-gen-rust-mcp

Generate code for the Rust MCP round-trip overlay test (T13).
Uses the mcp-stub-ok model + `--rs-generate-mcp-server=true` (Result errors mode) and
overlays `test/rust-stub-mcp-overlay/` on top of a rs-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rust-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"
mkdir -p "$TEST_DIR/rs-stub"

# Copy only the Cargo.toml (package name = baboon-rs-stub) from rs-stub;
# the generated source tree from mcp-stub-ok will be written into src/.
cp ./test/rs-stub/Cargo.toml "$TEST_DIR/rs-stub/Cargo.toml"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-rust-mcp.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type=Result \
  '--service-result-pattern=<$success, $error>' \
  --rs-generate-mcp-server=true

rsync -a ./test/rust-stub-mcp-overlay/ "$TEST_DIR/rs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rust-mcp

Run the Rust MCP round-trip overlay tests (T13).
Validates initialize/tools-list/tools-call + error paths; runs serde_json
validation on every returned inputSchema (K1 structural equality check).

```bash
TEST_DIR="${action.test-gen-rust-mcp.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test --test mcp_tests
popd

ret success:bool=true
```

# action: test-gen-kotlin-mcp

Generate code for the Kotlin MCP round-trip overlay test (T14).
Uses the mcp-stub-ok model + `--kt-generate-mcp-server=true` (Either mode) and
overlays `test/kotlin-stub-mcp-overlay/` on top of a kt-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kotlin-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-kotlin-mcp.lock \
  :kotlin \
  --output "$TEST_DIR/kt-stub/src/main/kotlin/generated-main" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  '--service-result-pattern=<$error, $success>' \
  --kt-generate-mcp-server=true

rsync -a ./test/kotlin-stub-mcp-overlay/ "$TEST_DIR/kt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-kotlin-mcp

Run the Kotlin MCP round-trip overlay tests (T14).
Validates initialize/tools-list/tools-call + error paths; runs kotlinx-serialization
JSON round-trip validation on every returned inputSchema (K1 structural equality
to T7 reference) and exercises negative controls.

```bash
TEST_DIR="${action.test-gen-kotlin-mcp.test_dir}"
pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test --tests "mcp.McpTests"
popd

ret success:bool=true
```

# action: test-gen-java-mcp

Generate code for the Java MCP round-trip overlay test (T15).
Uses the mcp-stub-ok model + `--jv-generate-mcp-server=true` (Either mode) and
overlays `test/java-stub-mcp-overlay/` on top of a jv-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-java-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-java-mcp.lock \
  :java \
  --output "$TEST_DIR/jv-stub/src/main/java/generated-main" \
  --jv-write-evolution-dict=true \
  --jv-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="baboon.runtime.shared.BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --jv-generate-mcp-server=true

rsync -a ./test/java-stub-mcp-overlay/ "$TEST_DIR/jv-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-java-mcp

Run the Java MCP round-trip overlay tests (T15).
Validates initialize/tools-list/tools-call + error paths; runs Jackson
JSON round-trip validation on every returned inputSchema (K1 structural equality
to T7 reference) and exercises negative controls.

```bash
TEST_DIR="${action.test-gen-java-mcp.test_dir}"
pushd "$TEST_DIR/jv-stub"
mvn clean test -Dtest=mcp.McpTests
popd

ret success:bool=true
```

# action: test-gen-rs-wiring-either

Generate code for Rust wiring tests with built-in Result container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-wiring-either"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/rs-stub/ "$TEST_DIR/rs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-rs-either.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --test-output "$TEST_DIR/rs-stub/src" \
  --fixture-output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Result" \
  --service-result-pattern="<\$success, \$error>"

rsync -a ./test/rs-stub-either-overlay/ "$TEST_DIR/rs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-wiring-either

Run Rust Either wiring tests.

```bash
TEST_DIR="${action.test-gen-rs-wiring-either.test_dir}"
pushd "$TEST_DIR/rs-stub"
cargo test
popd

ret success:bool=true
```

# action: test-gen-rs-wiring-result

Generate code for Rust wiring tests with custom MyResult container (reversed param order).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-wiring-result"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/rs-stub/ "$TEST_DIR/rs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-rs-result.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --test-output "$TEST_DIR/rs-stub/src" \
  --fixture-output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="crate::custom_containers::MyResult" \
  --service-result-pattern="<\$success, \$error>"

rsync -a ./test/rs-stub-result-overlay/ "$TEST_DIR/rs-stub/"
echo 'pub mod custom_containers;' >> "$TEST_DIR/rs-stub/src/lib.rs"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-wiring-result

Run Rust MyResult wiring tests.

```bash
TEST_DIR="${action.test-gen-rs-wiring-result.test_dir}"
pushd "$TEST_DIR/rs-stub"
cargo test
popd

ret success:bool=true
```

# action: test-gen-rs-wiring-outcome

Generate code for Rust wiring tests with single-param Outcome container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-wiring-outcome"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/rs-stub/ "$TEST_DIR/rs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-rs-outcome.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --test-output "$TEST_DIR/rs-stub/src" \
  --fixture-output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="crate::custom_containers::Outcome" \
  --service-result-pattern="<\$success>"

rsync -a ./test/rs-stub-outcome-overlay/ "$TEST_DIR/rs-stub/"
echo 'pub mod custom_containers;' >> "$TEST_DIR/rs-stub/src/lib.rs"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-wiring-outcome

Run Rust Outcome wiring tests.

```bash
TEST_DIR="${action.test-gen-rs-wiring-outcome.test_dir}"
pushd "$TEST_DIR/rs-stub"
cargo test
popd

ret success:bool=true
```

# action: test-gen-py-wiring-either

Generate code for Python wiring tests with built-in BaboonEither container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-wiring-either"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' --exclude='.venv' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"
rsync -a ./test/py-stub-either-overlay/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-py-either.lock \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=[$error, $success]'

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-py-wiring-either

Run Python BaboonEither wiring tests.

```bash
TEST_DIR="${action.test-gen-py-wiring-either.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests -p "WiringTests.py"
popd

ret success:bool=true
```

# action: test-gen-py-wiring-result

Generate code for Python wiring tests with custom Result container (reversed param order).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-wiring-result"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' --exclude='.venv' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"
rsync -a ./test/py-stub-result-overlay/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-py-result.lock \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  --service-result-no-errors=false \
  --service-result-type="Result" \
  '--service-result-pattern=[$success, $error]'

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-py-wiring-result

Run Python Result wiring tests.

```bash
TEST_DIR="${action.test-gen-py-wiring-result.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests -p "WiringTests.py"
popd

ret success:bool=true
```

# action: test-gen-py-wiring-outcome

Generate code for Python wiring tests with single-param Outcome container.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-wiring-outcome"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' --exclude='.venv' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"
rsync -a ./test/py-stub-outcome-overlay/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-py-outcome.lock \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  --service-result-no-errors=false \
  --service-result-type="Outcome" \
  '--service-result-pattern=[$success]'

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-py-wiring-outcome

Run Python Outcome wiring tests.

```bash
TEST_DIR="${action.test-gen-py-wiring-outcome.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests -p "WiringTests.py"
popd

ret success:bool=true
```

# action: test-py-wiring-async

Generate async Python service wiring (`--py-async-services=true`) for the
petstore model and assert syntactic validity via `py_compile`. This exercises
the `async def` / `await` axis of `PyServiceWiringTranslator` +
`PyDefnTranslator` (interface methods, invoke dispatchers, client). No async
unittest overlay exists yet, so this lane is generation + compile-only; the
synchronous wiring round-trips remain covered by the
`test-py-wiring-{either,result,outcome}` lanes.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-wiring-async"

rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lock-file=./target/baboon-py-async.lock \
  :python \
  --output "$TEST_DIR/gen" \
  --py-async-services=true \
  --service-result-no-errors=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true

python3 -m compileall -q "$TEST_DIR/gen"

# Assert the async axis actually fired in generated service files.
grep -q "async def" "$TEST_DIR/gen/petstore/api/PetStore.py"
grep -q "await " "$TEST_DIR/gen/petstore/api/PetStore_Wiring.py"
grep -q "Awaitable\[" "$TEST_DIR/gen/petstore/api/PetStore_Client.py"

ret success:bool=true
```

# action: test-gen-kt-wiring

Generate code for Kotlin wiring tests in no-errors service-result mode
(matches the service-acceptance harness's `--service-result-no-errors=true`).

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kt-wiring"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-kt-wiring.lock \
  :kotlin \
  --output "$TEST_DIR/kt-stub/src/main/kotlin/generated-main" \
  --test-output "$TEST_DIR/kt-stub/src/test/kotlin/generated-tests" \
  --fixture-output "$TEST_DIR/kt-stub/src/main/kotlin/generated-fixtures" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true

rsync -a ./test/kt-stub-wiring-overlay/ "$TEST_DIR/kt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-kt-wiring

Run Kotlin cross-domain muxer + per-domain wiring tests.

```bash
TEST_DIR="${action.test-gen-kt-wiring.test_dir}"
pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test --tests "runtime.WiringTests"
popd

ret success:bool=true
```

# action: test-gen-jv-wiring

Generate code for Java wiring tests in no-errors service-result mode.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-jv-wiring"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-jv-wiring.lock \
  :java \
  --output "$TEST_DIR/jv-stub/src/main/java/generated-main" \
  --test-output "$TEST_DIR/jv-stub/src/test/java/generated-tests" \
  --fixture-output "$TEST_DIR/jv-stub/src/main/java/generated-fixtures" \
  --jv-write-evolution-dict=true \
  --jv-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true

rsync -a ./test/jv-stub-wiring-overlay/ "$TEST_DIR/jv-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-jv-wiring

Run Java cross-domain muxer + per-domain wiring tests.

```bash
TEST_DIR="${action.test-gen-jv-wiring.test_dir}"
pushd "$TEST_DIR/jv-stub"
mvn -q -Dtest=WiringTests test
popd

ret success:bool=true
```

# action: test-gen-dt-wiring

Generate code for Dart wiring tests in no-errors service-result mode.
Mirrors `test-gen-regular-adt`'s Dart post-processing: the generated runtime
files are moved into the `packages/baboon_runtime` package and the checked-in
hand-written runtime tests are restored after codegen wipes `test/`.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dt-wiring"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/dt-stub"

rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-dt-wiring.lock \
  :dart \
  --output "$TEST_DIR/dt-stub/lib" \
  --test-output "$TEST_DIR/dt-stub/test" \
  --fixture-output "$TEST_DIR/dt-stub/lib" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true

if [ -d "./test/dt-stub/test/runtime" ]; then
  rsync -a ./test/dt-stub/test/runtime/ "$TEST_DIR/dt-stub/test/runtime/"
fi

mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_fixture.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_any_opaque.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_codecs_facade.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_identifier_repr.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

rsync -a ./test/dt-stub-wiring-overlay/ "$TEST_DIR/dt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-dt-wiring

Run Dart cross-domain muxer + per-domain wiring tests.

```bash
TEST_DIR="${action.test-gen-dt-wiring.test_dir}"
pushd "$TEST_DIR/dt-stub"
dart pub get
dart test test/wiring_test.dart
popd

ret success:bool=true
```

# action: test-gen-sw-wiring

Generate code for Swift wiring tests in no-errors service-result mode.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sw-wiring"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"

rsync -a --exclude='.build' --exclude='.swiftpm' \
  ./test/sw-stub/ "$TEST_DIR/sw-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon-sw-wiring.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --test-output "$TEST_DIR/sw-stub/Tests/BaboonTests" \
  --fixture-output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true

# Swift SPM splits Tests/BaboonTests into per-module .testTarget()s; the
# codegen-emitted CrossLanguageFixturePath.swift sits at the top level and must
# be copied into each per-module subdirectory (mirrors test-gen-regular-adt).
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

rsync -a ./test/sw-stub-wiring-overlay/ "$TEST_DIR/sw-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sw-wiring

Run Swift cross-domain muxer + per-domain wiring tests.

```bash
if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

TEST_DIR="${action.test-gen-sw-wiring.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test --filter WiringTests

ret success:bool=true
```

# action: test-gen-sw-wiring-async

Generate the petstore Swift service in ASYNC mode (`--sw-async-services=true`,
no-errors service-result) into the `sw-async` smoke package. Exercises the
async axis of the Swift service generator: `async throws` service protocol
methods, invoke dispatchers, muxer-wrapper thunk, and client.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sw-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-async"

rsync -a --exclude='.build' --exclude='.swiftpm' --exclude='Sources/Generated' \
  ./test/services/sw-async/ "$TEST_DIR/sw-async/"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lock-file="$TEST_DIR/baboon-sw-wiring-async.lock" \
  :swift \
  --output "$TEST_DIR/sw-async/Sources/Generated" \
  --sw-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sw-wiring-async

Build and run the async Swift wiring smoke package: confirms the generated
`async throws` protocol, invoke dispatchers, muxer-wrapper thunk and client all
type-check against the runtime and round-trip in-process (JSON + UEBA).

```bash
if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

TEST_DIR="${action.test-gen-sw-wiring-async.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-async" run AsyncMain

ret success:bool=true
```

# action: test-gen-sw-wiring-errors

Generate the err-carrying petstore Swift service in ERRORS mode
(`--service-result-no-errors=false`, BaboonEither container) into the
`sw-errors` smoke package. Exercises the errors axis of the Swift service
generator that the no-errors matrix never reaches: the service protocol returns
the result CONTAINER (`BaboonEither<Err, Out>`), the wiring threads it via
`rt.leftMap`, and the keyword-bearing `data in` (named `in`) container
annotations are keyword-escaped.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-sw-wiring-errors"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-errors"

rsync -a --exclude='.build' --exclude='.swiftpm' --exclude='Sources/Generated' \
  ./test/services/sw-errors/ "$TEST_DIR/sw-errors/"

$BABOON_BIN \
  --model-dir ./test/services/petstore-errors.baboon \
  --lock-file="$TEST_DIR/baboon-sw-wiring-errors.lock" \
  :swift \
  --output "$TEST_DIR/sw-errors/Sources/Generated" \
  --service-result-no-errors=false \
  --service-result-type=BaboonEither \
  --service-result-pattern="<\$error, \$success>" \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-sw-wiring-errors

Build the errors-mode Swift wiring smoke package: confirms the generated
container-returning service protocol, the errors-mode invoke dispatchers
(JSON + UEBA), the muxer-wrapper and the client all type-check against the
runtime and link. The bar is compile/link (`swift build`); the package also
round-trips in-process when run.

```bash
if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

TEST_DIR="${action.test-gen-sw-wiring-errors.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-errors" build

ret success:bool=true
```

# action: test-gen-graphql

Generate GraphQL SDL schemas and validate them.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-graphql"

rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  :graphql \
  --output "$TEST_DIR" \
  --disable-conversions=true \
  --runtime=without

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-graphql

Validate generated GraphQL schemas using graphql-js buildSchema.

```bash
TEST_DIR="${action.test-gen-graphql.test_dir}"

pushd ./test/gql-stub
npm install
node validate.mjs "../../$TEST_DIR"
popd

ret success:bool=true
```

# action: test-gen-openapi

Generate OpenAPI 3.1 component schemas and validate them.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-openapi"

rm -rf "$TEST_DIR"
mkdir -p "$TEST_DIR"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  :openapi \
  --output "$TEST_DIR" \
  --disable-conversions=true \
  --runtime=without

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-openapi

Validate generated OpenAPI schemas using swagger-parser.

```bash
TEST_DIR="${action.test-gen-openapi.test_dir}"

pushd ./test/oas-stub
npm install
node validate.mjs "../../$TEST_DIR"
popd

ret success:bool=true
```

# action: test-gen-scala-mcp

Generate code for the Scala MCP round-trip overlay test (T12).
Uses the mcp-stub-ok model + `--scala-generate-mcp-server=true` (Either mode) and
overlays `test/scala-stub-mcp-overlay/` on top of a scala-stub-mcp copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-scala-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/scala-stub-mcp"

rsync -a --exclude='target' --exclude='project/target' \
  ./test/scala-stub-mcp/ "$TEST_DIR/scala-stub-mcp/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-scala-mcp.lock \
  :scala \
  --output "$TEST_DIR/scala-stub-mcp/src/main/scala/generated-main" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  "--service-result-pattern=[\$error, \$success]" \
  --scala-generate-mcp-server=true

rsync -a ./test/scala-stub-mcp-overlay/ "$TEST_DIR/scala-stub-mcp/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-scala-mcp

Run the Scala MCP round-trip overlay tests (T12).
Validates initialize/tools-list/tools-call + error paths; runs K1 structural-equality
validation on every returned inputSchema (each inputSchema is a Circe Json value
that must parse and round-trip identically — per-Circe structural equality).

```bash
TEST_DIR="${action.test-gen-scala-mcp.test_dir}"
pushd "$TEST_DIR/scala-stub-mcp"
sbt test
popd

ret success:bool=true
```

# action: test-gen-python-mcp

Generate code for the Python MCP round-trip overlay test (T18).
Uses the mcp-stub-ok model + `--py-generate-mcp-server=true` (Either mode) and
overlays `test/py-stub-mcp-overlay/` on top of a py-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-python-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-python-mcp.lock \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --py-generate-mcp-server=true

rsync -a ./test/py-stub-mcp-overlay/ "$TEST_DIR/py-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-python-mcp

Run the Python MCP round-trip overlay tests (T18).
Validates initialize/tools-list/tools-call + error paths; runs structural-equality
validation on every returned inputSchema against T7 §2.3 reference values (K1);
exercises negative controls.

```bash
TEST_DIR="${action.test-gen-python-mcp.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest BaboonTests.mcp.test_mcp
popd

ret success:bool=true
```

# action: test

Run complete test suite (orchestrator action).

```bash
dep action.test-sbt-basic
dep action.test-graphql
dep action.test-openapi
dep action.test-cs-regular
dep action.test-scala-regular
dep action.test-python-regular
dep action.test-rust-regular
dep action.test-typescript-regular
dep action.test-kotlin-regular
dep action.test-kotlin-kmp-regular
dep action.test-java-regular
dep action.test-dart-regular
dep action.test-swift-regular
dep action.test-cs-wrapped
dep action.test-scala-wrapped
dep action.test-python-wrapped
dep action.test-rust-wrapped
dep action.test-typescript-wrapped
dep action.test-kotlin-wrapped
dep action.test-kotlin-kmp-wrapped
dep action.test-java-wrapped
dep action.test-dart-wrapped
dep action.test-swift-wrapped
dep action.test-manual-cs
dep action.test-manual-scala
dep action.test-manual-python
dep action.test-manual-rust
dep action.test-manual-typescript
dep action.test-manual-kotlin
dep action.test-manual-kotlin-kmp
dep action.test-manual-java
dep action.test-manual-dart
dep action.test-manual-swift
dep action.test-cs-wiring-either
dep action.test-cs-wiring-result
dep action.test-cs-wiring-outcome
dep action.test-sc-wiring-either
dep action.test-sc-wiring-result
dep action.test-sc-wiring-outcome
dep action.test-sc-wiring-hkt
dep action.test-ts-wiring-either
dep action.test-ts-wiring-result
dep action.test-ts-wiring-outcome
dep action.test-ts-mcp
dep action.test-cs-mcp
dep action.test-scala-mcp
dep action.test-rust-mcp
dep action.test-kotlin-mcp
dep action.test-java-mcp
dep action.test-dart-mcp
dep action.test-python-mcp
dep action.test-rs-wiring-either
dep action.test-rs-wiring-result
dep action.test-rs-wiring-outcome
dep action.test-py-wiring-either
dep action.test-py-wiring-result
dep action.test-py-wiring-outcome
dep action.test-py-wiring-async
dep action.test-kt-wiring
dep action.test-jv-wiring
dep action.test-dt-wiring
dep action.test-sw-wiring
dep action.test-sw-wiring-async
dep action.test-sw-wiring-errors

ret success:bool=true
```

# action: test-acceptance

Run cross-language serialization acceptance tests (full cartesian product).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TARGET_DIR="./target/acceptance"

python3 test/acceptance/run_acceptance.py \
  --baboon "$BABOON_BIN" \
  --target "$TARGET_DIR" \
  --parallelism "$(nproc)"

ret success:bool=true
```

# action: test-service-acceptance

Run service-flavour acceptance tests (RPC wiring round-trips). Sibling to
`test-acceptance`; CI runs both. Wraps `test/acceptance/run_service_acceptance.py`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TARGET_DIR="./target/service-acceptance"

python3 test/acceptance/run_service_acceptance.py \
  --baboon "$BABOON_BIN" \
  --target "$TARGET_DIR" \
  --parallelism "$(nproc)"

ret success:bool=true
```

# action: test-gen-cs-client

Generate C# code for the generated-RPC-client round-trip test. Emits BOTH
json + ueba codecs (noErrors flavour) so the generated PetStoreClient exposes
both the UEBA (bare-name) and JSON (Json-suffixed) endpoint methods, plus the
PetStoreWiring server invoker used as the in-process transport.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-client"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ClientRoundTrip"

rsync -a --exclude='Generated' --exclude='bin' --exclude='obj' \
  ./test/services/cs/ClientRoundTrip/ "$TEST_DIR/ClientRoundTrip/"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  :cs \
  --output "$TEST_DIR/ClientRoundTrip/Generated" \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-client

Build and run the C# generated-RPC-client in-process round-trip test
(client-encode -> transport -> server-invoke -> decode, for both JSON and UEBA).

```bash
TEST_DIR="${action.test-gen-cs-client.test_dir}"
pushd "$TEST_DIR/ClientRoundTrip"
dotnet run -c Release
popd

ret success:bool=true
```

# action: test-gen-cs-wiring-async

Generate ASYNC C# service wiring for the petstore model
(`--cs-async-services=true`, noErrors flavour, both codec families). Emits the
`IPetStore` interface with `Task<T>` methods, `PetStoreWiring.Invoke{Json,Ueba}`
as `async Task<...>`, the muxer wrappers parameterised over `Task<...>`, and the
`PetStoreClient` with `async`/`await` over `Task`-returning transport delegates.
A minimal async `IPetStore` impl is scaffolded inline so `dotnet build`
type-checks the emitted async surface (interface conformance, await sites,
delegate signatures).

NOTE: requires the native binary (`action.build`) and dotnet. Authored to
mirror the sync `test-gen-cs-client` lane; not wired into the aggregate `:test`
target yet and UNRUN in CI as of this commit — verified locally only via
`sbt baboonJVM/runMain ...` + `dotnet build`.

```bash
dep action.restore-dotnet

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-wiring-async"

mkdir -p "$TEST_DIR/gen"
rm -rf "$TEST_DIR/gen"
mkdir -p "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  :cs \
  --output "$TEST_DIR/gen" \
  --cs-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

cat > "$TEST_DIR/gen/Build.csproj" <<'EOF'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net9.0</TargetFramework>
    <Nullable>enable</Nullable>
    <LangVersion>latest</LangVersion>
    <GenerateAssemblyInfo>false</GenerateAssemblyInfo>
    <OutputType>Library</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
  </ItemGroup>
</Project>
EOF

cat > "$TEST_DIR/gen/AsyncImpl.cs" <<'EOF'
#nullable enable
using System.Threading.Tasks;
namespace Petstore.Api {
    public sealed class PetStoreAsyncImpl : IPetStore {
        public Task<PetStore.AddPet.Out> addPet(PetStore.AddPet.In arg) => Task.FromResult<PetStore.AddPet.Out>(null!);
        public Task<PetStore.GetPet.Out> getPet(PetStore.GetPet.In arg) => Task.FromResult<PetStore.GetPet.Out>(null!);
        public Task<PetStore.ListPets.Out> listPets(PetStore.ListPets.In arg) => Task.FromResult<PetStore.ListPets.Out>(null!);
        public Task<PetStore.DeletePet.Out> deletePet(PetStore.DeletePet.In arg) => Task.FromResult<PetStore.DeletePet.Out>(null!);
    }
}
EOF

pushd "$TEST_DIR/gen"
dotnet build -c Release Build.csproj
popd

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-jv-client-roundtrip

Exercise the GENERATED Java RPC client (`${Svc}Client`) end to end with an
in-process transport: client encode -> transport -> server
`PetStoreWiring.invoke{Json,Ueba}` -> decode, for BOTH JSON and UEBA. Unlike
the cross-language service-acceptance harness (which hand-rolls codecs over
HTTP), this asserts the emitted client class compiles and round-trips. Runs
the synchronous client (default; `--jv-async-services` left off) generated
with both codec families active.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-jv-client-roundtrip"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-client"
rsync -a --exclude='generated' --exclude='target' \
  ./test/services/jv-client/ "$TEST_DIR/jv-client/"
mkdir -p "$TEST_DIR/model"
cp ./test/services/petstore.baboon "$TEST_DIR/model/"

$BABOON_BIN \
  --model-dir "$TEST_DIR/model" \
  --lock-file="$TEST_DIR/baboon.lock" \
  :java \
  --output "$TEST_DIR/jv-client/src/main/java/generated" \
  --jv-write-evolution-dict=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

pushd "$TEST_DIR/jv-client"
mvn -q compile org.codehaus.mojo:exec-maven-plugin:3.5.0:java
popd

ret success:bool=true
```

# action: test-gen-dart-mcp

Generate code for the Dart MCP round-trip overlay test (T16).
Uses the mcp-stub-ok model + `--dt-generate-mcp-server=true` (no-errors mode) and
overlays `test/dart-stub-mcp-overlay/` on top of a dt-stub copy.
Applies the runtime-file relocation post-codegen step (mv runtime files into
packages/baboon_runtime/lib/) matching the Dart regular-adt harness.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dart-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/dt-stub"

rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/mcp-stub-ok/ \
  --lock-file=./target/baboon-dart-mcp.lock \
  :dart \
  --output "$TEST_DIR/dt-stub/lib" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --dt-generate-mcp-server=true

# Move Dart runtime files into the baboon_runtime package (same as regular-adt)
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_any_opaque.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_codecs_facade.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_identifier_repr.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_mcp_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

# Apply MCP overlay (test file + updated pubspec with collection dep)
rsync -a ./test/dart-stub-mcp-overlay/ "$TEST_DIR/dt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-dart-mcp

Run the Dart MCP round-trip overlay tests (T16).
Validates initialize/tools-list/tools-call + error paths; runs K1 structural-equality
validation on every returned inputSchema (each inputSchema is parsed via dart:convert
`jsonDecode` and compared structurally to the T7 §2.3 reference value).

```bash
TEST_DIR="${action.test-gen-dart-mcp.test_dir}"
pushd "$TEST_DIR/dt-stub"
dart pub get
# Analyze only the generated lib and MCP test directory (runtime/ tests reference
# the full model which is not generated in the MCP-only pass).
dart analyze --fatal-warnings lib/ test/mcp/
dart test test/mcp/mcp_tests.dart
popd

ret success:bool=true
```

# action: test-editors

Test editor extension grammars against real baboon files.
Requires tree-sitter, node, and a C compiler on PATH.

```bash
bash test/editors/test-tree-sitter.sh .

ret success:bool=true
```

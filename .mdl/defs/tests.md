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

Reserved-word coverage (T13): the `reserved-words-ok/` model lives INSIDE the
shared `--model-dir ./baboon-compiler/src/test/resources/baboon/` scanned below,
so its keyword-named ADT branches (`Default`/`Type`/`Object`/`Class`/`When`/
`Match`/`Is`/`In`), enum members (`Type`/`Object`/`Default`/`Is`/`In`/`True`/
`False`), and keyword DTO fields (`default`/`class`/`final`/`void`/`type`/
`true`/`false`/…) are GENERATED into every per-language stub here AND compiled
by the matching `test-<lang>-regular` / `test-<lang>-wrapped` lanes. Both
`test-gen-*-adt` passes set `--generate-json-codecs-by-default=true` and
`--generate-ueba-codecs-by-default=true`, so the codec-capture / branch-lowercase
collision sites (C#/Java/Scala/Kotlin) ARE exercised with codecs ON — no
dedicated `test-gen-reserved` lane is needed (option (a)). The
`ReservedWords*EmissionTest` JVM suite (`baboon-compiler/.jvm/src/test`) is the
fast codegen-shape regression guard for the keyword-escaping fixes (T3-T12).
Do NOT remove `reserved-words-ok/` from the shared model-dir without replacing
this coverage.

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
  --lockfile=./target/baboon.lock \
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
  --lockfile=./target/baboon.lock \
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
dotnet build -c Release -p:Platform="Any CPU"
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
npm run build
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
  --lockfile=./target/baboon.lock \
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
  --lockfile=./target/baboon.lock \
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
dotnet build -c Debug -p:Platform="Any CPU"
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
dotnet build -p:Platform="Any CPU"
dotnet test -p:Platform="Any CPU"
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
  --lockfile=./target/baboon-either.lock \
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
dotnet build -c Release -p:Platform="Any CPU"
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
  --lockfile=./target/baboon-result.lock \
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
dotnet build -c Release -p:Platform="Any CPU"
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
  --lockfile=./target/baboon-outcome.lock \
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
dotnet build -c Release -p:Platform="Any CPU"
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
  --lockfile=./target/baboon-sc-either.lock \
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
  --lockfile=./target/baboon-sc-result.lock \
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
  --lockfile=./target/baboon-sc-outcome.lock \
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
  --lockfile=./target/baboon-sc-hkt.lock \
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
  --lockfile=./target/baboon-ts-either.lock \
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
  --lockfile=./target/baboon-ts-result.lock \
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
  --lockfile=./target/baboon-ts-outcome.lock \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-ts-mcp.lock \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-cs-mcp.lock \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-rust-mcp.lock \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-kotlin-mcp.lock \
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

# action: test-gen-kt-mcp-mux

Generate code for the Kotlin MCP muxer round-trip overlay test (T110).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--kt-generate-mcp-server=true` (Either mode) and overlays
`test/kotlin-stub-mcp-mux-overlay/` on top of a kt-stub copy.
Generated code lands in `src/main/kotlin/generated-main/`.
SYNC ONLY — Kotlin MCP has no async variant (R112 criticism 3).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kt-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-kt-mcp-mux.lock \
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

rsync -a ./test/kotlin-stub-mcp-mux-overlay/ "$TEST_DIR/kt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-kt-mcp-mux

Run the Kotlin MCP muxer round-trip overlay tests (T110).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602)
on the sync path. No async lane — Kotlin MCP is sync-only (R112 criticism 3).

```bash
TEST_DIR="${action.test-gen-kt-mcp-mux.test_dir}"
pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test --tests "mcpmux.McpMuxTests"
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-java-mcp.lock \
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

# action: test-gen-jv-mcp-mux

Generate code for the Java MCP muxer round-trip overlay test (T111, sync only).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--jv-generate-mcp-server=true` (Either mode) and overlays
`test/java-stub-mcp-mux-overlay/` on top of a jv-stub copy.
Generated code lands in `src/main/java/generated-main/`.
SYNC ONLY — Java MCP has no async variant.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-jv-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-jv-mcp-mux.lock \
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

rsync -a ./test/java-stub-mcp-mux-overlay/ "$TEST_DIR/jv-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-jv-mcp-mux

Run the Java MCP muxer round-trip overlay tests (T111, sync).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602).
SYNC ONLY — Java MCP has no async variant.

```bash
TEST_DIR="${action.test-gen-jv-mcp-mux.test_dir}"
pushd "$TEST_DIR/jv-stub"
mvn clean test -Dtest=mcpmux.McpMuxTests
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
  --lockfile=./target/baboon-rs-either.lock \
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
  --lockfile=./target/baboon-rs-result.lock \
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
  --lockfile=./target/baboon-rs-outcome.lock \
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
  --lockfile=./target/baboon-py-either.lock \
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
  --lockfile=./target/baboon-py-result.lock \
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
  --lockfile=./target/baboon-py-outcome.lock \
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
  --lockfile=./target/baboon-py-async.lock \
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

# action: test-gen-ts-wiring-async

Generate async TypeScript service wiring (`--ts-async-services=true`) for the
petstore model into the `ts-async` overlay project. Both JSON and UEBA codec
families are enabled. Generates the `async`-flavour invoke dispatchers
(`invokeJson_PetStore` / `invokeUeba_PetStore` returning `Promise<...>`) and
the service interface with `Promise<Out>` methods.

```bash
BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-async"

rsync -a --exclude='node_modules' --exclude='dist' \
  ./test/services/ts-async/ "$TEST_DIR/ts-async/"

mkdir -p "$TEST_DIR/ts-async/src/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lockfile="$TEST_DIR/baboon-ts-wiring-async.lock" \
  :typescript \
  --output "$TEST_DIR/ts-async/src/generated" \
  --ts-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-wiring-async

Build and run the async TypeScript wiring round-trip: type-checks the generated
`Promise`-returning service interface and async invoke dispatchers via `tsc --noEmit`,
then runs an in-process round-trip (JSON + UEBA) via the generated
`PetStoreClient` backed by the async dispatchers.

```bash
TEST_DIR="${action.test-gen-ts-wiring-async.test_dir}"
pushd "$TEST_DIR/ts-async"
npm install
npm run build
npm test
popd

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
  --lockfile=./target/baboon-kt-wiring.lock \
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
  --lockfile=./target/baboon-jv-wiring.lock \
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

# action: test-gen-jv-wiring-async

Generate ASYNC Java service wiring for the petstore model
(`--jv-async-services=true`, noErrors flavour, both codec families) into the
`jv-async` overlay project. GREEN since T72/T73: the generated `PetStore.java`
interface declares `CompletableFuture<T>` return types and `PetStoreWiring.invokeJson`/
`invokeUeba` compose the async server impl call via `CompletableFuture`. The
companion overlay `PetStoreAsyncImpl` satisfies the async interface; `Driver`
round-trips JSON and UEBA in-process. Companion runner: `test-jv-wiring-async`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-jv-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-async"

rsync -a --exclude='target' \
  ./test/services/jv-async/ "$TEST_DIR/jv-async/"

mkdir -p "$TEST_DIR/jv-async/src/main/java/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lockfile="$TEST_DIR/baboon-jv-async.lock" \
  :java \
  --output "$TEST_DIR/jv-async/src/main/java/generated" \
  --jv-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-jv-wiring-async

Compile and round-trip the async Java wiring lane (D25/T73 GREEN guard).
Builds the overlay project (impl + Driver) against the generated
`--jv-async-services=true` petstore code via `mvn compile`, then runs
the Driver which exercises JSON and UEBA in-process round-trips through
the async dispatchers. All assertions use explicit throws (no vacuous
Java asserts).

```bash
TEST_DIR="${action.test-gen-jv-wiring-async.test_dir}"
pushd "$TEST_DIR/jv-async"
mvn -q compile org.codehaus.mojo:exec-maven-plugin:3.5.0:java
popd

ret success:bool=true
```

# action: test-gen-kt-wiring-async

Generate ASYNC Kotlin service wiring for the petstore model
(`--kt-async-services=true`, noErrors flavour, both codec families) into the
`kt-async` overlay project. GREEN since T76/T77: the generated `PetStore.kt`
interface declares `suspend fun` methods and `PetStoreWiring.invokeJson`/
`invokeUeba` are `suspend fun`s that `await` the impl call. The companion
overlay `PetStoreAsyncImpl` uses `suspend fun` overrides that satisfy the
generated interface; `Driver` round-trips JSON and UEBA in-process.
Companion runner: `test-kt-wiring-async`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kt-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-async"

rsync -a --exclude='build' --exclude='.gradle' \
  ./test/services/kt-async/ "$TEST_DIR/kt-async/"

mkdir -p "$TEST_DIR/kt-async/src/main/kotlin/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lockfile="$TEST_DIR/baboon-kt-async.lock" \
  :kotlin \
  --output "$TEST_DIR/kt-async/src/main/kotlin/generated" \
  --kt-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-kt-wiring-async

Compile and round-trip the async Kotlin wiring lane (D25/T77 GREEN guard).
Builds the overlay project (impl + Driver) against the generated
`--kt-async-services=true` petstore code via `gradle --no-daemon run`, which
compiles the Kotlin sources and then runs the Driver that exercises JSON and
UEBA in-process round-trips through the async suspend dispatchers. All
assertions use explicit throws (no vacuous Kotlin asserts).

```bash
TEST_DIR="${action.test-gen-kt-wiring-async.test_dir}"
pushd "$TEST_DIR/kt-async"
gradle --no-daemon run
popd

ret success:bool=true
```

# action: test-gen-kt-wiring-errors-async

Generate ERRORS+ASYNC Kotlin service wiring for the petstore-errors model
(`--kt-async-services=true`, errors flavour with `Either<err,success>` container,
both codec families) into the `kt-errors-async` overlay project.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kt-wiring-errors-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-errors-async"

rsync -a --exclude='build' --exclude='.gradle' \
  ./test/services/kt-errors-async/ "$TEST_DIR/kt-errors-async/"

mkdir -p "$TEST_DIR/kt-errors-async/src/main/kotlin/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore-errors.baboon \
  --lockfile="$TEST_DIR/baboon-kt-errors-async.lock" \
  :kotlin \
  --output "$TEST_DIR/kt-errors-async/src/main/kotlin/generated" \
  --kt-async-services=true \
  --service-result-no-errors=false \
  --service-result-type=Either \
  '--service-result-pattern=<$error,$success>' \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-kt-wiring-errors-async

Compile the errors+async Kotlin server impl overlay against the generated
`--kt-async-services=true --service-result-no-errors=false` petstore-errors
code (T93: GREEN after the suspend-lambda fix for D26).

```bash
TEST_DIR="${action.test-gen-kt-wiring-errors-async.test_dir}"
pushd "$TEST_DIR/kt-errors-async"
gradle --no-daemon compileKotlin
popd

ret success:bool=true
```

# action: test-gen-jv-wiring-errors-async

Generate ERRORS+ASYNC Java service wiring for the petstore-errors model
(`--jv-async-services=true`, errors flavour with `BaboonEither<err,success>`
container, both codec families) into the `jv-errors-async` overlay project.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-jv-wiring-errors-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-errors-async"

rsync -a --exclude='target' \
  ./test/services/jv-errors-async/ "$TEST_DIR/jv-errors-async/"

mkdir -p "$TEST_DIR/jv-errors-async/src/main/java/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore-errors.baboon \
  --lockfile="$TEST_DIR/baboon-jv-errors-async.lock" \
  :java \
  --output "$TEST_DIR/jv-errors-async/src/main/java/generated" \
  --jv-async-services=true \
  --service-result-no-errors=false \
  --service-result-type="baboon.runtime.shared.BaboonEither" \
  '--service-result-pattern=<$error,$success>' \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-jv-wiring-errors-async

Compile the errors+async Java server impl overlay against the generated
`--jv-async-services=true --service-result-no-errors=false` petstore-errors
code (T94: GREEN after the async-errors double-wrap fix for D26).

```bash
TEST_DIR="${action.test-gen-jv-wiring-errors-async.test_dir}"
pushd "$TEST_DIR/jv-errors-async"
mvn compile
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
  --lockfile=./target/baboon-dt-wiring.lock \
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

# action: test-gen-dt-wiring-async

Generate ASYNC Dart service wiring for the petstore model
(`--dt-async-services=true`, noErrors flavour, both codec families) into the
`dt-async` overlay project. GREEN since T80/T81: the generated `pet_store.dart`
abstract class declares `Future<T>` return types and `PetStoreWiring.invokeJson`/
`invokeUeba` are `async` functions that `await` the impl call. The companion
overlay `PetStoreAsyncImpl` returns `Future<T>` for each method and passes
`dart analyze` with no errors. Companion runner: `test-dt-wiring-async`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dt-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/dt-async"

rsync -a --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/services/dt-async/ "$TEST_DIR/dt-async/"

mkdir -p "$TEST_DIR/dt-async/lib/generated"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lockfile="$TEST_DIR/baboon-dt-async.lock" \
  :dart \
  --output "$TEST_DIR/dt-async/lib/generated" \
  --dt-async-services=true \
  --service-result-no-errors=true \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

mv "$TEST_DIR/dt-async/lib/generated/baboon_runtime.dart" \
   "$TEST_DIR/dt-async/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-async/lib/generated/baboon_any_opaque.dart" \
   "$TEST_DIR/dt-async/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-async/lib/generated/baboon_codecs_facade.dart" \
   "$TEST_DIR/dt-async/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-async/lib/generated/baboon_identifier_repr.dart" \
   "$TEST_DIR/dt-async/packages/baboon_runtime/lib/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-dt-wiring-async

Analyze the async Dart server impl overlay against the generated
`--dt-async-services=true` petstore code (D25/T81 GREEN guard). With the fix,
the generated `pet_store.dart` abstract class declares `Future<T>` methods so
the overlay `PetStoreAsyncImpl` returning `Future<T>` passes `dart analyze`
with no errors. Verification scope: `dart analyze lib/petstore_async_impl.dart`
(single-file analyze, mirrors the original repro scope).

```bash
TEST_DIR="${action.test-gen-dt-wiring-async.test_dir}"
pushd "$TEST_DIR/dt-async"
dart pub get
dart analyze lib/petstore_async_impl.dart
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
  --lockfile=./target/baboon-sw-wiring.lock \
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
  --lockfile="$TEST_DIR/baboon-sw-wiring-async.lock" \
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
  --lockfile="$TEST_DIR/baboon-sw-wiring-errors.lock" \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-scala-mcp.lock \
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

# action: test-gen-scala-mcp-mux

Generate code for the Scala MCP muxer round-trip overlay test (T107).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--scala-generate-mcp-server=true` (Either mode) and overlays
`test/scala-stub-mcp-mux-overlay/` on top of a scala-stub-mcp-mux copy.
Generated code lands in `src/main/scala/generated-main/`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-scala-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/scala-stub-mcp-mux"

rsync -a --exclude='target' --exclude='project/target' \
  ./test/scala-stub-mcp-mux/ "$TEST_DIR/scala-stub-mcp-mux/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-scala-mcp-mux.lock \
  :scala \
  --output "$TEST_DIR/scala-stub-mcp-mux/src/main/scala/generated-main" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  "--service-result-pattern=[\$error, \$success]" \
  --scala-generate-mcp-server=true

rsync -a ./test/scala-stub-mcp-mux-overlay/ "$TEST_DIR/scala-stub-mcp-mux/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-scala-mcp-mux

Run the Scala MCP muxer round-trip overlay tests (T107).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602)
on the Either (sync) path. No async lane — Scala MCP is Either-only (D24/T69).

```bash
TEST_DIR="${action.test-gen-scala-mcp-mux.test_dir}"
pushd "$TEST_DIR/scala-stub-mcp-mux"
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-python-mcp.lock \
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

# action: test-diff

End-to-end smoke test for the `:diff` CLI (T173, goal G29). Runs the built
native binary against the shared model-dir, diffing `testpkg.pkg0` from 2.0.0
to 3.0.0, in both text and JSON output modes. Pins that the three types
introduced in v3 — `S1` (contract), `I1` (root service), `T7_D1` (root data)
— surface in each mode's `added` set. Checks are UNCONDITIONAL `if … exit 1`
guards (NOT shell asserts, which are vacuous) so a regression fails the lane.

The compiler prints a fixed log banner + input listing to stdout ahead of the
diff payload (no quiet/log-level flag exists), so JSON well-formedness is
validated against the trailing JSON object (from its first `{`-column line to
EOF, extracted with `sed`) rather than the whole raw stdout.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
MODEL_DIR="./baboon-compiler/src/test/resources/baboon/"

# --- Text mode -----------------------------------------------------------
TEXT_OUT="$("$BABOON_BIN" --model-dir "$MODEL_DIR" :diff --domain testpkg.pkg0 --from 2.0.0 --to 3.0.0)"
TEXT_RC=$?
if [ "$TEXT_RC" -ne 0 ]; then
  echo "FAIL: :diff (text) exited with $TEXT_RC" >&2
  echo "$TEXT_OUT" >&2
  exit 1
fi
for name in S1 I1 T7_D1; do
  if ! printf '%s' "$TEXT_OUT" | grep -qF "$name"; then
    echo "FAIL: :diff (text) output missing added type '$name'" >&2
    echo "$TEXT_OUT" >&2
    exit 1
  fi
done

# --- JSON mode -----------------------------------------------------------
JSON_OUT="$("$BABOON_BIN" --model-dir "$MODEL_DIR" :diff --domain testpkg.pkg0 --from 2.0.0 --to 3.0.0 --format json)"
JSON_RC=$?
if [ "$JSON_RC" -ne 0 ]; then
  echo "FAIL: :diff (json) exited with $JSON_RC" >&2
  echo "$JSON_OUT" >&2
  exit 1
fi
# Extract the trailing JSON object (first `{`-column line .. EOF), skipping the
# compiler's stdout log banner, then validate well-formedness (python3 is
# available in all test lanes).
JSON_BODY="$(printf '%s\n' "$JSON_OUT" | sed -n '/^{/,$p')"
if ! printf '%s' "$JSON_BODY" | python3 -c 'import sys, json; json.load(sys.stdin)'; then
  echo "FAIL: :diff (json) did not emit valid JSON" >&2
  echo "$JSON_OUT" >&2
  exit 1
fi
for name in S1 I1 T7_D1; do
  if ! printf '%s' "$JSON_OUT" | grep -qF "$name"; then
    echo "FAIL: :diff (json) output missing added type '$name'" >&2
    echo "$JSON_OUT" >&2
    exit 1
  fi
done

echo "test-diff: :diff text + json both surface S1, I1, T7_D1"
ret success:bool=true
```

# action: test-diff-ref

End-to-end smoke test for the `:diff --from <v>@<ref>` code path (T198, goal G33).
Creates a self-contained throwaway git repo in a temp dir, commits version 1.0.0 of
a small baboon fixture, then commits version 2.0.0 (a derivable change that adds
`AddedRecord`). Runs the built native binary against the temp repo with
`--from 1.0.0@HEAD~1 --to 2.0.0`, which pins the from-side at the prior commit via
the T192 `GitModelMaterializer` and loads the to-side from the working tree. Asserts
that `AddedRecord` (added in v2) appears in the diff output. Also asserts that no
leftover detached git worktrees remain after the run (leak check: the bracket in
`GitModelMaterializer` must remove the worktree even on success). Guards on git
availability: if git is not on the PATH the lane skips rather than hard-failing.
Checks are UNCONDITIONAL `if … exit 1` guards (not shell asserts, which are vacuous).

```bash
dep action.build

# Guard: skip if git is not available.
if ! command -v git &>/dev/null; then
  echo "test-diff-ref: git not found on PATH, skipping"
  ret success:bool=true
  exit 0
fi

BABOON_BIN="${action.build.binary}"

# --- Create a self-contained throwaway git repo in a temp dir ----------------
DIFF_REPO="$(mktemp -d)"
# Do NOT install an EXIT trap here: mudyla's runtime.sh already sets
# `trap 'mudyla_write_outputs' EXIT` to write output.json on exit, and a second
# `trap ... EXIT` REPLACES it — output.json would never be written ("No output.json
# generated"). Clean the temp repo explicitly on every exit path instead.
cleanup_diff_repo() { [ -n "${DIFF_REPO:-}" ] && rm -rf "$DIFF_REPO"; }

git -C "$DIFF_REPO" init -q
git -C "$DIFF_REPO" config user.email "test@baboon-ci"
git -C "$DIFF_REPO" config user.name  "Baboon CI"

# Commit 1 (HEAD~1): v1.0.0 model only — BaseRecord.
# Only v1.baboon exists; baboon loads it and finds version 1.0.0.
cat > "$DIFF_REPO/v1.baboon" << 'BABOON_V1'
model diff.ref.fixture

version "1.0.0"

root data BaseRecord {
  name: str
  value: i32
}
BABOON_V1

git -C "$DIFF_REPO" add v1.baboon
git -C "$DIFF_REPO" commit -q -m "v1: BaseRecord"

# Commit 2 (HEAD): v1.baboon unchanged; v2.baboon added (AddedRecord — derivable change).
# Both files are present so baboon finds version 1.0.0 (for the import) and 2.0.0.
cat > "$DIFF_REPO/v2.baboon" << 'BABOON_V2'
model diff.ref.fixture

version "2.0.0"

import "1.0.0" { * }

root data AddedRecord {
  tag: str
  count: i32
}
BABOON_V2

git -C "$DIFF_REPO" add v2.baboon
git -C "$DIFF_REPO" commit -q -m "v2: AddedRecord"

# --- Run :diff with @ref pinning the from-side at HEAD~1 ---------------------
# set-e-safe capture (a plain VAR="$(cmd)" aborts under `set -e` if cmd exits nonzero).
if DIFF_OUT="$("$BABOON_BIN" --model-dir "$DIFF_REPO" :diff --domain diff.ref.fixture --from 1.0.0@HEAD~1 --to 2.0.0 2>&1)"; then DIFF_RC=0; else DIFF_RC=$?; fi
if [ "$DIFF_RC" -ne 0 ]; then
  echo "FAIL: :diff --from 1.0.0@HEAD~1 exited with $DIFF_RC" >&2
  echo "$DIFF_OUT" >&2
  cleanup_diff_repo
  exit 1
fi

# Assert the added type surfaces in the output.
if ! printf '%s' "$DIFF_OUT" | grep -qF "AddedRecord"; then
  echo "FAIL: :diff --from 1.0.0@HEAD~1 output missing added type 'AddedRecord'" >&2
  echo "$DIFF_OUT" >&2
  cleanup_diff_repo
  exit 1
fi

# --- Worktree leak check: no leftover detached worktrees ---------------------
# git worktree list prints one line per worktree; the main checkout is always
# present. Any extra line means a worktree was not cleaned up.
WT_COUNT="$(git -C "$DIFF_REPO" worktree list --porcelain | grep -c '^worktree ')"
if [ "$WT_COUNT" -ne 1 ]; then
  echo "FAIL: expected 1 worktree entry after diff, got $WT_COUNT (worktree leak)" >&2
  git -C "$DIFF_REPO" worktree list >&2
  cleanup_diff_repo
  exit 1
fi

cleanup_diff_repo
echo "test-diff-ref: :diff --from 1.0.0@HEAD~1 surfaces AddedRecord; no worktree leak"
ret success:bool=true
```

# action: test-no-args-help

Smoke test for D42 Part A: `baboon` with no arguments must print help text and
exit 0. Uses the built native binary. Checks are unconditional `if … exit 1`
guards (not shell asserts, which are vacuous) so a regression fails the lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"

HELP_OUT="$("$BABOON_BIN" 2>/dev/null)"
HELP_RC=$?
if [ "$HELP_RC" -ne 0 ]; then
  echo "FAIL: baboon (no args) exited with $HELP_RC, expected 0" >&2
  echo "$HELP_OUT" >&2
  exit 1
fi
if ! printf '%s' "$HELP_OUT" | grep -qF "Usage: baboon"; then
  echo "FAIL: baboon (no args) did not print help text (missing 'Usage: baboon')" >&2
  echo "$HELP_OUT" >&2
  exit 1
fi

echo "test-no-args-help: baboon with no args prints help and exits 0"
ret success:bool=true
```
# action: test-bincompat

End-to-end smoke test for the `:bincompat` modality (T196, goal G34). Validates exit
codes against the three T193 fixture pairs in `evo-classify-ok/{safe-add,derivable-change,
non-derivable-change}`. Each pair holds two versions (v1.baboon and v2.baboon) of one
domain, with expected verdicts:
- `safe-add`: no existing type is altered — **exit 0** (NoBreak).
- `derivable-change`: adds opt field + new ADT branch — **exit 1** (Derivable).
- `non-derivable-change`: removes enum branch — **exit 2** (NonDerivable / CustomConversionRequired).

Runs the built native binary with `--model-dir` (GLOBAL arg, BEFORE `:bincompat`),
`:bincompat --domain <name> --from 1.0.0 --to 2.0.0` (LOCAL args), and unconditionally
asserts exit codes via `if [ "$RC" -ne <expected> ]; then exit 1; fi` guards (not vacuous
shell asserts, so a regression fails the lane).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
BASE_MODEL_DIR="./baboon-compiler/src/test/resources/evo-classify-ok"

# --- Test 1: safe-add → exit 0 (NoBreak) ---
# NOTE: :bincompat exits NONZERO (1/2) on its success path (the verdict is the
# machine signal), so the exit code MUST be captured in a way that does not trip
# `set -e` — a plain `VAR="$(cmd)"` assignment aborts the whole script under `set -e`
# when cmd exits nonzero, BEFORE `$?` can be read. The `if VAR=$(...); then ... else RC=$?`
# form is exempt from `set -e` for the tested command and captures the real exit code.
if SAFE_ADD_OUT="$("$BABOON_BIN" --model-dir "$BASE_MODEL_DIR/safe-add" :bincompat --domain evo.classify.safe_add --from 1.0.0 --to 2.0.0 2>&1)"; then SAFE_ADD_RC=0; else SAFE_ADD_RC=$?; fi
if [ "$SAFE_ADD_RC" -ne 0 ]; then
  echo "FAIL: :bincompat safe-add exited with $SAFE_ADD_RC, expected 0 (NoBreak)" >&2
  echo "$SAFE_ADD_OUT" >&2
  exit 1
fi

# --- Test 2: derivable-change → exit 1 (Derivable) ---
if DERIVABLE_OUT="$("$BABOON_BIN" --model-dir "$BASE_MODEL_DIR/derivable-change" :bincompat --domain evo.classify.derivable_change --from 1.0.0 --to 2.0.0 2>&1)"; then DERIVABLE_RC=0; else DERIVABLE_RC=$?; fi
if [ "$DERIVABLE_RC" -ne 1 ]; then
  echo "FAIL: :bincompat derivable-change exited with $DERIVABLE_RC, expected 1 (Derivable)" >&2
  echo "$DERIVABLE_OUT" >&2
  exit 1
fi

# --- Test 3: non-derivable-change → exit 2 (NonDerivable) ---
if NON_DERIVABLE_OUT="$("$BABOON_BIN" --model-dir "$BASE_MODEL_DIR/non-derivable-change" :bincompat --domain evo.classify.non_derivable_change --from 1.0.0 --to 2.0.0 2>&1)"; then NON_DERIVABLE_RC=0; else NON_DERIVABLE_RC=$?; fi
if [ "$NON_DERIVABLE_RC" -ne 2 ]; then
  echo "FAIL: :bincompat non-derivable-change exited with $NON_DERIVABLE_RC, expected 2 (NonDerivable)" >&2
  echo "$NON_DERIVABLE_OUT" >&2
  exit 1
fi

echo "test-bincompat: safe-add→0, derivable-change→1, non-derivable-change→2 [OK]"
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
dep action.test-ts-wiring-collision
dep action.test-ts-mcp
dep action.test-ts-mcp-mux
dep action.test-ts-mcp-mux-async
dep action.test-cs-mcp-mux
dep action.test-cs-mcp-mux-async
dep action.test-py-mcp-mux
dep action.test-py-mcp-mux-async
dep action.test-cs-mcp
dep action.test-scala-mcp
dep action.test-scala-mcp-mux
dep action.test-rust-mcp
dep action.test-rs-mcp-mux
dep action.test-rs-mcp-mux-async
dep action.test-kotlin-mcp
dep action.test-kt-mcp-mux
dep action.test-java-mcp
dep action.test-jv-mcp-mux
dep action.test-dart-mcp
dep action.test-dt-mcp-mux
dep action.test-python-mcp
dep action.test-swift-mcp
dep action.test-ts-mcp-async
dep action.test-cs-mcp-async
dep action.test-rust-mcp-async
dep action.test-python-mcp-async
dep action.test-swift-mcp-async
dep action.test-swift-mcp-mux
dep action.test-swift-mcp-mux-async
dep action.test-rs-wiring-either
dep action.test-rs-wiring-result
dep action.test-rs-wiring-outcome
dep action.test-py-wiring-either
dep action.test-py-wiring-result
dep action.test-py-wiring-outcome
dep action.test-py-wiring-async
dep action.test-ts-wiring-async
dep action.test-cs-wiring-async
dep action.test-jv-wiring-async
dep action.test-kt-wiring-async
dep action.test-dt-wiring-async
dep action.test-kt-wiring
dep action.test-jv-wiring
dep action.test-dt-wiring
dep action.test-sw-wiring
dep action.test-sw-wiring-async
dep action.test-sw-wiring-errors
dep action.test-kt-wiring-errors-async
dep action.test-jv-wiring-errors-async
dep action.test-cs-wiring-errors-async
dep action.test-rs-wiring-async
dep action.test-rs-wiring-async-errors
dep action.test-diff
dep action.test-diff-ref
dep action.test-no-args-help
dep action.test-bincompat

# D40/T182: zero-service MCP lanes — permanent regression guard that the MCP
# runtime is emitted (and the overlay compiles) even for a model with @root
# types but NO RPC block. gen-only lanes (post-fix positive-generation check)
# + self-contained overlay run lanes (empty-muxer runtime assertions).
dep action.test-gen-ts-mcp-zero
dep action.test-gen-cs-mcp-zero
dep action.test-gen-rust-mcp-zero
dep action.test-gen-kotlin-mcp-zero
dep action.test-gen-java-mcp-zero
dep action.test-gen-scala-mcp-zero
dep action.test-gen-python-mcp-zero
dep action.test-gen-dart-mcp-zero
dep action.test-gen-swift-mcp-zero
dep action.test-ts-mcp-zero
dep action.test-cs-mcp-zero
dep action.test-rust-mcp-zero
dep action.test-kotlin-mcp-zero
dep action.test-java-mcp-zero
dep action.test-scala-mcp-zero
dep action.test-python-mcp-zero
dep action.test-dart-mcp-zero
dep action.test-swift-mcp-zero

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
A minimal async `IPetStore` stub is scaffolded inline so `dotnet build`
type-checks the emitted async surface (interface conformance, await sites,
delegate signatures). Companion runner: `test-cs-wiring-async`.

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
        public Task<PetStore.AddPet.Out> AddPet(PetStore.AddPet.In arg) => Task.FromResult<PetStore.AddPet.Out>(null!);
        public Task<PetStore.GetPet.Out> GetPet(PetStore.GetPet.In arg) => Task.FromResult<PetStore.GetPet.Out>(null!);
        public Task<PetStore.ListPets.Out> ListPets(PetStore.ListPets.In arg) => Task.FromResult<PetStore.ListPets.Out>(null!);
        public Task<PetStore.DeletePet.Out> DeletePet(PetStore.DeletePet.In arg) => Task.FromResult<PetStore.DeletePet.Out>(null!);
    }
}
EOF

pushd "$TEST_DIR/gen"
dotnet build -c Release Build.csproj
popd

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wiring-async

Build and run the async C# wiring round-trip: type-checks the generated
`Task<T>`-returning service interface and async invoke dispatchers via
`dotnet build`, then runs an in-process round-trip (JSON + UEBA) via the
generated `PetStoreClient` backed by the async dispatchers.

```bash
TEST_DIR="${action.test-gen-cs-wiring-async.test_dir}"

cat > "$TEST_DIR/gen/Build.csproj" <<'EOF'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net9.0</TargetFramework>
    <Nullable>enable</Nullable>
    <LangVersion>latest</LangVersion>
    <GenerateAssemblyInfo>false</GenerateAssemblyInfo>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
  </ItemGroup>
</Project>
EOF

cat > "$TEST_DIR/gen/Driver.cs" <<'EOF'
#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
using Petstore.Api;

// In-process async round-trip driver for the C# async wiring lane.
// Exercises the generated PetStoreClient (async transport delegates) end-to-end
// via both JSON and UEBA codecs, calling through the async invoke dispatchers.
namespace AsyncDriver {
    public sealed class Impl : IPetStore {
        private readonly Dictionary<long, Pet> _pets = new();
        private long _next = 1;

        public Task<PetStore.AddPet.Out> AddPet(PetStore.AddPet.In arg) {
            long id = _next++;
            Pet pet = new Pet(id, arg.Name, arg.Status, arg.Tag);
            _pets[id] = pet;
            return Task.FromResult(new PetStore.AddPet.Out(pet));
        }

        public Task<PetStore.GetPet.Out> GetPet(PetStore.GetPet.In arg) =>
            Task.FromResult(new PetStore.GetPet.Out(_pets[arg.Id]));

        public Task<PetStore.ListPets.Out> ListPets(PetStore.ListPets.In arg) =>
            Task.FromResult(new PetStore.ListPets.Out(_pets.Values.OrderBy(p => p.Id).ToList()));

        public Task<PetStore.DeletePet.Out> DeletePet(PetStore.DeletePet.In arg) =>
            Task.FromResult(new PetStore.DeletePet.Out(_pets.Remove(arg.Id)));
    }

    public static class Driver {
        public static async Task<int> Main() {
            BaboonCodecContext ctx = BaboonCodecContext.Default;
            await RunUeba(ctx);
            await RunJson(ctx);
            Console.WriteLine("OK");
            return 0;
        }

        private static async Task RunUeba(BaboonCodecContext ctx) {
            Impl impl = new Impl();
            PetStoreClient client = new PetStoreClient(
                async (svc, method, data) => await PetStoreWiring.InvokeUeba(new BaboonMethodId(svc, method), data, impl, ctx),
                async (svc, method, data) => await PetStoreWiring.InvokeJson(new BaboonMethodId(svc, method), data, impl, ctx)
            );

            PetStore.AddPet.Out addOut = await client.AddPet(new PetStore.AddPet.In("Buddy", PetStatus.Available, "dog"), ctx);
            Assert(addOut.Pet.Name == "Buddy", $"UEBA AddPet name: {addOut.Pet.Name}");
            long id = addOut.Pet.Id;

            PetStore.GetPet.Out getOut = await client.GetPet(new PetStore.GetPet.In(id), ctx);
            Assert(getOut.Pet.Status == PetStatus.Available, "UEBA GetPet status");

            PetStore.ListPets.Out listOut = await client.ListPets(new PetStore.ListPets.In(), ctx);
            Assert(listOut.Pets.Count == 1, $"UEBA ListPets count: {listOut.Pets.Count}");

            PetStore.DeletePet.Out delOut = await client.DeletePet(new PetStore.DeletePet.In(id), ctx);
            Assert(delOut.Deleted, "UEBA DeletePet");

            Console.WriteLine("UEBA round-trip OK");
        }

        private static async Task RunJson(BaboonCodecContext ctx) {
            Impl impl = new Impl();
            PetStoreClient client = new PetStoreClient(
                async (svc, method, data) => await PetStoreWiring.InvokeUeba(new BaboonMethodId(svc, method), data, impl, ctx),
                async (svc, method, data) => await PetStoreWiring.InvokeJson(new BaboonMethodId(svc, method), data, impl, ctx)
            );

            PetStore.AddPet.Out addOut = await client.AddPetJson(new PetStore.AddPet.In("Whiskers", PetStatus.Pending, "cat"), ctx);
            Assert(addOut.Pet.Name == "Whiskers", $"JSON AddPet name: {addOut.Pet.Name}");
            long id = addOut.Pet.Id;

            PetStore.GetPet.Out getOut = await client.GetPetJson(new PetStore.GetPet.In(id), ctx);
            Assert(getOut.Pet.Status == PetStatus.Pending, "JSON GetPet status");

            PetStore.ListPets.Out listOut = await client.ListPetsJson(new PetStore.ListPets.In(), ctx);
            Assert(listOut.Pets.Count == 1, $"JSON ListPets count: {listOut.Pets.Count}");

            PetStore.DeletePet.Out delOut = await client.DeletePetJson(new PetStore.DeletePet.In(id), ctx);
            Assert(delOut.Deleted, "JSON DeletePet");

            Console.WriteLine("JSON round-trip OK");
        }

        private static void Assert(bool cond, string msg) {
            if (!cond) {
                Console.Error.WriteLine("ASSERT FAILED: " + msg);
                Environment.Exit(1);
            }
        }
    }
}
EOF

pushd "$TEST_DIR/gen"
dotnet build -c Release Build.csproj
dotnet run --project Build.csproj -c Release --no-build
popd

ret success:bool=true
```

# action: test-gen-cs-wiring-errors-async

Generate ERRORS+ASYNC C# service wiring for the petstore-errors model
(`--cs-async-services=true`, errors flavour with `Baboon.Runtime.Shared.Either<err,success>`
container, both codec families). Emits the `IPetStore` interface with
`Task<Either<Err, Out>>` methods, `PetStoreWiring.InvokeJson/InvokeUeba`
as `async Task<Either<BaboonWiringError, string/byte[]>>`, the muxer
wrappers parameterised over `Task<Either<…>>`, and the `PetStoreClient`
with `async`/`await` over `Task`-returning transport delegates.
This is the GENERATE-ONLY step: it ends after baboon (no compile).
The companion runner `test-cs-wiring-errors-async` compiles and round-trips.

This is the CONTROL lane for D26: C# errors+async is already sound via
`generateErrorsJsonCaseAsync` (the await-then-thread linear path).

Wired into the aggregate `:test` target (T95).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-wiring-errors-async"

mkdir -p "$TEST_DIR/gen"
rm -rf "$TEST_DIR/gen"
mkdir -p "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./test/services/petstore-errors.baboon \
  :cs \
  --output "$TEST_DIR/gen" \
  --cs-async-services=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  '--service-result-pattern=<$error, $success>' \
  --generate-json-codecs-by-default=true \
  --generate-ueba-codecs-by-default=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-wiring-errors-async

Build and round-trip the ERRORS+ASYNC C# wiring control lane (D26/T92).
Deps the gen action, scaffolds an errors+async impl and an in-process
driver, then:
  - `dotnet build` (type-checks the emitted async surface, including the
    `generateErrorsJsonCaseAsync` await-then-`rt.LeftMap` path)
  - runs the driver executable which exercises the success path
    (impl returns `Right(out)` → wiring encodes to JSON → driver
    decodes and asserts) and the error path (impl returns `Left(err)` →
    `rt.LeftMap` converts to `BaboonWiringError.CallFailed` → driver
    asserts Left).

This lane is GREEN — C# errors+async is already sound. It is the C#
control lane for the D26 errors+async wiring contract (alongside the
now-green Kotlin/Java errors+async lanes fixed in T93/T94).

```bash
TEST_DIR="${action.test-gen-cs-wiring-errors-async.test_dir}"

cat > "$TEST_DIR/gen/Build.csproj" <<'EOF'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net9.0</TargetFramework>
    <Nullable>enable</Nullable>
    <LangVersion>latest</LangVersion>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
  </ItemGroup>
</Project>
EOF

cat > "$TEST_DIR/gen/AsyncErrorsImpl.cs" <<'EOF'
#nullable enable
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
namespace Petstore.Api {
    // Success impl: always returns Right(out).
    public sealed class PetStoreSuccessImpl : IPetStore {
        private long _nextId = 1;
        public Task<Either<PetStore.AddPet.Err, PetStore.AddPet.Out>> AddPet(PetStore.AddPet.In arg) {
            long id = _nextId++;
            var pet = new Pet(id, arg.Name, arg.Status, arg.Tag);
            return Task.FromResult(Either.Right<PetStore.AddPet.Err, PetStore.AddPet.Out>(new PetStore.AddPet.Out(pet)));
        }
        public Task<Either<PetStore.GetPet.Err, PetStore.GetPet.Out>> GetPet(PetStore.GetPet.In arg) {
            var pet = new Pet(arg.Id, "Rex", PetStatus.Available, null);
            return Task.FromResult(Either.Right<PetStore.GetPet.Err, PetStore.GetPet.Out>(new PetStore.GetPet.Out(pet)));
        }
    }

    // Error impl: always returns Left(err).
    public sealed class PetStoreErrorImpl : IPetStore {
        public Task<Either<PetStore.AddPet.Err, PetStore.AddPet.Out>> AddPet(PetStore.AddPet.In arg) {
            return Task.FromResult(Either.Left<PetStore.AddPet.Err, PetStore.AddPet.Out>(new PetStore.AddPet.Err(new PetError(42, "test error"))));
        }
        public Task<Either<PetStore.GetPet.Err, PetStore.GetPet.Out>> GetPet(PetStore.GetPet.In arg) {
            return Task.FromResult(Either.Left<PetStore.GetPet.Err, PetStore.GetPet.Out>(new PetStore.GetPet.Err(new PetError(42, "test error"))));
        }
    }
}
EOF

cat > "$TEST_DIR/gen/Driver.cs" <<'EOF'
#nullable enable
using System;
using System.IO;
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using Petstore.Api;

// In-process round-trip driver for the errors+async C# wiring control lane.
// Exercises generateErrorsJsonCaseAsync (the await-then-rt.LeftMap path):
//   - Success path: impl returns Right(out) -> wiring encodes to JSON -> Right(jsonString)
//   - Error path:   impl returns Left(err)  -> rt.LeftMap -> Left(BaboonWiringError.CallFailed)
public static class Driver {
    public static async Task<int> Main() {
        var ctx = BaboonCodecContext.Default;
        var rt  = BaboonServiceRtDefault.Instance;
        var addPetMethod = new BaboonMethodId("PetStore", "addPet");
        var getPetMethod = new BaboonMethodId("PetStore", "getPet");

        // --- Success path (JSON) ---
        {
            var impl = new PetStoreSuccessImpl();
            var inArg = new PetStore.AddPet.In("Buddy", PetStatus.Available, "dog");
            var encoded = PetStore.AddPet.In_JsonCodec.Instance.Encode(ctx, inArg).ToString(Newtonsoft.Json.Formatting.None);
            var result = await PetStoreWiring.InvokeJson(addPetMethod, encoded, impl, rt, ctx);
            if (result is not Either<BaboonWiringError, string>.Right successResult) {
                Console.Error.WriteLine($"JSON SUCCESS PATH FAILED: expected Right, got {result}");
                return 1;
            }
            Console.WriteLine($"JSON success path OK: {successResult.Value}");
        }

        // --- Error path (JSON) ---
        {
            var impl = new PetStoreErrorImpl();
            var inArg = new PetStore.AddPet.In("Ghost", PetStatus.Sold, null);
            var encoded = PetStore.AddPet.In_JsonCodec.Instance.Encode(ctx, inArg).ToString(Newtonsoft.Json.Formatting.None);
            var result = await PetStoreWiring.InvokeJson(addPetMethod, encoded, impl, rt, ctx);
            if (result is not Either<BaboonWiringError, string>.Left errResult) {
                Console.Error.WriteLine($"JSON ERROR PATH FAILED: expected Left, got {result}");
                return 1;
            }
            if (errResult.Value is not BaboonWiringError.CallFailed callFailed) {
                Console.Error.WriteLine($"JSON ERROR PATH FAILED: expected CallFailed, got {errResult.Value}");
                return 1;
            }
            Console.WriteLine($"JSON error path OK: CallFailed domain error = {callFailed.DomainError}");
        }

        // --- Success path (UEBA) ---
        {
            var impl = new PetStoreSuccessImpl();
            var inArg = new PetStore.GetPet.In(1L);
            var ms = new MemoryStream();
            var bw = new BinaryWriter(ms);
            PetStore.GetPet.In_UEBACodec.Instance.Encode(ctx, bw, inArg);
            bw.Flush();
            var result = await PetStoreWiring.InvokeUeba(getPetMethod, ms.ToArray(), impl, rt, ctx);
            if (result is not Either<BaboonWiringError, byte[]>.Right uebaSuccess) {
                Console.Error.WriteLine($"UEBA SUCCESS PATH FAILED: expected Right, got {result}");
                return 1;
            }
            Console.WriteLine($"UEBA success path OK: {uebaSuccess.Value.Length} bytes");
        }

        Console.WriteLine("ALL OK");
        return 0;
    }
}
EOF

pushd "$TEST_DIR/gen"
dotnet build -c Release Build.csproj
dotnet run --project Build.csproj -c Release --no-build
popd

ret success:bool=true
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
  --lockfile="$TEST_DIR/baboon.lock" \
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
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-dart-mcp.lock \
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

# action: test-gen-dt-mcp-mux

Generate code for the Dart MCP muxer round-trip overlay test (T112).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--dt-generate-mcp-server=true` (no-errors mode) and overlays
`test/dart-stub-mcp-mux-overlay/` on top of a dt-stub copy.
Applies the runtime-file relocation post-codegen step (mv runtime files into
packages/baboon_runtime/lib/) matching the Dart MCP harness.
No async lane — Dart MCP is sync-only (R112 criticism 3).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dt-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/dt-stub"

rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-dt-mcp-mux.lock \
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

# Apply mux overlay (mux test file)
rsync -a ./test/dart-stub-mcp-mux-overlay/ "$TEST_DIR/dt-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-dt-mcp-mux

Run the Dart MCP muxer round-trip overlay tests (T112).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602)
on the sync path. No async lane — Dart MCP is sync-only.

```bash
TEST_DIR="${action.test-gen-dt-mcp-mux.test_dir}"
pushd "$TEST_DIR/dt-stub"
dart pub get
# Analyze only the generated lib and mux test directory.
dart analyze --fatal-warnings lib/ test/mcp_mux/
dart test test/mcp_mux/mcp_mux_tests.dart
popd

ret success:bool=true
```

# action: test-gen-swift-mcp

Generate code for the Swift MCP round-trip overlay test (T17).
Uses the mcp-stub-ok model + `--sw-generate-mcp-server=true` (no-errors mode) and
assembles a self-contained Swift package: the generated `Sources/BaboonRuntime`
(including the additive `baboon_mcp_runtime.swift`, emitted only when the flag is on)
and `Sources/McpStub` (definitions + JSON/UEBA codecs + service wiring + the
per-service `McpToolsMcpServer`), then overlays `test/swift-stub-mcp-overlay/`
(Package.swift + the McpTests target).

Applies the Swift fixture fan-out post-codegen step the regular-adt harness uses
(CrossLanguageFixturePath.swift into each `Tests/BaboonTests/<Module>/`); this pass
emits no `--test-output` so the helper is absent and the step is a guarded no-op,
kept for parity with the regular-adt harness.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"
mkdir -p "$TEST_DIR/sw-stub/Sources"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-swift-mcp.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true

# Swift SPM splits Tests/BaboonTests into per-module .testTarget()s; the
# codegen-emitted CrossLanguageFixturePath.swift sits at the top level and must
# be copied into each per-module subdirectory (mirrors test-gen-regular-adt).
# This MCP pass emits no test product, so the helper is absent — guarded no-op.
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

# Apply MCP overlay (Package.swift + McpTests target).
rsync -a ./test/swift-stub-mcp-overlay/ "$TEST_DIR/sw-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-swift-mcp

Run the Swift MCP round-trip overlay tests (T17).
Validates initialize/tools-list/tools-call + error paths; runs the K1 validity tier on
every returned inputSchema (each is re-serialized/re-parsed via JSONSerialization for
well-formedness AND deep-compared structurally to the embedded T7 §2.3 reference
literal, with a live negative control). Channel-B is triggered with a non-object nested
field (Swift force-unwrap traps, so the missing-field trigger used by the GC-language
replicas is deliberately avoided).

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

TEST_DIR="${action.test-gen-swift-mcp.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-ts-mcp-zero

Generate code for the TypeScript zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-ts-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` (declares @root types, NO RPC block) and
emits into an isolated output dir. Every other flag — including
`--ts-generate-mcp-server=true` and the Either service-result mode — is
identical to `test-gen-ts-mcp`. This is a gen-only RED-baseline lane (no overlay
copy, no run lane): pre-fix, servicesOf(domain) is empty so NO MCP runtime file
is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-ts-mcp-zero.lock \
  :typescript \
  --output "$TEST_DIR/gen" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --ts-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-cs-mcp-zero

Generate code for the C# zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-cs-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--cs-generate-mcp-server=true` and the Either
service-result mode — is identical to `test-gen-cs-mcp`. Gen-only RED-baseline
lane: pre-fix, NO `BaboonMcpRuntime.cs` is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-cs-mcp-zero.lock \
  :cs \
  --output "$TEST_DIR/gen" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  --service-result-pattern="<\$error, \$success>" \
  --cs-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-rust-mcp-zero

Generate code for the Rust zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-rust-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--rs-generate-mcp-server=true` and the Result
service-result mode — is identical to `test-gen-rust-mcp`. Gen-only RED-baseline
lane: pre-fix, NO MCP runtime file is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rust-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-rust-mcp-zero.lock \
  :rust \
  --output "$TEST_DIR/gen" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type=Result \
  '--service-result-pattern=<$success, $error>' \
  --rs-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-kotlin-mcp-zero

Generate code for the Kotlin zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-kotlin-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--kt-generate-mcp-server=true` and the Either
service-result mode — is identical to `test-gen-kotlin-mcp`. Gen-only
RED-baseline lane: pre-fix, NO MCP runtime file is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kotlin-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-kotlin-mcp-zero.lock \
  :kotlin \
  --output "$TEST_DIR/gen" \
  --kt-write-evolution-dict=true \
  --kt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  '--service-result-pattern=<$error, $success>' \
  --kt-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-java-mcp-zero

Generate code for the Java zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-java-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--jv-generate-mcp-server=true` and the Either
service-result mode — is identical to `test-gen-java-mcp`. Gen-only RED-baseline
lane: pre-fix, none of the 14 Java MCP runtime files are emitted into the
output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-java-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-java-mcp-zero.lock \
  :java \
  --output "$TEST_DIR/gen" \
  --jv-write-evolution-dict=true \
  --jv-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="baboon.runtime.shared.BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --jv-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-scala-mcp-zero

Generate code for the Scala zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-scala-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--scala-generate-mcp-server=true` and the Either
service-result mode (D24: Scala MCP requires Either) — is identical to
`test-gen-scala-mcp`. Gen-only RED-baseline lane: pre-fix, NO MCP runtime file
is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-scala-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-scala-mcp-zero.lock \
  :scala \
  --output "$TEST_DIR/gen" \
  --sc-write-evolution-dict=true \
  --sc-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Either" \
  "--service-result-pattern=[\$error, \$success]" \
  --scala-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-python-mcp-zero

Generate code for the Python zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-python-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--py-generate-mcp-server=true` and the Either
service-result mode — is identical to `test-gen-python-mcp`. Gen-only
RED-baseline lane: pre-fix, NO MCP runtime file is emitted into the output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-python-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-python-mcp-zero.lock \
  :python \
  --output "$TEST_DIR/gen" \
  --py-write-evolution-dict=true \
  --py-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --py-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-dart-mcp-zero

Generate code for the Dart zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-dart-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--dt-generate-mcp-server=true` and the no-errors
service-result mode — is identical to `test-gen-dart-mcp`. Gen-only RED-baseline
lane: pre-fix, NO `baboon_mcp_runtime.dart` is emitted into the output dir
(hence the sibling lane's post-codegen `mv` of that runtime file is deliberately
NOT replicated here — there is nothing to move).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dart-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-dart-mcp-zero.lock \
  :dart \
  --output "$TEST_DIR/gen" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --dt-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-gen-swift-mcp-zero

Generate code for the Swift zero-service MCP RED baseline (D40/T177).
Sibling of `test-gen-swift-mcp` but points `--model-dir` at the zero-service
fixture `mcp-stub-zero-services-ok/` and emits into an isolated output dir.
Every other flag — including `--sw-generate-mcp-server=true` and the no-errors
service-result mode — is identical to `test-gen-swift-mcp`. Gen-only
RED-baseline lane: pre-fix, NO `baboon_mcp_runtime.swift` is emitted into the
output dir.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp-zero"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/gen"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-swift-mcp-zero.lock \
  :swift \
  --output "$TEST_DIR/gen" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-mcp-zero

Run the C# ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-cs-mcp`, but gens the zero-service fixture
`mcp-stub-zero-services-ok/` (declares @root types, NO RPC block) into a full
cs-stub copy and overlays `test/cs-stub-mcp-zero-overlay/`. This lane is
SELF-CONTAINED (it gens + overlays + compiles, exactly like `test-gen-cs-mcp`
+ `test-cs-mcp` folded into one) — the T177 `test-gen-cs-mcp-zero` gen-only lane
is a separate bare-dir RED-baseline gen check.

The overlay test imports ONLY the static MCP runtime namespace
`Baboon.Runtime.Shared` (AbstractMcpMuxer / McpServerInfo / McpSession /
JsonRpcRequest / JsonRpcResponse) — with zero services there is NO generated
`<Service>McpServer` to reference, so those types resolve ONLY from the static
`BaboonMcpRuntime.cs`. PRE-FIX (RED) the generator emits NO `BaboonMcpRuntime.cs`
for a zero-service model, so the overlay FAILS TO COMPILE with CS0246/CS0234
(missing runtime type / missing namespace) — the D40 reproduction. POST-FIX the
runtime is emitted, the overlay compiles, and the empty-muxer runtime assertions
(tools/list empty; unknown-tool call → -32602) pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-cs-mcp-zero-overlay.lock \
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

rsync -a ./test/cs-stub-mcp-zero-overlay/ "$TEST_DIR/cs-stub/"

pushd "$TEST_DIR/cs-stub"
dotnet build -c Release McpZeroTests/McpZeroTests.csproj
dotnet test -c Release McpZeroTests/McpZeroTests.csproj
popd

ret success:bool=true
```

# action: test-rust-mcp-zero

Run the Rust ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-rust-mcp`; gens the zero-service fixture into a rs-stub copy
(Cargo.toml only, generated tree into src/) and overlays
`test/rust-stub-mcp-zero-overlay/`. Self-contained (gen + overlay + compile).

The overlay imports ONLY the static runtime module
`baboon_rs_stub::baboon_mcp_server` (AbstractMcpMuxer / McpServerInfo /
McpSession / JsonRpcRequest / json_rpc_error_codes) — with zero services there is
NO `<Service>McpServer` module, so the runtime resolves ONLY from the static
`baboon_mcp_server.rs` (and its `pub mod` declaration). PRE-FIX (RED) that file
is NOT emitted, so `cargo test` FAILS with E0432/E0433 (unresolved import /
failed to resolve module `baboon_mcp_server`) — the D40 reproduction. POST-FIX
the runtime is emitted and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rust-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"
mkdir -p "$TEST_DIR/rs-stub"

cp ./test/rs-stub/Cargo.toml "$TEST_DIR/rs-stub/Cargo.toml"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-rust-mcp-zero-overlay.lock \
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

rsync -a ./test/rust-stub-mcp-zero-overlay/ "$TEST_DIR/rs-stub/"

pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test --test mcp_zero_tests
popd

ret success:bool=true
```

# action: test-kotlin-mcp-zero

Run the Kotlin ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-kotlin-mcp`; gens the zero-service fixture into a kt-stub copy
and overlays `test/kotlin-stub-mcp-zero-overlay/`. Self-contained.

The overlay imports ONLY the static runtime package `baboon.runtime.shared`
(AbstractMcpMuxer / McpServerInfo / McpSession / JsonRpcRequest) — with zero
services there is NO generated `<Service>McpServer`, so those types resolve ONLY
from the static `BaboonMcpRuntime.kt`. PRE-FIX (RED) that file is NOT emitted, so
`gradle test` FAILS to compile (unresolved reference: AbstractMcpMuxer) — the D40
reproduction. POST-FIX the runtime is emitted and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-kotlin-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/kt-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='build' --exclude='.gradle' \
  ./test/kt-stub/ "$TEST_DIR/kt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-kotlin-mcp-zero-overlay.lock \
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

rsync -a ./test/kotlin-stub-mcp-zero-overlay/ "$TEST_DIR/kt-stub/"

pushd "$TEST_DIR/kt-stub"
gradle --no-daemon clean test --tests "mcpzero.McpZeroTests"
popd

ret success:bool=true
```

# action: test-java-mcp-zero

Run the Java ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-java-mcp`; gens the zero-service fixture into a jv-stub copy
and overlays `test/java-stub-mcp-zero-overlay/`. Self-contained.

The overlay imports ONLY the static runtime package `baboon.runtime.shared`
(AbstractMcpMuxer / McpServerInfo / McpSession / JsonRpcRequest / JsonRpcResponse)
— with zero services there is NO generated `<Service>McpServer`, so those types
resolve ONLY from the 14 static `baboon.runtime.shared` MCP `.java` files.
PRE-FIX (RED) NONE of those files are emitted, so `mvn test` FAILS to compile
(`package baboon.runtime.shared does not exist` / `cannot find symbol
AbstractMcpMuxer`) — the D40 reproduction. POST-FIX the runtime is emitted and
the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-java-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/jv-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='target' \
  ./test/jv-stub/ "$TEST_DIR/jv-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-java-mcp-zero-overlay.lock \
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

rsync -a ./test/java-stub-mcp-zero-overlay/ "$TEST_DIR/jv-stub/"

pushd "$TEST_DIR/jv-stub"
mvn clean test -Dtest=mcpzero.McpZeroTests
popd

ret success:bool=true
```

# action: test-scala-mcp-zero

Run the Scala ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-scala-mcp`; gens the zero-service fixture into a scala-stub-mcp
copy and overlays `test/scala-stub-mcp-zero-overlay/`. Self-contained.
(D24: Scala MCP requires Either service-result mode.)

The overlay imports ONLY the static runtime package `baboon.runtime.shared`
(AbstractMcpMuxer / McpServerInfo / McpSession / JsonRpcRequest) — with zero
services there is NO generated `<Service>McpServer`, so those types resolve ONLY
from the static `BaboonMcpRuntime.scala`. PRE-FIX (RED) that file is NOT emitted,
so `sbt test` FAILS to compile (not found: type AbstractMcpMuxer) — the D40
reproduction. POST-FIX the runtime is emitted and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-scala-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/scala-stub-mcp"

rsync -a --exclude='target' --exclude='project/target' \
  ./test/scala-stub-mcp/ "$TEST_DIR/scala-stub-mcp/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-scala-mcp-zero-overlay.lock \
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

rsync -a ./test/scala-stub-mcp-zero-overlay/ "$TEST_DIR/scala-stub-mcp/"

pushd "$TEST_DIR/scala-stub-mcp"
sbt test
popd

ret success:bool=true
```

# action: test-python-mcp-zero

Run the Python ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-python-mcp`; gens the zero-service fixture into a py-stub copy
and overlays `test/py-stub-mcp-zero-overlay/`. Self-contained.

The overlay imports ONLY the static runtime module `baboon_mcp_runtime`
(AbstractMcpMuxer / McpServerInfo / McpSession / JsonRpcRequest) — with zero
services there is NO generated `<service>_mcp_server` module, so those symbols
resolve ONLY from the static `baboon_mcp_runtime.py`. PRE-FIX (RED) that file is
NOT emitted, so the test FAILS with `ModuleNotFoundError: baboon_mcp_runtime`
(collected as an import error, not a test failure) — the D40 reproduction.
POST-FIX the runtime is emitted and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-python-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-python-mcp-zero-overlay.lock \
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

rsync -a ./test/py-stub-mcp-zero-overlay/ "$TEST_DIR/py-stub/"

pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest BaboonTests.mcp_zero.test_mcp_zero
popd

ret success:bool=true
```

# action: test-ts-mcp-zero

Run the TypeScript ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-ts-mcp`; gens the zero-service fixture into a ts-stub copy and
overlays `test/ts-stub-mcp-zero-overlay/`. Self-contained.

The overlay imports ONLY the static runtime module `BaboonMcpRuntime`
(AbstractMcpMuxer / McpServerInfo / McpSession / JsonRpcRequest / JsonRpcResponse
/ JsonRpcErrorCodes) — with zero services there is NO generated per-service
`mcp-server` module, so those symbols resolve ONLY from the static
`BaboonMcpRuntime.ts`. PRE-FIX (RED) that file is NOT emitted, so `vitest`/`tsc`
FAILS with "Cannot find module './Generated/BaboonMcpRuntime'" — the D40
reproduction. POST-FIX the runtime is emitted and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-ts-mcp-zero-overlay.lock \
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

rsync -a ./test/ts-stub-mcp-zero-overlay/ "$TEST_DIR/ts-stub/"

pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/mcp.zero.test.ts
popd

ret success:bool=true
```

# action: test-dart-mcp-zero

Run the Dart ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-dart-mcp` (no-errors mode); gens the zero-service fixture into a
dt-stub copy and overlays `test/dart-stub-mcp-zero-overlay/`. Self-contained.

The sibling `test-gen-dart-mcp` lane `mv`s `baboon_mcp_runtime.dart` into the
baboon_runtime package post-codegen; POST-FIX (D40/T179) the zero-service model
emits that runtime file unconditionally, so — like the sibling — it is moved into
the baboon_runtime package here. The other four runtime files are moved as usual
so the domain lib still resolves.

The overlay imports ONLY the static runtime `package:baboon_runtime/
baboon_mcp_runtime.dart` (AbstractMcpMuxer / McpServerInfo / McpSession /
JsonRpcRequest / JsonRpcResponse) — with zero services there is NO generated
`<service>_mcp_server`, so those symbols resolve ONLY from that static file.
PRE-FIX (RED, T178) it was NOT emitted, so `dart analyze`/`dart test` FAILED with
"Target of URI doesn't exist: 'package:baboon_runtime/baboon_mcp_runtime.dart'"
(and undefined class AbstractMcpMuxer) — the D40 reproduction. POST-FIX the
runtime is emitted and moved, and the empty-muxer assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-dart-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/dt-stub"

rsync -a --exclude='generated-*' --exclude='.dart_tool' --exclude='pubspec.lock' \
  ./test/dt-stub/ "$TEST_DIR/dt-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-dart-mcp-zero-overlay.lock \
  :dart \
  --output "$TEST_DIR/dt-stub/lib" \
  --dt-write-evolution-dict=true \
  --dt-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --dt-generate-mcp-server=true

# Move the domain runtime files into the baboon_runtime package (same as regular-adt).
# POST-FIX (D40/T179): baboon_mcp_runtime.dart is now emitted unconditionally on a
# zero-service model, so it is moved into the package like the sibling test-gen-dart-mcp lane.
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_any_opaque.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_codecs_facade.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_identifier_repr.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_mcp_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

rsync -a ./test/dart-stub-mcp-zero-overlay/ "$TEST_DIR/dt-stub/"

pushd "$TEST_DIR/dt-stub"
dart pub get
dart analyze --fatal-warnings lib/ test/mcp_zero/
dart test test/mcp_zero/mcp_zero_tests.dart
popd

ret success:bool=true
```

# action: test-swift-mcp-zero

Run the Swift ZERO-SERVICE MCP overlay test (D40/T178) — RED baseline.
Sibling of `test-swift-mcp` (no-errors mode); gens the zero-service fixture into
a self-contained Swift package and overlays `test/swift-stub-mcp-zero-overlay/`.
Self-contained.

The overlay imports the generated `BaboonRuntime` module and references ONLY the
static MCP runtime types (AbstractMcpMuxer / McpServerInfo / McpSession /
JsonRpcRequest / JsonRpcResponse) that live in `baboon_mcp_runtime.swift`. With
zero services there is NO generated `<Service>McpServer`, so those types come
ONLY from that static file. PRE-FIX (RED) `baboon_mcp_runtime.swift` is NOT
emitted, so `swift test` FAILS with "cannot find 'AbstractMcpMuxer' in scope" —
the D40 reproduction. POST-FIX the runtime is emitted and the empty-muxer
assertions pass.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp-zero-overlay"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"
mkdir -p "$TEST_DIR/sw-stub/Sources"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-zero-services-ok/ \
  --lockfile=./target/baboon-swift-mcp-zero-overlay.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true

# Swift SPM per-module fixture fan-out (mirrors test-gen-swift-mcp) — guarded no-op
# on this MCP pass which emits no --test-output helper.
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

rsync -a ./test/swift-stub-mcp-zero-overlay/ "$TEST_DIR/sw-stub/"

if ! command -v swift &> /dev/null; then
  if [[ "$(uname)" == "Linux" ]]; then
    echo "Swift is required on Linux but was not found in PATH" >&2
    exit 1
  fi
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-ts-mcp-async

Generate code for the TypeScript ASYNC MCP round-trip overlay test (D24/G11).
Async sibling of `test-gen-ts-mcp`: uses the mcp-stub-ok model + BOTH
`--ts-generate-mcp-server=true` AND `--ts-async-services=true`, and overlays
`test/ts-stub-mcp-async-overlay/` on top of a ts-stub copy.

GREEN since T65: under `--ts-async-services=true` `TsMcpServerGenerator` emits the
async server (extends `AbstractAsyncBaboonMcpServer`, `Promise`-returning delegate,
`async handle`/`await`). DO NOT modify the sync `test-gen-ts-mcp` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-ts-mcp-async.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/baboondefinitions/generated" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --ts-generate-mcp-server=true \
  --ts-async-services=true

rsync -a ./test/ts-stub-mcp-async-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-mcp-async

Run the TypeScript ASYNC MCP round-trip overlay tests (D24/G11).
Async sibling of `test-ts-mcp`. GREEN since T65: under
`--ts-async-services=true` the wiring `invokeJson_McpTools` is async and returns
`Promise<BaboonEither<…>>`, and `TsMcpServerGenerator` now emits the generated
`McpToolsMcpServer` extending `AbstractAsyncBaboonMcpServer` with a
`Promise`-returning delegate (`=> Promise<BaboonEitherResult>`) and an async
`invokeJson`/inherited `handle`. The `tsc --noEmit` build step typechecks the
binding; vitest awaits a `tools/call` round-trip and asserts `{"ok":true}`. DO
NOT modify the sync `test-ts-mcp` lane.

```bash
TEST_DIR="${action.test-gen-ts-mcp-async.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
# Type gate: the Promise-returning delegate now typechecks against the async server.
npm run build
npx vitest run src/mcp.test.ts
popd

ret success:bool=true
```

# action: test-gen-cs-mcp-async

Generate code for the C# ASYNC MCP round-trip overlay test (D24/G11).
Async sibling of `test-gen-cs-mcp`: uses the mcp-stub-ok model + BOTH
`--cs-generate-mcp-server=true` AND `--cs-async-services=true` (Either mode), and
overlays `test/cs-stub-mcp-async-overlay/` on top of a cs-stub copy.

Scaffold only — the async-MCP C# backend fix has not landed; this lane is
expected RED until it does. DO NOT modify the sync `test-gen-cs-mcp` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-cs-mcp-async.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  --service-result-pattern="<\$error, \$success>" \
  --cs-generate-mcp-server=true \
  --cs-async-services=true

rsync -a ./test/cs-stub-mcp-async-overlay/ "$TEST_DIR/cs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-mcp-async

Run the C# ASYNC MCP round-trip overlay tests (D24/G11).
Async sibling of `test-cs-mcp`. Scaffold only — expected RED until the
C# async-MCP backend fix lands.

```bash
TEST_DIR="${action.test-gen-cs-mcp-async.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release McpTests/McpTests.csproj
dotnet test -c Release McpTests/McpTests.csproj
popd

ret success:bool=true
```

# action: test-gen-rust-mcp-async

Generate code for the Rust ASYNC MCP round-trip overlay test (D24/G11).
Async sibling of `test-gen-rust-mcp`: uses the mcp-stub-ok model + BOTH
`--rs-generate-mcp-server=true` AND `--rs-async-services=true` (Result errors
mode), and overlays `test/rust-stub-mcp-async-overlay/` on top of a rs-stub copy.

GREEN since T98/D28 fixed the async-MCP Rust backend: the async `McpJsonInvoke`
alias and `block_on` driver now compile clean under `-D warnings`. See
`test/rust-stub-mcp-async-overlay/README.md` for the full history.
DO NOT modify the sync `test-gen-rust-mcp` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rust-mcp-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"
mkdir -p "$TEST_DIR/rs-stub"

# Copy only the Cargo.toml (package name = baboon-rs-stub) from rs-stub;
# the generated source tree from mcp-stub-ok will be written into src/.
cp ./test/rs-stub/Cargo.toml "$TEST_DIR/rs-stub/Cargo.toml"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-rust-mcp-async.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type=Result \
  '--service-result-pattern=<$success, $error>' \
  --rs-generate-mcp-server=true \
  --rs-async-services=true

rsync -a ./test/rust-stub-mcp-async-overlay/ "$TEST_DIR/rs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rust-mcp-async

Run the Rust ASYNC MCP round-trip overlay tests (D24/G11).
Async sibling of `test-rust-mcp`. GREEN since T98/D28 landed the async-MCP
Rust backend fix (`McpJsonInvoke` async alias + `block_on` driver).

```bash
TEST_DIR="${action.test-gen-rust-mcp-async.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test --test mcp_tests
popd

ret success:bool=true
```

# action: test-gen-rs-wiring-async

Generate Rust ASYNC service wiring for the petstore model (`--rs-async-services=true`,
no-errors flavour, both codec families) into an isolated `rs-async` project copied from
`test/services/rs-async/`. GREEN since T97/D28 fixed the async wiring (Clone bounds
on muxer generics + sized error type in client `?`). Companion runner: `test-rs-wiring-async`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-wiring-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-async"

rsync -a ./test/services/rs-async/ "$TEST_DIR/rs-async/"

mkdir -p "$TEST_DIR/rs-async/src"

$BABOON_BIN \
  --model-dir ./test/services/petstore.baboon \
  --lockfile="$TEST_DIR/baboon-rs-wiring-async.lock" \
  :rust \
  --output "$TEST_DIR/rs-async/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --rs-async-services=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-wiring-async

Run `cargo build` for the async Rust no-errors petstore service wiring (D28/T97 GREEN).

```bash
TEST_DIR="${action.test-gen-rs-wiring-async.test_dir}"
pushd "$TEST_DIR/rs-async"
RUSTFLAGS="-D warnings" cargo build
popd

ret success:bool=true
```

# action: test-gen-rs-wiring-async-errors

Generate Rust ASYNC service wiring for the errors-mode petstore model
(`--rs-async-services=true`, errors flavour with Result container, both codec families)
into an isolated `rs-async-errors` project copied from `test/services/rs-async-errors/`.
GREEN since T97/D28 fixed the async errors wiring (async invoke body, Clone bounds,
sized error type). Companion runner: `test-rs-wiring-async-errors`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-wiring-async-errors"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-async-errors"

rsync -a ./test/services/rs-async-errors/ "$TEST_DIR/rs-async-errors/"

mkdir -p "$TEST_DIR/rs-async-errors/src"

$BABOON_BIN \
  --model-dir ./test/services/petstore-errors.baboon \
  --lockfile="$TEST_DIR/baboon-rs-wiring-async-errors.lock" \
  :rust \
  --output "$TEST_DIR/rs-async-errors/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type=Result \
  '--service-result-pattern=<$success, $error>' \
  --rs-async-services=true

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-wiring-async-errors

Run `cargo build` for the async Rust errors-mode petstore service wiring (D28/T97 GREEN).

```bash
TEST_DIR="${action.test-gen-rs-wiring-async-errors.test_dir}"
pushd "$TEST_DIR/rs-async-errors"
RUSTFLAGS="-D warnings" cargo build
popd

ret success:bool=true
```

# action: test-gen-python-mcp-async

Generate code for the Python ASYNC MCP round-trip overlay test (D24/G11).
Async sibling of `test-gen-python-mcp`: uses the mcp-stub-ok model + BOTH
`--py-generate-mcp-server=true` AND `--py-async-services=true` (Either mode), and
overlays `test/py-stub-mcp-async-overlay/` on top of a py-stub copy.

GREEN since T61 threaded `asyncServices` into `PyMcpServerGenerator` (async
server base that awaits the delegate). DO NOT modify the sync
`test-gen-python-mcp` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-python-mcp-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-python-mcp-async.lock \
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
  --py-generate-mcp-server=true \
  --py-async-services=true

rsync -a ./test/py-stub-mcp-async-overlay/ "$TEST_DIR/py-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-python-mcp-async

Run the Python ASYNC MCP round-trip overlay tests (D24/G11).
Async sibling of `test-python-mcp`. GREEN since the T61 async-MCP Python backend
fix (async server base that awaits the delegate).

```bash
TEST_DIR="${action.test-gen-python-mcp-async.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest BaboonTests.mcp.test_mcp
popd

ret success:bool=true
```

# action: test-gen-swift-mcp-async

Generate code for the Swift ASYNC MCP round-trip overlay test (D24/G11).
Async sibling of `test-gen-swift-mcp`: uses the mcp-stub-ok model + BOTH
`--sw-generate-mcp-server=true` AND `--sw-async-services=true` (no-errors mode),
and overlays `test/swift-stub-mcp-async-overlay/` on top of a sw-stub copy
(same self-contained Swift-package assembly as the sync lane).

Scaffold only — the async-MCP Swift backend fix has not landed; this lane is
expected RED until it does. DO NOT modify the sync `test-gen-swift-mcp` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"
mkdir -p "$TEST_DIR/sw-stub/Sources"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-stub-ok/ \
  --lockfile=./target/baboon-swift-mcp-async.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true \
  --sw-async-services=true

# Swift SPM splits Tests/BaboonTests into per-module .testTarget()s; the
# codegen-emitted CrossLanguageFixturePath.swift sits at the top level and must
# be copied into each per-module subdirectory (mirrors test-gen-regular-adt).
# This MCP pass emits no test product, so the helper is absent — guarded no-op.
SW_BTESTS="$TEST_DIR/sw-stub/Tests/BaboonTests"
if [ -f "$SW_BTESTS/CrossLanguageFixturePath.swift" ]; then
  for sub in "$SW_BTESTS"/*/; do
    [ -d "$sub" ] || continue
    cp "$SW_BTESTS/CrossLanguageFixturePath.swift" "$sub"
  done
  rm -f "$SW_BTESTS/CrossLanguageFixturePath.swift"
fi

# Apply async MCP overlay (Package.swift + McpTests target).
rsync -a ./test/swift-stub-mcp-async-overlay/ "$TEST_DIR/sw-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-swift-mcp-async

Run the Swift ASYNC MCP round-trip overlay tests (D24/G11).
Async sibling of `test-swift-mcp`. Scaffold only — expected RED until the
Swift async-MCP backend fix lands.

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

TEST_DIR="${action.test-gen-swift-mcp-async.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-swift-mcp-mux

Generate code for the Swift MCP muxer round-trip test (T113, sync).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--sw-generate-mcp-server=true` (no-errors mode) and assembles a self-contained
Swift package: the generated `Sources/BaboonRuntime` (including the additive
baboon_mcp_runtime.swift, which carries AbstractMcpMuxer + the
DuplicateTool/NoMatchingTool taxonomy, emitted only when the MCP flag is on) and
`Sources/McpMuxStub` (the two @root services + their wiring + the per-service
UserServiceMcpServer / OrderServiceMcpServer), then overlays
`test/swift-stub-mcp-mux-overlay/` (Package.swift + the McpMuxTests target).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"
mkdir -p "$TEST_DIR/sw-stub/Sources"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-swift-mcp-mux.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true

# Apply MCP muxer overlay (Package.swift + McpMuxTests target).
rsync -a ./test/swift-stub-mcp-mux-overlay/ "$TEST_DIR/sw-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-swift-mcp-mux

Run the Swift MCP muxer round-trip tests (T113, sync).
Validates tools/list union across UserService + OrderService in
registration-then-declaration order, per-service routing (Channel-A success +
per-service Channel-B), DuplicateTool on collision, and NoMatchingTool (-32602)
against `AbstractMcpMuxer`. Composes the two generated servers strictly through
the public T114 routable surface (`AnyRoutableMcpServer`); never a member's
handle(). Assertions are unconditional XCTest checks (Swift `assert` is vacuous).

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

TEST_DIR="${action.test-gen-swift-mcp-mux.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-swift-mcp-mux-async

Generate code for the Swift ASYNC MCP muxer round-trip test (T113).
Async sibling of `test-gen-swift-mcp-mux`: uses the mcp-mux-stub-ok model + BOTH
`--sw-generate-mcp-server=true` AND `--sw-async-services=true` (no-errors mode),
and overlays `test/swift-stub-mcp-mux-async-overlay/` on top of a sw-stub copy.
Under the async axis the generated `<Svc>Wiring.invokeJson` is
`async throws -> String` and the generated servers conform to
`IBaboonAsyncMcpServer`, so the test composes them behind `AbstractAsyncMcpMuxer`
(`async handle`, `async throws routeToolCall`). DO NOT modify the sync
`test-gen-swift-mcp-mux` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-swift-mcp-mux-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/sw-stub"
mkdir -p "$TEST_DIR/sw-stub/Sources"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-swift-mcp-mux-async.lock \
  :swift \
  --output "$TEST_DIR/sw-stub/Sources" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=true \
  --sw-generate-mcp-server=true \
  --sw-async-services=true

# Apply async MCP muxer overlay (Package.swift + McpMuxTests target).
rsync -a ./test/swift-stub-mcp-mux-async-overlay/ "$TEST_DIR/sw-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-swift-mcp-mux-async

Run the Swift ASYNC MCP muxer round-trip tests (T113).
Async sibling of `test-swift-mcp-mux`. Validates the SAME four muxer behaviours
through the genuinely-async `AbstractAsyncMcpMuxer` (each `tools/call` awaits the
owning server's `routeToolCall` before Channel-A/B), plus an @MainActor-isolated
round-trip (cooperative suspension, not a blocking bridge). Assertions are
unconditional XCTest checks.

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

TEST_DIR="${action.test-gen-swift-mcp-mux-async.test_dir}"
./scripts/swift-xcode.sh "$TEST_DIR/sw-stub" test

ret success:bool=true
```

# action: test-gen-ts-mcp-mux

Generate code for the TypeScript MCP muxer round-trip test (T105).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--ts-generate-mcp-server=true` and overlays `test/ts-stub-mcp-mux-overlay/`
on top of a ts-stub copy. Generated code lands in `src/mux-generated/`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-ts-mcp-mux.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/mux-generated" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --ts-generate-mcp-server=true

rsync -a ./test/ts-stub-mcp-mux-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-mcp-mux

Run the TypeScript MCP muxer round-trip tests (T105).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602).

```bash
TEST_DIR="${action.test-gen-ts-mcp-mux.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/mcp.muxer.test.ts
popd

ret success:bool=true
```


# action: test-gen-ts-mcp-mux-async

Generate code for the TypeScript ASYNC MCP muxer round-trip test (T105).
Async sibling of `test-gen-ts-mcp-mux`: uses the mcp-mux-stub-ok model with BOTH
`--ts-generate-mcp-server=true` AND `--ts-async-services=true`, overlays
`test/ts-stub-mcp-mux-async-overlay/` on top of a ts-stub copy. Generated code
lands in `src/mux-async-generated/`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-mcp-mux-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='node_modules' --exclude='dist' \
  ./test/ts-stub/ "$TEST_DIR/ts-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-ts-mcp-mux-async.lock \
  :typescript \
  --output "$TEST_DIR/ts-stub/src/mux-async-generated" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>' \
  --ts-generate-mcp-server=true \
  --ts-async-services=true

rsync -a ./test/ts-stub-mcp-mux-async-overlay/ "$TEST_DIR/ts-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-mcp-mux-async

Run the TypeScript ASYNC MCP muxer round-trip tests (T105).
Async sibling of `test-ts-mcp-mux`. Validates tools/list union, per-service
routing for UserService and OrderService (awaited), DuplicateTool collision,
and NoMatchingTool (-32602) against `AbstractAsyncMcpMuxer`.

```bash
TEST_DIR="${action.test-gen-ts-mcp-mux-async.test_dir}"
pushd "$TEST_DIR/ts-stub"
npm install
npx vitest run src/mcp.muxer.async.test.ts
popd

ret success:bool=true
```

# action: test-gen-cs-mcp-mux

Generate code for the C# MCP muxer round-trip test (T106, sync).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--cs-generate-mcp-server=true` (Either mode) and overlays
`test/cs-stub-mcp-mux-overlay/` on top of a cs-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-cs-mcp-mux.lock \
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

rsync -a ./test/cs-stub-mcp-mux-overlay/ "$TEST_DIR/cs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-mcp-mux

Run the C# MCP muxer round-trip tests (T106, sync).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602).

```bash
TEST_DIR="${action.test-gen-cs-mcp-mux.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release McpMuxTests/McpMuxTests.csproj
dotnet test -c Release McpMuxTests/McpMuxTests.csproj
popd

ret success:bool=true
```

# action: test-gen-cs-mcp-mux-async

Generate code for the C# ASYNC MCP muxer round-trip test (T106, async).
Async sibling of `test-gen-cs-mcp-mux`: uses the mcp-mux-stub-ok model with BOTH
`--cs-generate-mcp-server=true` AND `--cs-async-services=true` (Either mode), and
overlays `test/cs-stub-mcp-mux-async-overlay/` on top of a cs-stub copy.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-cs-mcp-mux-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/cs-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-cs-mcp-mux-async.lock \
  :cs \
  --output "$TEST_DIR/cs-stub/BaboonDefinitions/Generated" \
  --cs-wrapped-adt-branch-codecs=false \
  --cs-write-evolution-dict=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="Baboon.Runtime.Shared.Either" \
  --service-result-pattern="<\$error, \$success>" \
  --cs-generate-mcp-server=true \
  --cs-async-services=true

rsync -a ./test/cs-stub-mcp-mux-async-overlay/ "$TEST_DIR/cs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-cs-mcp-mux-async

Run the C# ASYNC MCP muxer round-trip tests (T106, async).
Async sibling of `test-cs-mcp-mux`. Validates tools/list union, per-service
routing (awaited), DuplicateTool collision, and NoMatchingTool (-32602) against
`AbstractAsyncMcpMuxer`.

```bash
TEST_DIR="${action.test-gen-cs-mcp-mux-async.test_dir}"
pushd "$TEST_DIR/cs-stub"
dotnet build -c Release McpMuxTests/McpMuxTests.csproj
dotnet test -c Release McpMuxTests/McpMuxTests.csproj
popd

ret success:bool=true
```
# action: test-gen-py-mcp-mux

Generate code for the Python MCP muxer round-trip test (T108).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--py-generate-mcp-server=true` and overlays `test/py-stub-mcp-mux-overlay/`
on top of a py-stub copy. Generated code lands in `BaboonDefinitions/Generated/`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-py-mcp-mux.lock \
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

rsync -a ./test/py-stub-mcp-mux-overlay/ "$TEST_DIR/py-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-py-mcp-mux

Run the Python MCP muxer round-trip tests (T108).
Validates tools/list union, per-service routing for UserService and
OrderService, DuplicateTool collision, and NoMatchingTool (-32602).

```bash
TEST_DIR="${action.test-gen-py-mcp-mux.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest BaboonTests.mcp_mux.test_mcp_mux
popd

ret success:bool=true
```

# action: test-gen-py-mcp-mux-async

Generate code for the Python ASYNC MCP muxer round-trip test (T108).
Async sibling of `test-gen-py-mcp-mux`: uses the mcp-mux-stub-ok model with BOTH
`--py-generate-mcp-server=true` AND `--py-async-services=true`, overlays
`test/py-stub-mcp-mux-async-overlay/` on top of a py-stub copy. Generated code
lands in `BaboonDefinitions/Generated/`.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-py-mcp-mux-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/py-stub"

rsync -a --exclude='Generated*' --exclude='generated-*' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-py-mcp-mux-async.lock \
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
  --py-generate-mcp-server=true \
  --py-async-services=true

rsync -a ./test/py-stub-mcp-mux-async-overlay/ "$TEST_DIR/py-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-py-mcp-mux-async

Run the Python ASYNC MCP muxer round-trip tests (T108).
Async sibling of `test-py-mcp-mux`. Validates tools/list union, per-service
routing for UserService and OrderService (awaited via asyncio.run), DuplicateTool
collision, and NoMatchingTool (-32602) against `AsyncMcpMuxer`.

```bash
TEST_DIR="${action.test-gen-py-mcp-mux-async.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest BaboonTests.mcp_mux_async.test_mcp_mux_async
popd

ret success:bool=true
```
# action: test-gen-rs-mcp-mux

Generate code for the Rust MCP muxer round-trip test (T109).
Uses the mcp-mux-stub-ok model (UserService + OrderService) with
`--rs-generate-mcp-server=true` (Result errors mode) and overlays
`test/rs-stub-mcp-mux-overlay/` on top of a rs-stub copy. Generated code
lands in `src/` (the McpToolsMcpServer / UserServiceMcpServer pattern of the
single-service `test-gen-rust-mcp` lane, but with two @root services so the
muxer can compose them).

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-mcp-mux"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"
mkdir -p "$TEST_DIR/rs-stub"

# Copy only the Cargo.toml (package name = baboon-rs-stub) from rs-stub;
# the generated source tree from mcp-mux-stub-ok will be written into src/.
cp ./test/rs-stub/Cargo.toml "$TEST_DIR/rs-stub/Cargo.toml"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-rs-mcp-mux.lock \
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

rsync -a ./test/rs-stub-mcp-mux-overlay/ "$TEST_DIR/rs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-mcp-mux

Run the Rust MCP muxer round-trip tests (T109).
Validates tools/list union across UserService + OrderService in
registration-then-declaration order, per-service routing, DuplicateTool on
collision, and NoMatchingTool (-32602) against `AbstractMcpMuxer`.

```bash
TEST_DIR="${action.test-gen-rs-mcp-mux.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test --test mcp_mux_tests
popd

ret success:bool=true
```

# action: test-gen-rs-mcp-mux-async

Generate code for the Rust ASYNC MCP muxer round-trip test (T109).
Async sibling of `test-gen-rs-mcp-mux`: uses the mcp-mux-stub-ok model + BOTH
`--rs-generate-mcp-server=true` AND `--rs-async-services=true` (Result errors
mode), and overlays `test/rs-stub-mcp-mux-async-overlay/` on top of a rs-stub
copy. Proves the muxer compiles + routes in async mode: `route_tool_call`
drives the async invoke future to completion via the same `block_on` bridge
the per-service async server uses (D30/T98), so the muxer needs no async-specific
type. DO NOT modify the sync `test-gen-rs-mcp-mux` lane.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-rs-mcp-mux-async"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/rs-stub"
mkdir -p "$TEST_DIR/rs-stub"

cp ./test/rs-stub/Cargo.toml "$TEST_DIR/rs-stub/Cargo.toml"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/mcp-mux-stub-ok/ \
  --lockfile=./target/baboon-rs-mcp-mux-async.lock \
  :rust \
  --output "$TEST_DIR/rs-stub/src" \
  --rs-write-evolution-dict=true \
  --rs-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type=Result \
  '--service-result-pattern=<$success, $error>' \
  --rs-generate-mcp-server=true \
  --rs-async-services=true

rsync -a ./test/rs-stub-mcp-mux-async-overlay/ "$TEST_DIR/rs-stub/"

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-rs-mcp-mux-async

Run the Rust ASYNC MCP muxer round-trip tests (T109).
Async sibling of `test-rs-mcp-mux`. Validates tools/list union, per-service
routing (driven through the async invoke via `block_on`), DuplicateTool on
collision, and NoMatchingTool (-32602) against `AbstractMcpMuxer` composing
async-generated `<Service>McpServer`s.

```bash
TEST_DIR="${action.test-gen-rs-mcp-mux-async.test_dir}"
pushd "$TEST_DIR/rs-stub"
RUSTFLAGS="-D warnings" cargo test --test mcp_mux_tests
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

# action: test-gen-ts-wiring-collision

Generate TypeScript service wiring for the T115 name-collision fixture model
(`baboon-compiler/src/test/resources/ts-wiring-collision-ok/`) in BaboonEither
errors mode. T117 fixed the alias-dangling bare name emitted at the decode-step;
T118 wired this lane into `:test` as a permanent regression gate.

The generated code goes into an ISOLATED stub directory (`target/test-ts-wiring-
collision/ts-wiring-collision-stub/src/`) with its own `tsconfig.typecheck.json`.
The shared `test/ts-stub` is NOT used.

```bash
dep action.build

BABOON_BIN="${action.build.binary}"
TEST_DIR="./target/test-ts-wiring-collision"

mkdir -p "$TEST_DIR"
rm -rf "$TEST_DIR/ts-wiring-collision-stub"

rsync -a --exclude='node_modules' \
  ./test/ts-wiring-collision-stub/ "$TEST_DIR/ts-wiring-collision-stub/"

mkdir -p "$TEST_DIR/ts-wiring-collision-stub/src"

$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/ts-wiring-collision-ok/ \
  --meta-write-evolution-json "$TEST_DIR/baboon-ts-collision-meta.json" \
  --lockfile="$TEST_DIR/baboon-ts-collision.lock" \
  :typescript \
  --output "$TEST_DIR/ts-wiring-collision-stub/src" \
  --ts-write-evolution-dict=true \
  --ts-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --service-result-no-errors=false \
  --service-result-type="BaboonEither" \
  '--service-result-pattern=<$error, $success>'

ret success:bool=true
ret test_dir:string="$TEST_DIR"
```

# action: test-ts-wiring-collision

Permanent regression gate for D32 (T116/T117/T118): runs `tsc --noEmit` over
the generated collision wiring.ts using an ISOLATED `tsconfig.typecheck.json`
(NOT the shared `test/ts-stub`). Wired into `:test` and `:ci` via T118.

With the UNFIXED translator this action FAILS with:
  error TS2304: Cannot find name 'In'
at `let input: BaboonEither<BaboonWiringError, In>` in the generated
`tswc/collision/svc-mux/wiring.ts` — the alias-dangling bare `In` name.

T117 fixed the translator (alias-aware rendering at L920). This lane is now
GREEN and aggregated into `:test`/`:ci` as a permanent guard.

```bash
TEST_DIR="${action.test-gen-ts-wiring-collision.test_dir}"
pushd "$TEST_DIR/ts-wiring-collision-stub"
npm install
npx tsc --noEmit -p tsconfig.typecheck.json
popd

ret success:bool=true
```

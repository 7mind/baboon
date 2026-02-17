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
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub" "$TEST_DIR/py-stub" "$TEST_DIR/rs-stub" "$TEST_DIR/ts-stub" "$TEST_DIR/kt-stub" "$TEST_DIR/jv-stub" "$TEST_DIR/dt-stub" "$TEST_DIR/sw-stub"

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
  --output "$TEST_DIR/ts-stub/src" \
  --test-output "$TEST_DIR/ts-stub/src" \
  --fixture-output "$TEST_DIR/ts-stub/src" \
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
  --output "$TEST_DIR/sw-stub/Sources/BaboonGenerated" \
  --test-output "$TEST_DIR/sw-stub/Tests/BaboonTests" \
  --fixture-output "$TEST_DIR/sw-stub/Sources/BaboonGenerated" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true

# Move Dart runtime files into the baboon_runtime package
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_fixture.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

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
popd

ret success:bool=true
```

# action: test-rust-regular

Run Rust tests with regular ADT codecs.

```bash
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/rs-stub"
cargo test
popd

ret success:bool=true
```

# action: test-typescript-regular

Run TypeScript tests with regular ADT codecs.

```bash
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
dart test
popd

ret success:bool=true
```

# action: test-swift-regular

Run Swift tests with regular ADT codecs.

```bash
if ! command -v swift &> /dev/null; then
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

# On macOS, Nix pollutes the environment with an incompatible SDK (VFS overlays, SDKROOT, compiler flags).
# Explicitly reset to Xcode's SDK and strip ALL Nix compiler/linker flags.
# Check SDKROOT and NIX_CFLAGS_COMPILE since nixpkgs 25.11 may not set SDKROOT but still injects SDK via CC wrapper.
if [[ "$(uname)" == "Darwin" ]] && { [[ "${SDKROOT:-}" == /nix/* ]] || [[ -n "${NIX_CFLAGS_COMPILE:-}" ]] || [[ -n "${NIX_LDFLAGS:-}" ]]; }; then
  export SDKROOT=$(/usr/bin/xcrun --sdk macosx --show-sdk-path)
  unset NIX_CFLAGS_COMPILE NIX_LDFLAGS NIX_CFLAGS_COMPILE_FOR_BUILD NIX_LDFLAGS_FOR_BUILD
fi

TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/sw-stub"
swift test
popd

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
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub" "$TEST_DIR/py-stub" "$TEST_DIR/rs-stub" "$TEST_DIR/ts-stub" "$TEST_DIR/kt-stub" "$TEST_DIR/jv-stub" "$TEST_DIR/dt-stub" "$TEST_DIR/sw-stub"

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
  --output "$TEST_DIR/ts-stub/src" \
  --test-output "$TEST_DIR/ts-stub/src" \
  --fixture-output "$TEST_DIR/ts-stub/src" \
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
  --output "$TEST_DIR/sw-stub/Sources/BaboonGenerated" \
  --test-output "$TEST_DIR/sw-stub/Tests/BaboonTests" \
  --fixture-output "$TEST_DIR/sw-stub/Sources/BaboonGenerated" \
  --sw-write-evolution-dict=true \
  --sw-wrapped-adt-branch-codecs=true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true

# Move Dart runtime files into the baboon_runtime package
mv "$TEST_DIR/dt-stub/lib/baboon_runtime.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"
mv "$TEST_DIR/dt-stub/lib/baboon_fixture.dart" "$TEST_DIR/dt-stub/packages/baboon_runtime/lib/"

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
popd

ret success:bool=true
```

# action: test-rust-wrapped

Run Rust tests with wrapped ADT codecs.

```bash
TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/rs-stub"
cargo test
popd

ret success:bool=true
```

# action: test-typescript-wrapped

Run TypeScript tests with wrapped ADT codecs.

```bash
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
dart test
popd

ret success:bool=true
```

# action: test-swift-wrapped

Run Swift tests with wrapped ADT codecs.

```bash
if ! command -v swift &> /dev/null; then
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

# On macOS, Nix pollutes the environment with an incompatible SDK (VFS overlays, SDKROOT, compiler flags).
# Explicitly reset to Xcode's SDK and strip ALL Nix compiler/linker flags.
# Check SDKROOT and NIX_CFLAGS_COMPILE since nixpkgs 25.11 may not set SDKROOT but still injects SDK via CC wrapper.
if [[ "$(uname)" == "Darwin" ]] && { [[ "${SDKROOT:-}" == /nix/* ]] || [[ -n "${NIX_CFLAGS_COMPILE:-}" ]] || [[ -n "${NIX_LDFLAGS:-}" ]]; }; then
  export SDKROOT=$(/usr/bin/xcrun --sdk macosx --show-sdk-path)
  unset NIX_CFLAGS_COMPILE NIX_LDFLAGS NIX_CFLAGS_COMPILE_FOR_BUILD NIX_LDFLAGS_FOR_BUILD
fi

TEST_DIR="${action.test-gen-wrapped-adt.test_dir}"
pushd "$TEST_DIR/sw-stub"
swift test
popd

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
  --output ./test/conv-test-sw/Sources/BaboonGenerated

# Move Dart runtime files into the baboon_runtime package
mv ./test/conv-test-dt/lib/generated/baboon_runtime.dart ./test/conv-test-dt/packages/baboon_runtime/lib/

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

if ! command -v swift &> /dev/null; then
  echo "Swift not found, skipping compat gen"
  ret success:bool=true
  exit 0
fi

# On macOS, Nix pollutes the environment with an incompatible SDK (VFS overlays, SDKROOT, compiler flags).
# Explicitly reset to Xcode's SDK and strip ALL Nix compiler/linker flags.
# Check SDKROOT and NIX_CFLAGS_COMPILE since nixpkgs 25.11 may not set SDKROOT but still injects SDK via CC wrapper.
if [[ "$(uname)" == "Darwin" ]] && { [[ "${SDKROOT:-}" == /nix/* ]] || [[ -n "${NIX_CFLAGS_COMPILE:-}" ]] || [[ -n "${NIX_LDFLAGS:-}" ]]; }; then
  export SDKROOT=$(/usr/bin/xcrun --sdk macosx --show-sdk-path)
  unset NIX_CFLAGS_COMPILE NIX_LDFLAGS NIX_CFLAGS_COMPILE_FOR_BUILD NIX_LDFLAGS_FOR_BUILD
fi

pushd ./test/conv-test-sw
swift run CompatMain
popd

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

Run basic SBT tests.

```bash
dep action.build
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
  echo "Swift not found, skipping test"
  ret success:bool=true
  exit 0
fi

# On macOS, Nix pollutes the environment with an incompatible SDK (VFS overlays, SDKROOT, compiler flags).
# Explicitly reset to Xcode's SDK and strip ALL Nix compiler/linker flags.
# Check SDKROOT and NIX_CFLAGS_COMPILE since nixpkgs 25.11 may not set SDKROOT but still injects SDK via CC wrapper.
if [[ "$(uname)" == "Darwin" ]] && { [[ "${SDKROOT:-}" == /nix/* ]] || [[ -n "${NIX_CFLAGS_COMPILE:-}" ]] || [[ -n "${NIX_LDFLAGS:-}" ]]; }; then
  export SDKROOT=$(/usr/bin/xcrun --sdk macosx --show-sdk-path)
  unset NIX_CFLAGS_COMPILE NIX_LDFLAGS NIX_CFLAGS_COMPILE_FOR_BUILD NIX_LDFLAGS_FOR_BUILD
fi

pushd ./test/conv-test-sw
swift test
popd

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
  --output "$TEST_DIR/ts-stub/src" \
  --test-output "$TEST_DIR/ts-stub/src" \
  --fixture-output "$TEST_DIR/ts-stub/src" \
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
npm test
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
  --output "$TEST_DIR/ts-stub/src" \
  --test-output "$TEST_DIR/ts-stub/src" \
  --fixture-output "$TEST_DIR/ts-stub/src" \
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
npm test
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
  --output "$TEST_DIR/ts-stub/src" \
  --test-output "$TEST_DIR/ts-stub/src" \
  --fixture-output "$TEST_DIR/ts-stub/src" \
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
npm test
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

# action: test

Run complete test suite (orchestrator action).

```bash
dep action.test-sbt-basic
dep action.test-cs-regular
dep action.test-scala-regular
dep action.test-python-regular
dep action.test-rust-regular
dep action.test-typescript-regular
dep action.test-kotlin-regular
dep action.test-java-regular
dep action.test-dart-regular
dep action.test-swift-regular
dep action.test-cs-wrapped
dep action.test-scala-wrapped
dep action.test-python-wrapped
dep action.test-rust-wrapped
dep action.test-typescript-wrapped
dep action.test-kotlin-wrapped
dep action.test-java-wrapped
dep action.test-dart-wrapped
dep action.test-swift-wrapped
dep action.test-manual-cs
dep action.test-manual-scala
dep action.test-manual-rust
dep action.test-manual-typescript
dep action.test-manual-kotlin
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
dep action.test-rs-wiring-either
dep action.test-rs-wiring-result
dep action.test-rs-wiring-outcome
dep action.test-py-wiring-either
dep action.test-py-wiring-result
dep action.test-py-wiring-outcome

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

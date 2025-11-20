# Baboon Build Actions

This file defines the build orchestration for the Baboon project using mudyla.

# arguments

- `args.mkdist-source`: directory="./baboon-compiler/.jvm/target/graalvm-native-image"; Source directory for distribution
- `args.mkdist-target`: directory="./target/dist"; Target directory for distribution

# action: fmt

Format Scala code using scalafmt.

```bash
cs launch scalafmt -- --non-interactive || true
git add . || true
ret success:bool=true
```

# action: build

Build the Baboon compiler as a GraalVM native image.

```bash
sbt baboonJVM/GraalVMNativeImage/packageBin
ret binary:file=baboon-compiler/.jvm/target/graalvm-native-image/baboon
```

# action: test

Run comprehensive test suite including code generation and validation.

```bash
set -euo pipefail

BABOON_BIN="${action.build.binary}"

echo "::group::GENERATE CODE (regular ADT)"
$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output ./test/cs-stub/BaboonDefinitions/Generated \
  --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
  --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
  --cs-wrapped-adt-branch-codecs false \
  --cs-write-evolution-dict true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output ./test/sc-stub/src/main/scala/generated-main \
  --test-output ./test/sc-stub/src/test/scala/generated-tests \
  --fixture-output ./test/sc-stub/src/main/scala/generated-fixtures \
  --sc-write-evolution-dict true \
  --sc-wrapped-adt-branch-codecs false
echo "::endgroup::"

echo "::group::RUN GENERATED C# TESTS"
pushd ./test/cs-stub
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd
echo "::endgroup::"

echo "::group::RUN GENERATED SCALA TESTS"
pushd ./test/sc-stub
sbt +clean +test
popd
echo "::endgroup::"

echo "::group::GENERATE CODE (wrapped ADT)"
$BABOON_BIN \
  --model-dir ./baboon-compiler/src/test/resources/baboon/ \
  --meta-write-evolution-json baboon-meta.json \
  --lock-file=./target/baboon.lock \
  :cs \
  --output ./test/cs-stub/BaboonDefinitions/Generated \
  --test-output ./test/cs-stub/BaboonTests/GeneratedTests \
  --fixture-output ./test/cs-stub/BaboonTests/GeneratedFixtures \
  --cs-wrapped-adt-branch-codecs true \
  --cs-write-evolution-dict true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :scala \
  --output ./test/sc-stub/src/main/scala/generated-main \
  --test-output ./test/sc-stub/src/test/scala/generated-tests \
  --fixture-output ./test/sc-stub/src/main/scala/generated-fixtures \
  --sc-write-evolution-dict true \
  --sc-wrapped-adt-branch-codecs true
echo "::endgroup::"

echo "::group::RUN GENERATED C# TESTS"
pushd ./test/cs-stub
dotnet build -c Debug
dotnet test -c Debug BaboonTests/BaboonTests.csproj
popd
echo "::endgroup::"

echo "::group::RUN GENERATED SCALA TESTS"
pushd ./test/sc-stub
sbt +clean +test
popd
echo "::endgroup::"

rm -rf ./test/conv-test-cs/ConvTest/Generated

echo "::group::GENERATE CODE (manual test project)"
$BABOON_BIN \
  --model-dir ./test/conv-test \
  :cs \
  --output ./test/conv-test-cs/ConvTest/Generated \
  :scala \
  --output ./test/conv-test-sc/src/main/scala/generated-main
echo "::endgroup::"

echo "::group::GENERATE COMPAT TEST FILES (Scala)"
pushd ./test/conv-test-sc
sbt "runMain example.CompatMain"
popd
echo "::endgroup::"

echo "::group::GENERATE COMPAT TEST FILES (C#)"
pushd ./test/conv-test-cs
dotnet run --project ConvTest/ConvTest.csproj
popd
echo "::endgroup::"

echo "::group::RUN MANUAL C# TESTS"
pushd ./test/conv-test-cs
dotnet build
dotnet test
popd
echo "::endgroup::"

echo "::group::RUN MANUAL SCALA TESTS"
pushd ./test/conv-test-sc
sbt +clean +test
popd
echo "::endgroup::"

ret success:bool=true
```

# action: mkdist

Create distribution packages from built binaries.

```bash
set -euo pipefail

BABOON_BIN="${action.build.binary}"
SRC="${args.mkdist-source}"
TGT="${args.mkdist-target}"

src="$(realpath "$SRC")"
tgt="$(realpath "$TGT")"

distbin="$tgt/dist-bin"
distzip="$tgt/dist-zip"

mkdir -p "$distbin"
mkdir -p "$distzip"

pushd "$src"

for d in baboon-*; do
  if [[ -f ./$d/baboon ]]; then
    cp ./${d}/baboon ${distbin}/${d}
    zip -r9 -j ${distzip}/${d}.zip ${distbin}/${d}
  elif [[ -f ./$d/baboon.exe ]]; then
    cp ./${d}/baboon.exe ${distbin}/${d}.exe
    zip -r9 -j ${distzip}/${d}.zip ${distbin}/${d}.exe
  fi
done

popd

ret dist_dir:directory=$tgt
```

# action: flake-refresh

Refresh Nix flake configuration.

```bash
if [[ "${DO_FLAKE_VALIDATE:-0}" == "1" ]]; then
  nix flake update
  nix flake check
fi
ret success:bool=true
```

# action: gen-js

Generate JavaScript bindings (placeholder for future implementation).

```bash
echo "JavaScript generation not yet implemented"
ret success:bool=true
```

# action: publish-scala

Publish Scala artifacts.

```bash
if [[ -n "${SONATYPE_SECRET:-}" ]]; then
  echo "Publishing Scala artifacts..."
  sbt +publishSigned
else
  echo "Skipping publish - SONATYPE_SECRET not set"
fi
ret success:bool=true
```

# action: full-build

Complete build pipeline with all steps.

```bash
# This action orchestrates the complete build by depending on other actions
echo "Full build completed successfully"
echo "- Formatting: ${action.fmt.success}"
echo "- Build: ${action.build.binary}"
echo "- Tests: ${action.test.success}"
ret success:bool=true
```

# Baboon Test Actions

This file defines test-related actions for the Baboon project.

# action: test-gen-regular-adt

Generate code with regular (non-wrapped) ADT branch codecs.

```bash
set -euo pipefail

BABOON_BIN="${action.build.binary}"

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

ret success:bool=true
```

# action: test-cs-regular

Run C# tests with regular ADT codecs (Release configuration).

```bash
set -euo pipefail

# Ensure code is generated first
echo "Dependency: ${action.test-gen-regular-adt.success}"

pushd ./test/cs-stub
dotnet build -c Release
dotnet test -c Release BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-regular

Run Scala tests with regular ADT codecs.

```bash
set -euo pipefail

# Ensure code is generated first
echo "Dependency: ${action.test-gen-regular-adt.success}"

pushd ./test/sc-stub
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-wrapped-adt

Generate code with wrapped ADT branch codecs.

```bash
set -euo pipefail

BABOON_BIN="${action.build.binary}"

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

ret success:bool=true
```

# action: test-cs-wrapped

Run C# tests with wrapped ADT codecs (Debug configuration).

```bash
set -euo pipefail

# Ensure code is generated first
echo "Dependency: ${action.test-gen-wrapped-adt.success}"

pushd ./test/cs-stub
dotnet build -c Debug
dotnet test -c Debug BaboonTests/BaboonTests.csproj
popd

ret success:bool=true
```

# action: test-scala-wrapped

Run Scala tests with wrapped ADT codecs.

```bash
set -euo pipefail

# Ensure code is generated first
echo "Dependency: ${action.test-gen-wrapped-adt.success}"

pushd ./test/sc-stub
sbt +clean +test
popd

ret success:bool=true
```

# action: test-gen-manual

Generate code for manual test projects.

```bash
set -euo pipefail

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
set -euo pipefail

# Ensure manual code is generated first
echo "Dependency: ${action.test-gen-manual.success}"

pushd ./test/conv-test-sc
sbt "runMain example.CompatMain"
popd

ret success:bool=true
```

# action: test-gen-compat-cs

Generate compatibility test files using C#.

```bash
set -euo pipefail

# Ensure manual code is generated first
echo "Dependency: ${action.test-gen-manual.success}"

pushd ./test/conv-test-cs
dotnet run --project ConvTest/ConvTest.csproj
popd

ret success:bool=true
```

# action: test-manual-cs

Run manual C# compatibility tests.

```bash
set -euo pipefail

# Ensure compat files are generated first
echo "Dependencies: ${action.test-gen-compat-scala.success}, ${action.test-gen-compat-cs.success}"

pushd ./test/conv-test-cs
dotnet build
dotnet test
popd

ret success:bool=true
```

# action: test-manual-scala

Run manual Scala compatibility tests.

```bash
set -euo pipefail

# Ensure compat files are generated first
echo "Dependencies: ${action.test-gen-compat-scala.success}, ${action.test-gen-compat-cs.success}"

pushd ./test/conv-test-sc
sbt +clean +test
popd

ret success:bool=true
```

# action: test

Run complete test suite (orchestrator action).

```bash
# All tests completed successfully via dependencies
echo "Test suite completed:"
echo "  Regular ADT: ${action.test-scala-regular.success} (Scala), ${action.test-cs-regular.success} (C#)"
echo "  Wrapped ADT: ${action.test-scala-wrapped.success} (Scala), ${action.test-cs-wrapped.success} (C#)"
echo "  Manual tests: ${action.test-manual-scala.success} (Scala), ${action.test-manual-cs.success} (C#)"

ret success:bool=true
```

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
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub"  "$TEST_DIR/py-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"  

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
  --sc-wrapped-adt-branch-codecs false \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict true \
  --py-wrapped-adt-branch-codecs false

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
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests/GeneratedTests/testpkg/pkg0
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
rm -rf "$TEST_DIR/cs-stub" "$TEST_DIR/sc-stub" "$TEST_DIR/py-stub"

# Copy stub projects, excluding generated and build artifacts
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/cs-stub/ "$TEST_DIR/cs-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/sc-stub/ "$TEST_DIR/sc-stub/"
rsync -a --exclude='Generated*' --exclude='generated-*' --exclude='bin' --exclude='obj' --exclude='target' --exclude='project/target' \
  ./test/py-stub/ "$TEST_DIR/py-stub/"  

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
  --sc-wrapped-adt-branch-codecs true \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  :python \
  --output "$TEST_DIR/py-stub/BaboonDefinitions/Generated" \
  --test-output "$TEST_DIR/py-stub/BaboonTests/GeneratedTests" \
  --fixture-output "$TEST_DIR/py-stub/BaboonTests/GeneratedFixtures" \
  --generate-ueba-codecs-by-default=true \
  --generate-json-codecs-by-default=true \
  --py-write-evolution-dict true \
  --py-wrapped-adt-branch-codecs true

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
TEST_DIR="${action.test-gen-regular-adt.test_dir}"
pushd "$TEST_DIR/py-stub"
python3 -m venv .venv
if [ -f ".venv/Scripts/activate" ]; then source .venv/Scripts/activate; else source .venv/bin/activate; fi
python3 -m pip install -r requirements.txt
python3 -m unittest discover -s BaboonTests/GeneratedTests/testpkg/pkg0
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
  --output ./test/conv-test-py/Generated

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

# action: test-manual-cs

Run manual C# compatibility tests.

```bash
dep action.test-gen-compat-scala
dep action.test-gen-compat-cs
dep action.test-gen-compat-python

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

# action: test

Run complete test suite (orchestrator action).

```bash
dep action.test-sbt-basic
dep action.test-cs-regular
dep action.test-scala-regular
dep action.test-python-regular
dep action.test-cs-wrapped
dep action.test-scala-wrapped
dep action.test-python-wrapped
dep action.test-manual-cs
dep action.test-manual-scala

ret success:bool=true
```

# Baboon Build Actions

This file defines the build orchestration for the Baboon project using mudyla.

# arguments

- `args.mkdist-source`: directory="./baboon-compiler/.jvm/target/graalvm-native-image"; Source directory for distribution
- `args.mkdist-target`: directory="./target/dist"; Target directory for distribution

# passthrough

- `LC_ALL`
- `HOME`
- `USER`
- `SONATYPE_SECRET`

# action: build

Build the Baboon compiler as a GraalVM native image.

## definition 

```bash
env
sbt baboonJVM/GraalVMNativeImage/packageBin
ret binary:file=baboon-compiler/.jvm/target/graalvm-native-image/baboon
```

## definition when `sys.platform: windows`

```bash
env
sbt baboonJVM/GraalVMNativeImage/packageBin
ret binary:file=baboon-compiler/.jvm/target/graalvm-native-image/baboon.exe
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

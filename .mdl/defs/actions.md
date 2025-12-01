# Baboon Build Actions

This file defines the build orchestration for the Baboon project using mudyla.

# arguments

- `args.mkdist-source`:  Source directory for distribution
   - type: `directory`
   - default: `"./baboon-compiler/.jvm/target/graalvm-native-image"`
- `args.mkdist-target`:  Target directory for distribution
   - type: `directory`
   - default: `"./target/dist"`

# environment

- `LANG=C.UTF-8`

## passthrough

- `HOME`
- `USER`
- `CI_PULL_REQUEST`
- `CI_BRANCH_TAG`
- `SONATYPE_SECRET`

# action: clean

Clean all the junk

```bash
sbt +clean
```

# action: build

Build the Baboon compiler as a GraalVM native image.

## definition

```bash
weak action.clean
sbt baboonJVM/GraalVMNativeImage/packageBin
ret binary:file=baboon-compiler/.jvm/target/graalvm-native-image/baboon
```

## definition when `platform: windows`

```bash
weak action.clean
sbt baboonJVM/GraalVMNativeImage/packageBin
ret binary:file=baboon-compiler/.jvm/target/graalvm-native-image/baboon.exe
```

# action: mkdist

Create distribution packages from built binaries.

```bash
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

Generate optimized JavaScript bundle for BaboonJS.

```bash
set -euo pipefail

JS_DIST_DIR="./baboon-compiler/.js/target/scala-2.13/baboon-opt"

if [[ -z "${CI_PULL_REQUEST:-}" ]]; then
  echo "CI_PULL_REQUEST must be explicitly provided as 'true' or 'false'." >&2
  exit 1
fi

if [[ "$CI_PULL_REQUEST" != "true" && "$CI_PULL_REQUEST" != "false" ]]; then
  echo "CI_PULL_REQUEST must be either 'true' or 'false', got '$CI_PULL_REQUEST'." >&2
  exit 1
fi

if [[ "$CI_PULL_REQUEST" != "false" ]]; then
  echo "Skipping BaboonJS build because this is a pull request."
  mkdir -p "$JS_DIST_DIR"
  ret skipped:bool=true
  ret dist_dir:directory="$(realpath "$JS_DIST_DIR")"
  exit 0
fi

direnv exec . sbt '++2.13 baboonJS/fullLinkJS'

if [[ ! -f "${JS_DIST_DIR}/main.js" ]]; then
  echo "Scala.js output is missing at ${JS_DIST_DIR}/main.js" >&2
  exit 1
fi

ret success:bool=true
ret dist_dir:directory="$(realpath "$JS_DIST_DIR")"
```

# action: publish-scala

Publish Scala artifacts.

```bash
dep env.SONATYPE_SECRET
echo "Publishing Scala artifacts..."
sbt +publishSigned
ret success:bool=true
```

# action: publish-npm

Build and publish the Baboon JavaScript package to npm.

```bash
dep action.gen-js

set -euo pipefail

if [[ -z "${CI_PULL_REQUEST:-}" ]]; then
  echo "CI_PULL_REQUEST must be explicitly provided as 'true' or 'false'." >&2
  exit 1
fi

if [[ "$CI_PULL_REQUEST" != "true" && "$CI_PULL_REQUEST" != "false" ]]; then
  echo "CI_PULL_REQUEST must be either 'true' or 'false', got '$CI_PULL_REQUEST'." >&2
  exit 1
fi

if [[ -z "${CI_BRANCH_TAG:-}" ]]; then
  echo "CI_BRANCH_TAG must be provided to derive the npm version." >&2
  exit 1
fi

if [[ "$CI_PULL_REQUEST" != "false" ]]; then
  echo "Skipping npm publish because this is a pull request."
  ret skipped:bool=true
  exit 0
fi

PROJECT_ROOT="${sys.project-root}"
DIST_DIR="${action.gen-js.dist_dir}"
PUBLISH_DIR="${PROJECT_ROOT}/target/npm-publish"
TEMPLATE_DIR="${PROJECT_ROOT}/baboon-compiler/npm-template"
LICENSE_FILE="${PROJECT_ROOT}/LICENSE"
VERSION_FILE="${PROJECT_ROOT}/version.sbt"

if [[ ! -d "$DIST_DIR" ]]; then
  echo "Scala.js distribution directory '$DIST_DIR' is missing." >&2
  exit 1
fi

if [[ ! -f "${DIST_DIR}/main.js" ]]; then
  echo "Scala.js entrypoint '${DIST_DIR}/main.js' is missing." >&2
  exit 1
fi

if [[ ! -d "$TEMPLATE_DIR" ]]; then
  echo "npm template directory '$TEMPLATE_DIR' is missing." >&2
  exit 1
fi

rm -rf "$PUBLISH_DIR"
mkdir -p "$PUBLISH_DIR"

cp "${DIST_DIR}/"* "$PUBLISH_DIR"/
cp "${TEMPLATE_DIR}/"* "$PUBLISH_DIR"/
cp "$LICENSE_FILE" "$PUBLISH_DIR"/

if [[ ! -f "$VERSION_FILE" ]]; then
  echo "Unable to locate version file: $VERSION_FILE" >&2
  exit 1
fi

VERSION_FROM_TAG="$CI_BRANCH_TAG"

if [[ "$VERSION_FROM_TAG" =~ ^v(.*)$ ]]; then
  VERSION_FROM_TAG="${BASH_REMATCH[1]}"
fi

VERSION_FROM_SBT=$(sed -n 's/.*"\(.*\)".*/\1/p' "$VERSION_FILE" | head -n 1)
VERSION_FROM_SBT="${VERSION_FROM_SBT%-SNAPSHOT}"

if [[ -z "$VERSION_FROM_TAG" && -z "$VERSION_FROM_SBT" ]]; then
  echo "Unable to derive version from CI_BRANCH_TAG or version.sbt" >&2
  exit 1
fi

VERSION="${VERSION_FROM_TAG:-$VERSION_FROM_SBT}"

sed -i "s/VERSION_PLACEHOLDER/${VERSION}/g" "$PUBLISH_DIR/package.json"

cd "$PUBLISH_DIR"

npm install
npm test
npm publish --provenance --access public

ret success:bool=true
ret publish_dir:directory="$(realpath "$PUBLISH_DIR")"
```

# action: full-build

Complete build pipeline with all steps.

```bash
dep action.build
dep action.test

ret success:bool=true
```

#!/usr/bin/env bash
set -euo pipefail

compiler=$(realpath "$1")
repo=$(cd "$(dirname "$0")/../.." && pwd)
output="$repo/target/ueba-layout-regression"

mkdir -p "$output/scala/src/test/scala" "$output/kotlin/src/test/kotlin"
cp "$repo/test/sc-stub/build.sbt" "$output/scala/"
cp "$repo/test/kt-stub/build.gradle.kts" "$repo/test/kt-stub/settings.gradle.kts" "$repo/test/kt-stub/gradle.properties" "$output/kotlin/"
cp "$repo/test/ueba-layout-regression/HighMarkerSpec.scala" "$output/scala/src/test/scala/"
cp "$repo/test/ueba-layout-regression/HighMarkerTest.kt" "$output/kotlin/src/test/kotlin/"

"$compiler" --model-dir "$repo/test/ueba-layout-regression" \
  :scala --output "$output/scala/src/main/scala" \
  --sc-wrapped-adt-branch-codecs=true --generate-ueba-codecs-by-default=true \
  :kotlin --output "$output/kotlin/src/main/kotlin" \
  --kt-wrapped-adt-branch-codecs=true --generate-ueba-codecs-by-default=true

(cd "$output/scala" && sbt -batch test)
(cd "$output/kotlin" && gradle --console=plain --no-daemon test)

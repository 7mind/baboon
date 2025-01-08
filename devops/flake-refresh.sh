#!/usr/bin/env bash

set -xe

do_update() {
    escaped="${2//[\/&]/\\&}"

    sed -i -E \
        -e "s/version\s*=\s*\"([a-z0-9-]\.?)+\";/version = \"$1\";/g" \
        -e "s/depsSha256\s*=\s*\"([^\"])*\";/depsSha256 = \"$escaped\";/g" \
        $3
}


do_update "0.0.0" "" ./flake.nix

PKG_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/-SNAPSHOT//")

PROPER_HASH=$(nix build --print-build-logs 2>&1 . | grep "got:" | awk '{print $2}')

do_update "$PKG_VERSION" "$PROPER_HASH" ./flake.nix

git add . || true

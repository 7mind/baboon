#!/usr/bin/env bash

set -euo pipefail

function do_update() {
  sed -i -E \
      -e "s/version\s*=\s*\"([a-z0-9-]\.?)+\";/version = \"$1\";/g" \
      $2
}


function run-flake-refresh() {
  PKG_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/-SNAPSHOT//")

  do_update "$PKG_VERSION" ./flake.nix

  nix run github:7mind/sbt-nix -- lockfile-config.json > deps.lock.json

  git add . || true
}
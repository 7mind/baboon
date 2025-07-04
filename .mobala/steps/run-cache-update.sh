#!/usr/bin/env bash

set -euo pipefail

function run-cache-update() {
  version=$(cat version.sbt | grep version | cut -d '"' -f 2)
  cache_dir="${XDG_CACHE_HOME:-"${HOME}/.cache"}"
  cache_dir="${cache_dir}/baboon"
  cache_path="${cache_dir}/baboon-v${version}"

  if [[ -f "./target/graalvm-native-image/baboon" && -d $cache_dir ]]; then
    echo "[info] Updating Baboon cache for '${cache_path}'."
    cp ./target/graalvm-native-image/baboon $cache_path
  else
    echo "[info] Skipped Baboon cache update for '${cache_path}': executable or cache not found."
  fi
}
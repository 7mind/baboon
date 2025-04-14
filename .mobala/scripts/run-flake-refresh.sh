#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

function do_update() {
  escaped="${2//[\/&]/\\&}"

  sed -i -E \
      -e "s/version\s*=\s*\"([a-z0-9-]\.?)+\";/version = \"$1\";/g" \
      -e "s/depsSha256\s*=\s*\"([^\"])*\";/depsSha256 = \"$escaped\";/g" \
      $3
}


function run-flake-refresh() {
if [[ "$DO_FLAKE_REFRESH" == 1  ]]; then
  
  if [[ "$DO_FLAKE_VALIDATE" == 1  ]]; then
    hash_before=$(cat flake.nix| md5sum)
  fi
  
  do_update "0.0.0" "" ./flake.nix
  
  PKG_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/-SNAPSHOT//")
  
  set +o pipefail
  PROPER_HASH=$(nix build --print-build-logs 2>&1 . | grep "got:" | awk '{print $2}')
  set -o pipefail
  
  do_update "$PKG_VERSION" "$PROPER_HASH" ./flake.nix
  
  if [[ "$DO_FLAKE_VALIDATE" == 1  ]]; then
    hash_after=$(cat flake.nix| md5sum)
    
    if [[ "$hash_before" != "$hash_after" ]]; then
        echo "flake.nix is not up to date, run ./build.sh nix flake-refresh"
        exit 1
    fi
  fi 
  
  git add . || true
fi
}
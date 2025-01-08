#!/usr/bin/env bash

set -xe

hash_before=$(cat flake.nix| md5sum)

./devops/flake-refresh.sh

hash_after=$(cat flake.nix| md5sum)

if [[ "$hash_before" != "$hash_after" ]]; then
    echo "flake.nix is not up to date, run ./build.sh nix flake-refresh"
    exit 1
fi

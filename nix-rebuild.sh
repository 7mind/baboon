#!/usr/bin/env bash

set -x
set -e

nix flake update
nix flake lock
nix flake metadata

nix develop --command bash -c "./local-rebuild.sh"

#nix-shell --pure shell.nix --run ./local-rebuild.sh
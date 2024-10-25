#!/usr/bin/env bash

set -x
set -e

nix flake lock
nix flake metadata

nix develop --command bash -c "./local-rebuild.sh"

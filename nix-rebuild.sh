#!/usr/bin/env bash

set -x
set -e

nix develop --command bash -c "./local-rebuild.sh"
#nix-shell --pure shell.nix --run ./local-rebuild.sh
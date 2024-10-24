#!/usr/bin/env bash

set -x
set -e

nix-shell --pure shell.nix --run ./local-rebuild.sh
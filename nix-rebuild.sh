#!/usr/bin/env bash

set -e

case "$OSTYPE" in
  linux*)   nix-shell --pure ./shell-linux.nix --run ./local-build.sh ;;
  darwin*)  nix-shell --pure ./shell-mac.nix --run ./local-test.sh ;;
  *)        echo "unknown: $OSTYPE" && false ;;
esac



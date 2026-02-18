#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
  echo "Usage: $0 <swift-project-dir> <swift-args...>" >&2
  exit 1
fi

SWIFT_PROJECT_DIR="$1"
shift

if [[ ! -d "$SWIFT_PROJECT_DIR" ]]; then
  echo "Swift project directory does not exist: $SWIFT_PROJECT_DIR" >&2
  exit 1
fi

if [[ "$(uname)" == "Darwin" ]]; then
  XCODE_DEVELOPER_DIR="$(env -u DEVELOPER_DIR -u SDKROOT /usr/bin/xcode-select -p)"
  if [[ -z "$XCODE_DEVELOPER_DIR" ]]; then
    echo "xcode-select did not return a developer directory" >&2
    exit 1
  fi

  if [[ "$XCODE_DEVELOPER_DIR" == /nix/store/* ]]; then
    echo "xcode-select resolved to Nix SDK path: $XCODE_DEVELOPER_DIR" >&2
    echo "Expected an Apple developer directory under /Applications" >&2
    exit 1
  fi

  XCODE_SWIFT_BIN="$(DEVELOPER_DIR="$XCODE_DEVELOPER_DIR" env -u SDKROOT /usr/bin/xcrun --find swift)"
  if [[ ! -x "$XCODE_SWIFT_BIN" ]]; then
    echo "Swift binary is missing: $XCODE_SWIFT_BIN" >&2
    exit 1
  fi

  MACOS_SDK_PATH="$(DEVELOPER_DIR="$XCODE_DEVELOPER_DIR" env -u SDKROOT /usr/bin/xcrun --sdk macosx --show-sdk-path)"
  if [[ -z "$MACOS_SDK_PATH" ]]; then
    echo "xcrun did not return a macOS SDK path" >&2
    exit 1
  fi

  pushd "$SWIFT_PROJECT_DIR" >/dev/null
  env -i \
    HOME="$HOME" \
    PATH="/usr/bin:/bin:/usr/sbin:/sbin:$XCODE_DEVELOPER_DIR/usr/bin" \
    DEVELOPER_DIR="$XCODE_DEVELOPER_DIR" \
    SDKROOT="$MACOS_SDK_PATH" \
    "$XCODE_SWIFT_BIN" "$@"
  popd >/dev/null
  exit 0
fi

if ! command -v swift >/dev/null 2>&1; then
  echo "swift is not available in PATH" >&2
  exit 1
fi

if [[ "$(uname)" == "Linux" ]]; then
  SWIFT_BIN="$(command -v swift)"
  if [[ "$SWIFT_BIN" == /nix/store/* ]]; then
    shopt -s nullglob
    swift_lib_paths=(
      /nix/store/*swift-*-lib*/lib/swift/linux
      /nix/store/*swift-corelibs-foundation-*/lib/swift/linux
      /nix/store/*swift-corelibs-libdispatch-*/lib
    )
    shopt -u nullglob

    if [[ ${#swift_lib_paths[@]} -gt 0 ]]; then
      SWIFT_LD_PATH="$(IFS=:; echo "${swift_lib_paths[*]}")"
      export LD_LIBRARY_PATH="${SWIFT_LD_PATH}${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    fi
  fi
fi

pushd "$SWIFT_PROJECT_DIR" >/dev/null
swift "$@"
popd >/dev/null

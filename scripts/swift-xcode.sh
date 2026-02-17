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

if ! command -v swift >/dev/null 2>&1; then
  echo "swift is not available in PATH" >&2
  exit 1
fi

if [[ "$(uname)" == "Darwin" ]]; then
  XCODE_DEVELOPER_DIR="$(/usr/bin/xcode-select -p)"
  if [[ -z "$XCODE_DEVELOPER_DIR" ]]; then
    echo "xcode-select did not return a developer directory" >&2
    exit 1
  fi

  XCODE_SWIFT_BIN="$XCODE_DEVELOPER_DIR/Toolchains/XcodeDefault.xctoolchain/usr/bin/swift"
  if [[ ! -x "$XCODE_SWIFT_BIN" ]]; then
    echo "Swift binary is missing: $XCODE_SWIFT_BIN" >&2
    exit 1
  fi

  MACOS_SDK_PATH="$(/usr/bin/xcrun --sdk macosx --show-sdk-path)"
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

pushd "$SWIFT_PROJECT_DIR" >/dev/null
swift "$@"
popd >/dev/null

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
    SWIFT_TARGET_INFO="$(swiftc -print-target-info)"
    SWIFT_STDLIB="$(python3 -c 'import json,sys; print(json.loads(sys.stdin.read())["paths"]["runtimeLibraryPaths"][0])' <<< "$SWIFT_TARGET_INFO")"
    SWIFT_VERSION="$(swift --version | sed -n 's/^Swift version \([0-9][0-9.]*\).*/\1/p' | head -n1)"
    if [[ -z "$SWIFT_VERSION" ]]; then
      echo "Unable to detect Swift version from 'swift --version'" >&2
      exit 1
    fi

    shopt -s nullglob
    swift_libdispatch_candidates=(/nix/store/*swift-corelibs-libdispatch-"$SWIFT_VERSION"*/lib)
    shopt -u nullglob

    swift_libdispatch_matches=()
    for candidate in "${swift_libdispatch_candidates[@]}"; do
      if [[ "$candidate" == *-dev/lib ]]; then
        continue
      fi
      if [[ -f "$candidate/libdispatch.so" || -f "$candidate/libdispatch.so.5" ]]; then
        swift_libdispatch_matches+=("$candidate")
      fi
    done

    if [[ ${#swift_libdispatch_matches[@]} -ne 1 ]]; then
      echo "Expected exactly one runtime swift-corelibs-libdispatch path for Swift $SWIFT_VERSION, found ${#swift_libdispatch_matches[@]}" >&2
      printf 'Candidates:\n%s\n' "${swift_libdispatch_candidates[@]}" >&2 || true
      exit 1
    fi

    SWIFT_LIBDISPATCH="${swift_libdispatch_matches[0]}"
    if [[ ! -d "$SWIFT_STDLIB" || ! -d "$SWIFT_LIBDISPATCH" ]]; then
      echo "Swift runtime libraries are not available: SWIFT_STDLIB=$SWIFT_STDLIB SWIFT_LIBDISPATCH=$SWIFT_LIBDISPATCH" >&2
      exit 1
    fi

    export LD_LIBRARY_PATH="$SWIFT_LIBDISPATCH:$SWIFT_STDLIB${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
  fi
fi

pushd "$SWIFT_PROJECT_DIR" >/dev/null
swift "$@"
popd >/dev/null

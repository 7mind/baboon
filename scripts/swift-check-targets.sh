#!/usr/bin/env bash
set -euo pipefail

# SwiftPM only warns about a directory under Sources/ or Tests/ that no target claims, so a
# generated module missing from the hand-written Package.swift is neither compiled nor tested
# and the lane still passes. Fail on such a directory instead.

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <swift-project-dir>" >&2
  exit 1
fi

SWIFT_PROJECT_DIR="$1"
MANIFEST="$SWIFT_PROJECT_DIR/Package.swift"

if [[ ! -f "$MANIFEST" ]]; then
  echo "Swift package manifest does not exist: $MANIFEST" >&2
  exit 1
fi

unclaimed=0
for root in Sources Tests/BaboonTests; do
  for dir in "$SWIFT_PROJECT_DIR/$root"/*/; do
    [[ -d "$dir" ]] || continue
    target_path="$root/$(basename "$dir")"
    if ! grep -qF "path: \"$target_path\"" "$MANIFEST"; then
      echo "No target in $MANIFEST has path \"$target_path\"" >&2
      unclaimed=1
    fi
  done
done

if [[ "$unclaimed" -ne 0 ]]; then
  echo "Add a .target / .testTarget for every directory listed above" >&2
  exit 1
fi

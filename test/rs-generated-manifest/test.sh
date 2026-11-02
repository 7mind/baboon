#!/usr/bin/env bash
# Builds the crate exactly as emitted, with the compiler-generated Cargo.toml, for each
# supported edition: the stub lanes compile generated sources under their own manifest.
set -euo pipefail

fixture_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
test_dir="$(mktemp -d -t baboon-rust-manifest-XXXXXXXX)"
printf 'Rust generated-manifest test output: %s\n' "$test_dir"
for edition in default 2021 2024; do
  out="$test_dir/$edition"
  if [ "$edition" = default ]; then edition_args=(); else edition_args=(--rs-edition "$edition"); fi
  "$@" --model-dir "$fixture_dir/models" :rust --output "$out" "${edition_args[@]}"
  RUSTFLAGS="-D warnings" cargo build --manifest-path "$out/Cargo.toml"
done

#!/usr/bin/env bash
# scripts/check-ci.sh — local + CI verification flow for baboon.
#
# Single source of truth. Both `.github/workflows/baboon-build.yml` and
# humans (pre-commit / pre-push) invoke this script — when CI checks
# change, this script changes; when this script changes, CI changes.
# No drift.
#
# Modes (composable):
#   build       — `mdl :build :test`. The cross-platform build + test
#                 matrix that historically caught CI failures local
#                 `sbt baboonJVM/compile` missed (e.g., JS-side
#                 inexhaustive-match warnings under -Wconf fatal).
#   smoke       — Native-image binary portability + simple round-trip.
#                 Runs the produced graalvm-native-image binary against
#                 a small model and ensures it links + executes outside
#                 the build sandbox.
#   editors     — Tree-sitter editor grammar tests.
#                 Wraps `test/editors/test-tree-sitter.sh`.
#   acceptance  — Acceptance + service-acceptance Python harnesses.
#                 Mirrors the `acceptance-tests` CI job.
#   all         — Everything above, in CI order.
#
# Default mode is `build` (the most-common pre-commit verification —
# fast enough to run before every commit, catches CI-fatal warnings).
# Use `all` before pushing to a PR or after non-trivial refactors.
#
# Assumed environment: a working nix devshell OR a local install of
# {sbt, GraalVM, mdl, dart, python3, ...}. CI wraps invocations in
# `nix develop --command bash scripts/check-ci.sh <mode>`. Locally either
# enter the devshell first (`nix develop`) or rely on system tools.
#
# This script is intentionally OS-aware (Linux-only commands gated)
# and platform-detectable: macOS uses `otool -L` instead of `ldd`,
# Windows is not supported (CI for Windows uses the YAML's inline
# build steps directly).

set -euo pipefail

# Default to single mode unless multiple modes are explicitly listed.
if [[ $# -eq 0 ]]; then
    MODES=(build)
else
    MODES=("$@")
fi

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

NATIVE_BIN=baboon-compiler/.jvm/target/graalvm-native-image/baboon

run_build() {
    # Equivalent to the linux+macos+windows CI "build" steps:
    #   mdl [--github-actions] [--without-nix] :build :test
    # The `:build` action triggers `sbt +compile` (cross JVM/JS) which
    # is the exact compilation path that surfaced the BaboonJS.scala
    # inexhaustive-match -Wconf-fatal failure on 2026-04-29 (commit
    # 2de517b). Never run plain `sbt baboonJVM/compile` as a
    # CI-equivalent check — it skips the JS cross-build.
    #
    # Flags honour the env when invoked from CI:
    #   GITHUB_ACTIONS=true        → --github-actions (CI grouped log)
    #   BABOON_CI_WITHOUT_NIX=true → --without-nix    (macOS / Windows)
    local mdl_flags=()
    [[ "${GITHUB_ACTIONS:-}" == "true" ]] && mdl_flags+=(--github-actions)
    [[ "${BABOON_CI_WITHOUT_NIX:-}" == "true" ]] && mdl_flags+=(--without-nix)
    # BABOON_CI_SEQ=true forces serial action execution. Local
    # workaround for the documented Kotlin compiler daemon OOM seen
    # under default parallel-matrix execution on memory-constrained
    # machines (M16 closeout note: "Pre-existing Kotlin OOM in
    # parallel matrix"). CI runs default-parallel; humans may need
    # this on laptops with <16 GB RAM.
    [[ "${BABOON_CI_SEQ:-}" == "true" ]] && mdl_flags+=(--seq)
    echo ">>> mdl ${mdl_flags[*]:-} :build :test"
    mdl "${mdl_flags[@]}" :build :test
}

run_smoke() {
    # Equivalent to the linux CI "Validate binary portability"
    # step. macOS uses `otool -L` and an extra "no nix store" check.
    if [[ ! -x "$NATIVE_BIN" ]]; then
        chmod +x "$NATIVE_BIN" || {
            echo "Native image not found at $NATIVE_BIN — run 'scripts/check-ci.sh build' first." >&2
            exit 2
        }
    fi
    echo "=== file ==="
    file "$NATIVE_BIN"

    case "$(uname -s)" in
        Linux)
            echo "=== ldd ==="
            ldd "$NATIVE_BIN" 2>&1 || true
            ;;
        Darwin)
            echo "=== otool -L ==="
            otool -L "$NATIVE_BIN"
            echo "=== check no nix store references ==="
            if otool -L "$NATIVE_BIN" | grep -q /nix/store; then
                echo "ERROR: binary has Nix store dependencies" >&2
                exit 1
            fi
            ;;
        *)
            echo "warning: smoke linker check skipped on $(uname -s) (Linux/Darwin only)" >&2
            ;;
    esac

    echo "=== smoke test ==="
    "$NATIVE_BIN" \
        --model-dir ./test/conv-test \
        :cs --output /tmp/baboon-validate-cs \
        :scala --output /tmp/baboon-validate-sc
}

run_editors() {
    # Equivalent to the test-editors CI job.
    # Initializes submodules (idempotent) before running.
    echo ">>> test/editors/test-tree-sitter.sh"
    git submodule update --init --recursive
    bash test/editors/test-tree-sitter.sh .
}

run_acceptance() {
    # Equivalent to the acceptance-tests CI job. Requires the native
    # binary; will build it if missing.
    if [[ ! -x "$NATIVE_BIN" ]]; then
        local mdl_flags=()
        [[ "${GITHUB_ACTIONS:-}" == "true" ]] && mdl_flags+=(--github-actions)
        [[ "${BABOON_CI_WITHOUT_NIX:-}" == "true" ]] && mdl_flags+=(--without-nix)
        echo ">>> mdl ${mdl_flags[*]:-} :build (prerequisite for acceptance tests)"
        mdl "${mdl_flags[@]}" :build
    fi
    chmod +x "$NATIVE_BIN"

    local nproc_count
    nproc_count="$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)"

    echo ">>> python3 test/acceptance/run_acceptance.py"
    python3 test/acceptance/run_acceptance.py \
        --baboon "$NATIVE_BIN" \
        --target ./target/acceptance \
        --parallelism "$nproc_count"

    echo ">>> python3 test/acceptance/run_service_acceptance.py"
    python3 test/acceptance/run_service_acceptance.py \
        --baboon "$NATIVE_BIN" \
        --target ./target/service-acceptance \
        --parallelism "$nproc_count"
}

run_all() {
    # CI order: build first (gates everything), then smoke, then
    # editors, then acceptance. Editors is independent and could be
    # parallelised but cost is minor.
    run_build
    run_smoke
    run_editors
    run_acceptance
}

for mode in "${MODES[@]}"; do
    case "$mode" in
        build)      run_build ;;
        smoke)      run_smoke ;;
        editors)    run_editors ;;
        acceptance) run_acceptance ;;
        all)        run_all ;;
        *)
            echo "Unknown mode: $mode" >&2
            echo "Usage: $0 [build|smoke|editors|acceptance|all]..." >&2
            echo "Default: build" >&2
            exit 2
            ;;
    esac
done

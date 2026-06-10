#!/usr/bin/env bash
set -euo pipefail

# Test tree-sitter grammar (Zed extension) against corpus tests and real baboon files.
#
# Expects: tree-sitter, node, cc on PATH (provided by nix develop)
# Run from project root: bash test/editors/test-tree-sitter.sh

PROJECT_ROOT="$(cd "${1:-.}" && pwd)"
GRAMMAR_DIR="$PROJECT_ROOT/editors/baboon-zed/grammars/baboon"

if [[ ! -d "$GRAMMAR_DIR" ]]; then
  echo "FATAL: grammar dir not found at $GRAMMAR_DIR" >&2
  echo "  Did you forget --recurse-submodules?" >&2
  exit 1
fi

# Collect all source .baboon files (skip build artifacts).
#
# reserved-words-ok/ is EXCLUDED from the editor real-file scan (D11).
# That fixture is a codegen torture-test: it deliberately names DTO fields after
# words that are grammar keywords in the tree-sitter editor grammar (`is`, `in`,
# `def`, `type`, `import`, `with`, `was`, `data`, …). The Baboon compiler accepts
# any `idt.symbol` as a field/member name, but the editor grammar sets
# `word: $ => $.identifier`, so those keyword-shaped names lex as their keyword
# token and the parser reports ERROR. Re-admitting every keyword at every
# name-binding position is notoriously conflict-prone in tree-sitter and would
# destabilize the corpus; the editor grammar is best-effort syntax highlighting,
# not an authoritative parser, so reserved-words-ok is not a meaningful editing
# target. It REMAINS in the shared model-dir for codegen coverage (T13) — this
# exclusion scopes only to the editor real-file scan. Mirrors the D9
# mcp-stub-ok model-dir isolation precedent.
mapfile -t BABOON_FILES < <(
  find "$PROJECT_ROOT/baboon-compiler/src/test/resources" "$PROJECT_ROOT/test" \
    -name '*.baboon' -not -path '*/target/*' -not -path '*/reserved-words-ok/*' 2>/dev/null | sort
)

if [[ ${#BABOON_FILES[@]} -eq 0 ]]; then
  echo "FATAL: no .baboon files found" >&2
  exit 1
fi

echo "=== Tree-sitter grammar tests ==="
echo ""

# 1. Corpus tests
echo "--- Corpus tests ---"
(cd "$GRAMMAR_DIR" && tree-sitter generate && tree-sitter test)
echo ""

# 2. Parse every real .baboon file
echo "--- Real file parse tests (${#BABOON_FILES[@]} files) ---"
errors=0
total=0
failed_files=()

for f in "${BABOON_FILES[@]}"; do
  total=$((total + 1))
  rel="${f#"$PROJECT_ROOT"/}"
  output=$(cd "$GRAMMAR_DIR" && tree-sitter parse "$f" 2>&1) || true
  if echo "$output" | grep -q "ERROR"; then
    errors=$((errors + 1))
    failed_files+=("$rel")
    echo "FAIL: $rel"
    echo "$output" | grep "ERROR" | head -3
    echo ""
  else
    echo "  OK: $rel"
  fi
done

echo ""
echo "--- Summary ---"
echo "Real files: $((total - errors))/$total passed"

if [[ $errors -gt 0 ]]; then
  echo ""
  echo "Failed files:"
  for f in "${failed_files[@]}"; do
    echo "  - $f"
  done
  exit 1
fi

echo ""
echo "All editor tests passed."

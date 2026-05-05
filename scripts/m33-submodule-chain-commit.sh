#!/usr/bin/env bash
# =============================================================================
# m33-submodule-chain-commit.sh
#
# One-shot script that stages and commits the PR-33.6 tree-sitter corpus file
# through the three-level submodule chain:
#
#   1. Inner submodule:  editors/baboon-zed/grammars/baboon
#      Commits:          test/corpus/m33-template-arms.txt
#
#   2. Middle submodule: editors/baboon-zed
#      Commits:          updated grammars/baboon pointer (SHA from step 1)
#
#   3. Parent repo:      <repo-root>
#      Commits:          updated editors/baboon-zed pointer (SHA from step 2)
#
# Chain pattern (documented in PR-29.8 history, CLAUDE.md §9 "Worktrees for
# parallel edits"): git submodules record a commit SHA, not a branch. Each
# level must be committed in bottom-up order so that the pointer recorded at
# each level references the newly-created commit in the level below.
#
# Usage:
#   bash scripts/m33-submodule-chain-commit.sh
#
# The script does NOT push. The user pushes each submodule and the parent repo
# manually (in bottom-up order: inner → middle → parent).
#
# Pre-conditions checked:
#   - The corpus file exists in the inner submodule's working tree.
#   - The parent repo's working tree is clean except for the expected
#     submodule dirty marker: "editors/baboon-zed" modified pointer.
#     Commit or stash all other parent-repo changes before running.
#     To temporarily allow extra dirty paths, set the env var:
#       M33_CHAIN_ALLOW_DIRTY=path1,path2,...
# =============================================================================

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MIDDLE_DIR="${REPO_ROOT}/editors/baboon-zed"
INNER_DIR="${MIDDLE_DIR}/grammars/baboon"
CORPUS_REL="test/corpus/m33-template-arms.txt"
CORPUS_ABS="${INNER_DIR}/${CORPUS_REL}"

# ─── precondition: corpus file must exist ─────────────────────────────────────
if [[ ! -f "${CORPUS_ABS}" ]]; then
  echo "ERROR: corpus file not found: ${CORPUS_ABS}" >&2
  echo "  Run the tree-sitter corpus generation step (PR-33.6) first." >&2
  exit 1
fi

# ─── precondition: inner submodule must be a git repo ─────────────────────────
if ! git -C "${INNER_DIR}" rev-parse --git-dir > /dev/null 2>&1; then
  echo "ERROR: ${INNER_DIR} is not a git repository." >&2
  echo "  Run: git submodule update --init --recursive" >&2
  exit 1
fi

# ─── precondition: middle submodule must be a git repo ────────────────────────
if ! git -C "${MIDDLE_DIR}" rev-parse --git-dir > /dev/null 2>&1; then
  echo "ERROR: ${MIDDLE_DIR} is not a git repository." >&2
  echo "  Run: git submodule update --init --recursive" >&2
  exit 1
fi

# ─── precondition: parent repo working tree clean except expected submodule ────
# We expect the parent repo status to show at most "editors/baboon-zed" as modified.
# Additional paths may be allowed via M33_CHAIN_ALLOW_DIRTY=path1,path2,...
PARENT_STATUS=$(git -C "${REPO_ROOT}" status --porcelain)
PARENT_DIRTY="${PARENT_STATUS}"
PARENT_DIRTY=$(echo "${PARENT_DIRTY}" | command grep -v '^ M editors/baboon-zed$' || true)
if [[ -n "${M33_CHAIN_ALLOW_DIRTY:-}" ]]; then
  IFS=',' read -ra _EXTRA_PATHS <<< "${M33_CHAIN_ALLOW_DIRTY}"
  for _PATH in "${_EXTRA_PATHS[@]}"; do
    PARENT_DIRTY=$(echo "${PARENT_DIRTY}" | command grep -vF "${_PATH}" || true)
  done
fi
if [[ -n "${PARENT_DIRTY}" ]]; then
  echo "ERROR: parent repo has unexpected uncommitted changes:" >&2
  echo "${PARENT_DIRTY}" >&2
  echo "  Commit or stash them before running this script." >&2
  echo "  To allow extra paths, set: M33_CHAIN_ALLOW_DIRTY=path1,path2,..." >&2
  exit 1
fi

echo "=== Step 1: commit corpus file in inner submodule ==="
git -C "${INNER_DIR}" add "${CORPUS_REL}"

# Verify the file is staged
INNER_STAGED=$(git -C "${INNER_DIR}" diff --cached --name-only)
if ! echo "${INNER_STAGED}" | command grep -qF "${CORPUS_REL}"; then
  echo "ERROR: ${CORPUS_REL} is not staged in inner submodule." >&2
  echo "  git status: $(git -C "${INNER_DIR}" status --short)" >&2
  exit 1
fi

git -C "${INNER_DIR}" commit -m "PR-33.6: add m33-template-arms tree-sitter corpus (5 cases: plus-arm, minus-arm, caret-arm, mixed-arms, nested-arg)"

INNER_SHA=$(git -C "${INNER_DIR}" rev-parse HEAD)
echo "  inner submodule committed: ${INNER_SHA}"

echo ""
echo "=== Step 2: commit updated inner-submodule pointer in middle submodule ==="
git -C "${MIDDLE_DIR}" add grammars/baboon
git -C "${MIDDLE_DIR}" commit -m "PR-33.6: bump grammars/baboon pointer to ${INNER_SHA} (m33-template-arms corpus)"

MIDDLE_SHA=$(git -C "${MIDDLE_DIR}" rev-parse HEAD)
echo "  middle submodule committed: ${MIDDLE_SHA}"

echo ""
echo "=== Step 3: commit updated middle-submodule pointer in parent repo ==="
git -C "${REPO_ROOT}" add editors/baboon-zed
git -C "${REPO_ROOT}" commit -m "PR-33.6: bump editors/baboon-zed pointer to ${MIDDLE_SHA} (m33 tree-sitter corpus chain)"

PARENT_SHA=$(git -C "${REPO_ROOT}" rev-parse HEAD)
echo "  parent repo committed: ${PARENT_SHA}"

echo ""
echo "=== Done. Push order (bottom-up, do NOT push parent before inner/middle): ==="
echo "  1. cd ${INNER_DIR}  && git push"
echo "  2. cd ${MIDDLE_DIR} && git push"
echo "  3. cd ${REPO_ROOT}  && git push"

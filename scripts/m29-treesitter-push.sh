#!/usr/bin/env bash
# m29-treesitter-push.sh — ship the M29 tree-sitter grammar changes
# through the 3-level submodule chain in one shot.
#
# Layout:
#   baboon (outer repo)
#     └── editors/baboon-zed     (submodule → 7mind/baboon-zed#main)
#           └── grammars/baboon  (submodule → 7mind/baboon-zed-grammar#main)
#
# Both submodules push directly to main (per user direction — no PRs).
# The outer repo's pointer bump is committed locally; user pushes the
# outer repo themselves on whatever branch they're working on.
#
# Prerequisites: the tree-sitter changes must already be in the inner
# submodule's working tree:
#   - grammars/baboon/grammar.js           (modified)
#   - grammars/baboon/src/{grammar.json,node-types.json,parser.c}  (regenerated)
#   - grammars/baboon/test/corpus/m29-templates.txt   (new)
#
# If any are missing, run a regenerate step first (e.g. dispatch a subagent
# or hand-edit + `tree-sitter generate` in grammars/baboon).
#
# Usage: bash scripts/m29-treesitter-push.sh
# Re-run safe up to the point where each submodule's commit lands.

set -euo pipefail

REPO_ROOT="$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
INNER="$REPO_ROOT/editors/baboon-zed/grammars/baboon"
MIDDLE="$REPO_ROOT/editors/baboon-zed"

echo "==> Outer repo:  $REPO_ROOT"
echo "==> Middle:      $MIDDLE"
echo "==> Inner:       $INNER"
echo

# --- Sanity checks --------------------------------------------------------

[ -e "$INNER/.git" ] || { echo "FAIL: $INNER is not a git repo (no .git)"; exit 1; }
[ -e "$MIDDLE/.git" ] || { echo "FAIL: $MIDDLE is not a git repo (no .git)"; exit 1; }
# Submodules use a .git FILE (gitlink), not a directory — both forms are valid.
git -C "$INNER" rev-parse --git-dir >/dev/null || { echo "FAIL: $INNER is not a usable git working tree"; exit 1; }
git -C "$MIDDLE" rev-parse --git-dir >/dev/null || { echo "FAIL: $MIDDLE is not a usable git working tree"; exit 1; }

# Inner submodule expected files
for f in grammar.js src/grammar.json src/node-types.json src/parser.c; do
  if ! git -C "$INNER" diff --quiet -- "$f" 2>/dev/null && \
     ! git -C "$INNER" diff --cached --quiet -- "$f" 2>/dev/null; then
    : # has staged or unstaged changes — good
  fi
done

INNER_DIRTY=$(git -C "$INNER" status --porcelain)
if [ -z "$INNER_DIRTY" ]; then
  echo "FAIL: $INNER has no pending changes — nothing to ship."
  echo "      Restore grammar.js / corpus / regenerated parser first."
  exit 1
fi

echo "==> Inner submodule pending changes:"
git -C "$INNER" status --porcelain
echo
read -r -p "Proceed with commit + push to inner / middle / outer? [y/N] " ans
[ "$ans" = "y" ] || [ "$ans" = "Y" ] || { echo "Aborted."; exit 1; }

# --- Step 1: inner submodule (grammars/baboon) ----------------------------

echo
echo "==> [1/3] Inner submodule: grammars/baboon"
cd "$INNER"

# Make sure we're on main and up to date with origin/main before adding
# our changes — avoids force-push surprises.
git fetch origin main
git checkout main
git pull --ff-only origin main || {
  echo "FAIL: cannot fast-forward inner submodule's main from origin."
  echo "      Resolve manually (rebase / merge) before re-running."
  exit 1
}

# Stage everything currently modified or untracked in the inner submodule.
# (If you want to scope tighter, edit this `git add` line.)
git add -A
git status --short
git commit -m "Grammar: M29 template syntax (data/adt/contract/service [T]) + alias derivations"
git push origin main

INNER_SHA=$(git rev-parse HEAD)
echo "==> Inner pushed:  $INNER_SHA"

# --- Step 2: middle submodule (editors/baboon-zed) ------------------------

echo
echo "==> [2/3] Middle submodule: editors/baboon-zed"
cd "$MIDDLE"

git fetch origin main
git checkout main
git pull --ff-only origin main

# The submodule pointer for grammars/baboon now needs to advance to the
# commit we just pushed. `git submodule update --remote --merge` or an
# explicit add of the directory both work.
git add grammars/baboon
git diff --cached --submodule
git commit -m "Bump grammars/baboon submodule for M29 template syntax ($INNER_SHA)"
git push origin main

MIDDLE_SHA=$(git rev-parse HEAD)
echo "==> Middle pushed: $MIDDLE_SHA"

# --- Step 3: outer repo (baboon) ------------------------------------------

echo
echo "==> [3/3] Outer repo: baboon"
cd "$REPO_ROOT"

# Same: stage the bumped submodule pointer.
git add editors/baboon-zed
git diff --cached --submodule
git commit -m "Bump editors/baboon-zed submodule for M29 grammar ($MIDDLE_SHA)"

OUTER_SHA=$(git rev-parse HEAD)
echo "==> Outer committed (NOT pushed): $OUTER_SHA"
echo
echo "Outer repo's submodule pointer is committed locally. Push the outer"
echo "repo manually when ready:"
echo "    git push origin $(git branch --show-current)"
echo
echo "Done."

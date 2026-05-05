## Archived ledgers — M29 (BAB-A04 generics) through M31 + M32-prep

Frozen 2026-05-05 at session start of multi-version codec facade upstream
(proposal.md). HEAD at archive time: `0d9d7165` ("meta version bumped to 16
to accomodate legacy values" — the M32 wire-version bump that prompted this
upstream pass).

- `tasks.md` — milestone + PR breakdown for M29 → M31 + M32-prep.
- `defects.md` — defect ledger keyed by PR id (PR-29P.x .. PR-31.x).

**Predecessor ledger (chained):** `docs/archive/20260503-bab-any-anyopaque-ledgers/`
covers M1–M28. The chain is M1–M28 → M29–M31/M32-prep → (active root).

Cross-cutting invariants locked in M29–M31 remain authoritative — consult these
ledgers when working on user-defined generics (BAB-A04), template registries,
docstring/comment preservation (M30), upstream defect handling against
multi-stack consumers (M31), or LSP exhaustive-match surfaces. Active ledgers
at the repo root supersede the milestone-tracking entries here, but never the
locked invariants.

### Notable in-progress carry-over

`tasks.md` lists `[~] M32` (META_VERSION_1 1→16 bump). Commit `0d9d7165`
landed the byte change in all 11 runtime files but PR-32.1's full close-out
(per-backend test fixture updates listed in the M32 entry) post-dates the
freeze and is owned by the upstream-facade work that opens with this
archive freeze. The new active ledger picks up M32 follow-up there.

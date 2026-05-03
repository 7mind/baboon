# Baboon — Task Ledger

Authoritative ledger of planned and completed work. Governing brief: pre-M29 CI hardening blocks BAB-A04 (user-defined generics) per user 2026-05-03 ("we will only proceed to generics once CI is green").

> **Predecessor ledgers (frozen):** `docs/archive/20260503-bab-any-anyopaque-ledgers/{tasks,defects}.md`
> Cover M1–M28 (`any`/AnyOpaque, identifiers, ADT inheritance, JSON/UEBA codecs, map-key encoding, wire-form canonicalisation). Locked invariants there remain authoritative.

Status: `[ ]` planned · `[~]` in progress · `[x]` done · `[!]` blocked

---

## Milestones (high-level)

- [~] **M29-prep** — Pre-M29 CI hardening. Two defects: CI-01 (RTCodecTest map-key ordering), CI-02 (acceptance-tests sbt resolution). Goal: both CI jobs green on `wip/ids-and-adts`.
- [ ] **M29** — User-defined generics (BAB-A04). Parked until M29-prep is `[x]`.

---

## Milestone M29-prep — PR breakdown

Detail: see `docs/archive/20260503-bab-any-anyopaque-ledgers/` for historical context, especially M28 close-out (`docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md`) which explicitly flagged PR-28.4-D02 (the prime suspect for CI-01).

- [ ] **PR-29P.1** — Fix CI-01: `RTCodecTest` JSON→UEBA→JSON roundtrip fails on multi-entry maps. Audit generator-wide map-iteration sort-key emit across all 9 codec backends; replace typed `_._1.toString` with encoded-wire-form-key sort. Add multi-entry-map regression to RTCodecTest fixture and m26 fixture. Resolves PR-28.4-D02.
- [ ] **PR-29P.2** — Fix CI-02: `acceptance-tests` sbt 1.11.7 resolution `forbidden` inside `nix develop --ignore-environment`. Pick lowest-blast-radius fix from (a) drop `--ignore-environment`, (b) flake-closure sbt-launcher, (c) prepend `:flake-refresh`.
- [ ] **PR-29P.3** — Close-out: confirm `mdl --seq :build :test`, `:test-acceptance`, `:test-service-acceptance` green locally; push and verify both CI jobs green; write session log.

---

## Milestone M29 — PR breakdown (parked)

Detail: see PR-29.1 spec doc once seeded. Locked design decisions from user 2026-05-03:

1. ADTs, Contracts, Services are all generifiable. AST-level templating handles them uniformly.
2. Self-reference is forbidden. Only DAG-shaped template-and-instantiation graphs are valid.
3. The **only** way to instantiate is through a type alias. Field-position instantiation and nested instantiation in alias RHS are forbidden — intermediate aliases required.
4. Monomorphisation identifier = alias id. No synthetic id `X<i32>` exposed.
5. Templates not subject to migration; instances (concrete `Typedef.User` under alias id) are.
6. `: derived[…]` written **only on the alias**, propagates to monomorphisation.

- [ ] **PR-29.1** — Spec + grammar + decision doc `docs/spec/generics.md`.
- [ ] **PR-29.2** — Parser: type-param head on declarations.
- [ ] **PR-29.3** — Parser: type alias instantiation RHS.
- [ ] **PR-29.4** — Typer: template registry; templates never become `DomainMember`.
- [ ] **PR-29.5** — Typer: monomorphisation via AST substitution; alias-id canonical.
- [ ] **PR-29.6** — Typer: dependency ordering (DAG-only); cycle diagnostic.
- [ ] **PR-29.7** — Validator: forbidden positions; new `TyperIssue` cases (3-site exhaustive-match update).
- [ ] **PR-29.8** — Diagnostics + LSP polish.
- [ ] **PR-29.9** — Cross-language emission smoke fixtures (`Page[T]`, `Envelope[T, E]`).
- [ ] **PR-29.10** — Acceptance fixture `m29` — JSON+UEBA roundtrip across all 9 backends.
- [ ] **PR-29.11** — Docs catch-up.

### Negative test matrix (locked)

1. `field: Foo[Bar]` — instantiation in field position.
2. `type Y = Foo[Bar[i32]]` — nested instantiation in alias RHS.
3. `data X[T] { a: lst[T] }; type XInt = X[i32, str]` — arity mismatch.
4. `data X[T, T] { … }` — duplicate type-param name.
5. `data X[T] { rec: X[T] }` — self-reference (forbidden).
6. `field: T` outside a template — placeholder leak.
7. `type Y = X` — template referenced without instantiation.
8. `type Y = i32[X]` — instantiating a non-generic.
9. `type A = X[B]; type B = X[A]` — cycle through aliases.

---

## Cross-cutting architectural notes (locked)

- [x] **M28-N01 / N02 inheritance** — wire-form canonical for tso (`±HH:MM`), Scala u64 JSON map-key (`Long.toUnsignedString` / `BaboonJsonCodec.decodeKeyU64`), Java u64 parity, Kotlin native ULong. Authoritative copies live in archived M1–M28 ledgers.
- [x] **Map-iteration sort-key invariant (NEW)** — encoder and decoder MUST agree on object-entry order under JSON→UEBA→JSON roundtrips. Encoded-wire-form key is canonical, NOT typed-key.toString. Lands in PR-29P.1.
- [x] **PR-28.4-D02 status** — re-classified from "deferred" to "active blocker"; resolves in PR-29P.1 with a note linking back to the archived defect.
- [ ] **CI sandbox policy for Nix `--ignore-environment`** — decide in PR-29P.2 whether the acceptance step needs the strict sandbox at all, or whether the flake closure is sufficient. Cross-cutting because the same pattern may apply to future test-* actions.

---

## Completed

(none yet for M29-prep)

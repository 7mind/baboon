# MFACADE + M32 — Multi-version codec facade upstream + byte-1 unification

**Date seeded:** 2026-05-06.
**Predecessors:**
- `docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md` — Q1–Q14 with user answers.
- `proposal.md` — upstream design source-of-truth (root of repo).
- `/tmp/exchange/Baboon/` — production C# reference (read-only; project-specific names MUST NOT leak into the repo per Q5 answer).
- `docs/drafts/20260424-1738-any-opaque-fields.md` — the field-level AnyMeta surface that Q11's "several forms" referenced.

This doc supersedes the in-flight M32 carry-over entry in `tasks.md`. It collapses what was previously two separate items (M32 byte-bump + MFACADE upstream) into one milestone because Q2/Q10 resolve the byte-16 question by retiring it: M32's reason-to-exist disappears.

---

## 1. Milestone summary

**Scope.** Bring baboon's in-tree codec-facade infrastructure to spec-parity with `proposal.md`'s design and the production C# reference (`/tmp/exchange/Baboon/`):

1. **Wire-format unification (M32 close).** Drop byte 16 from `BaboonTypeMeta` across all 11 runtimes; emit and decode byte 1 with the proposal's all-fields layout. Top-level envelope becomes wire-identical to the production reference (modulo backend formatter quirks like Swift sorted-keys, which are non-defects per m29/m33 history).
2. **Forward-compat reader semantics.** Bin-side `ReadMeta` returns "no envelope" (None/null) for unknown `metaVersion`, matching the JSON path and the production reference. Lets future `metaVersion=N` payloads coexist with older readers gracefully.
3. **JSON `$mv` value type = number.** Per Q7 + Q10. Writers (when explicit) emit numeric `$mv: 1`; readers accept numeric AND string (back-compat with M28-vintage Java/Kotlin/Dart/Swift fixtures that produced string `$mv`). Once `$mv` is elided on canonical-version write, readers rarely see it in practice.
4. **Facade API parity** with proposal §4 across all 11 backends: add `DecodeFromBin/JsonLatest<T>` (compose `Convert(Decode(...))`), add `Latest(domain)` query (already in C# — verify others), add optional `Preload()`.
5. **`BaboonExt`-style helpers** mirroring the production reference: `Version(IBaboonGenerated)`, `DomainVersion(IBaboonGenerated)`, `BaboonUnmodifiedSinceVersion(IBaboonGenerated)`, `UnmodifiedSinceVersion(IBaboonMeta, name)`. Cuts noisy callsites in the facade.
6. **C# `TypeIsAdt` widening (Q13).** Reference uses `IsAbstract || IsInterface`; baboon C# uses `IsInterface` only. Verify what baboon emits today (interface-only or also abstract-class ADTs) and widen if the gap is real. Other backends use structural / codegen-marker checks — confirm no equivalent gap.
7. **Per-domain `Domain<DomainId>Facade` codegen (Q3 + Q14).** New compiler flag, **per-target-prefixed** (`--cs-generate-domain-facade=true`, `--scala-generate-domain-facade=true`, …, default on). Generated class registers `(domainVersion, codecsJson, codecsBin, conversions, meta)` for every version known at gen time. Application uses `rootFacade.Register(domainFacade)` to compose.
8. **Bidirectional support for legacy + future formats (Q9).** Both decode and emit are required. For the unified byte-1 case, this is automatic (one envelope). The "several forms" planned for the field-level AnyMeta (kinds `0x00..0x07`) are already shipped at the field level (per `docs/drafts/20260424-1738-any-opaque-fields.md`); a future top-level kind-prefixed envelope (if/when needed) gets a fresh `metaVersion` byte allocation, NOT byte 16.
9. **Spec & docs.** New `docs/spec/codec-envelope.md` (canonical wire spec — top-level envelope, NOT the field-level AnyMeta which already lives in `docs/ueba-format.md` + `docs/json-codecs.md`). Cross-link.
10. **Conformance tests.** Round-trip every shape (DTO, ADT-via-parent, ADT-via-branch, enum) through the envelope at top level, both binary and JSON, all 11 backends. Cross-version decode using `domainVersionMinCompat`. Captured-byte fixtures against reference's known wire form so future drift is caught.

**Exit criteria.**
- `mdl --simple-log :build :test` GREEN; cross-language acceptance 200+ rows still pass; envelope fixtures byte-canonical against captured production samples (modulo backend formatter quirks).
- Zero new TyperIssue cases added (facade work is runtime-side; codegen surface is per-domain class generation only).
- `proposal.md` either marked "upstreamed" or annotated with the deviations baboon ships.

**Out of scope.**
- The "several forms" planned top-level kind-prefixed envelope. Only ratify the wire byte allocation policy; do NOT implement until there's a demand. Per Q10 + Q11: byte 1 is full-meta; if/when subset-metadata top-level is wanted, allocate a fresh byte (e.g. 2) — never re-use 16.
- `FireLogger`-flavoured overloads (Q6 — no logging needed).
- Per-language async surface (proposal §10.3 — out of scope per the proposal itself).
- Streaming envelope (proposal §10.4 — separate work).

---

## 2. PR-level breakdown

Tentative; sized for review-loop iterations. Adjust during planning of each phase.

### MFACADE-PR-1 — Byte-1 unification across 11 runtimes

- **Scope.** Flip `META_VERSION_1` from 16 → 1 in all 11 backends (`baboon-runtime/{cs,scala,rust,java,kotlin,kotlin-kmp,dart,typescript,python,swift}/...`). Update test fixtures M32 touched (`AnyMetaCodec*` across cs/dt/jv/rs/sw/ts) to match. Reverse the M32 commit `0d9d7165` byte change.
- **Files.** 10 runtime files + 6 test fixture files (the M32-commit footprint, mirrored).
- **Success.** `mdl --seq :build :test` GREEN; all `AnyMetaCodec*` tests pass; cross-language acceptance unaffected (byte-1 envelope is what the m29/m33 fixtures already emit semantically — their tests assert semantic round-trip, not byte-canonical).
- **Closes:** M32 / PR-32.1 carry-over (becomes a no-op revert).

### MFACADE-PR-2 — Reader forward-compat: bin path returns null on unknown metaVersion

- **Scope.** `BaboonCodecsFacade.DecodeFromBin` (or runtime equivalent in each backend) — match the JSON path: return null/`None`/`Option.empty` instead of throwing `DecoderFailure` when `BaboonTypeMeta.ReadMeta(reader)` returns null. JSON path is already correct in C#; verify per-backend.
- **Files.** 10 facade files (Kotlin/Kotlin-KMP have the bin-meta path elsewhere — check).
- **Success.** New unit test per backend: bin stream with `metaVersion=2` → `DecodeFromBin` returns null, no exception thrown. Existing tests unchanged.

### MFACADE-PR-3 — JSON `$mv` value type = number

- **Scope.** Writer change: when JSON `$mv` is emitted (today: never on canonical version), use numeric `1` not string `"1"`. Reader change: accept both numeric and string `$mv` (back-compat). Per CLAUDE.md / M28 history, backend JSON serialiser quirks (Java Jackson, Swift JSONSerialization) need lockstep verification. Per Q7's recommendation, prefer (a) "always write `$mv`" — but the in-tree pattern is "elide for canonical version" and that is wire-identical to the reference. Decision: keep elide-for-canonical (no asymmetric production drift); update parse path to numeric-first, string-fallback.
- **Files.** 11 runtime files + a per-backend reader test that exercises both numeric and string explicit `$mv: 1` inputs.
- **Success.** Per-backend `AnyMetaCodec` reader tests pass on numeric-and-string inputs; existing reader behaviour unchanged.

### MFACADE-PR-4 — Facade API parity (`DecodeFromBin/JsonLatest`, `Latest`, `Preload`)

- **Scope.** Add the missing methods per backend. `DecodeFromBin/JsonLatest<T>` is pure composition over existing `DecodeFromBin/Json` + `Convert`. `Latest(domain)` may already exist in C# — verify and replicate. `Preload()` is optional sync-eval over all `Lazy<>` registries.
- **Files.** 11 facade files.
- **Success.** New per-backend tests exercise each new method against an m29-or-m33 holder with `IBaboonGeneratedLatest` constraint (Q4 confirms compiler emits the marker — verify via grep before relying).

### MFACADE-PR-5 — `BaboonExt`-style helpers + C# `TypeIsAdt` widening (Q13)

- **Scope.** Add the four extension methods (`Version`, `DomainVersion`, `BaboonUnmodifiedSinceVersion`, `UnmodifiedSinceVersion`) per language idiom. Investigate baboon's emitted ADT shape per backend: if any backend (likely C#) emits abstract-class ADTs alongside interfaces, widen the `TypeIsAdt` predicate. Verify via grep before changing.
- **Files.** 11 runtime files (helpers); `baboon-runtime/cs/BaboonTypeMeta.cs` for the C# widening if confirmed needed.
- **Success.** Helpers compile and existing facade callsites can optionally use them. ADT widening covered by an ADT-via-parent encode round-trip test.

### MFACADE-PR-6 — Per-domain facade codegen (Q3 + Q14)

- **Scope.** Compiler emits `Domain<DomainId>Facade` per domain in each backend. New compiler flag per target: `--cs-generate-domain-facade=true`, `--scala-generate-domain-facade=true`, etc. Default on. Generated class has parameterless ctor that calls the right `Register(...)` overloads per registered version (current full-set; older decode-only or skeleton).
- **Files.** Compiler-side translator `*BaboonTranslator.scala` per backend (10 of them) — emit the new class alongside the existing `BaboonCodecs<json|ueba>` per-codec singletons. Plus CLI flag wiring in `BaboonCmdLine.scala` or wherever flags live. Plus per-language test stubs that exercise the generated facade (compile + register check).
- **Success.** Per-backend stub tests construct the generated `DomainXFacade`, compose into a root `BaboonCodecsFacade`, encode + decode + convert across versions. Existing applications without `Register(...)` boilerplate work unchanged when they switch to the generated facade.
- **M29 invariant:** the per-domain facade is codegen of REGISTRATION CALLS, not of new types. The codegen still doesn't see templates (they're already lowered).

### MFACADE-PR-7 — Spec doc + conformance tests

- **Scope.** New `docs/spec/codec-envelope.md` (canonical wire spec — top-level envelope only; reserve `metaVersion` allocation policy: byte 1 = all-fields full meta; bytes 2..255 unallocated; byte 16 retired). Cross-link from `docs/ueba-format.md` § "Any fields" and `docs/json-codecs.md` § "Any fields" (they document the field-level envelope; the top-level envelope is now its own doc).
- **Conformance tests.** Three categories per proposal §7.4:
  - Envelope round-trip every shape × {bin, JSON} × 11 backends (golden bytes).
  - Cross-version decode via `domainVersionMinCompat` / `$uv` (m20-style chained-evolution fixture extended).
  - Convert-chain incl. multi-step shortcut (existing test corpus already covers this — verify).
- **Captured-byte fixture.** A handful of byte-canonical samples against the production reference (read-only — copy bytes only, NOT names/domains-from-tmp-exchange per Q5). Lets us catch silent drift if the reference upstream-ships changes.
- **Files.** 1 new spec doc; per-backend new envelope round-trip tests; captured-byte fixtures under `test/conv-test/captured/`.

### MFACADE-PR-8 — Close-out

- **Scope.** Update `proposal.md` with deviations (or mark fully upstreamed). Session log under `docs/logs/`. M32 milestone close in `tasks.md`.
- **Success.** `mdl --simple-log :build :test` GREEN; close log written; all PRs landed.

---

## 3. Architectural notes (locked from Q1–Q14)

- **§3.a — Byte 1 is canonical.** Top-level envelope `metaVersion` byte = 1, all-fields layout per proposal §2. Byte 16 retired. Future allocations land at byte 2+; never reuse 16. (Q1, Q2, Q10.)
- **§3.b — Field-level AnyMeta is unchanged.** The "several forms" (kinds `0x00..0x07`) live at the field-level envelope and are already shipped. Top-level envelope and field-level envelope are SEPARATE; both are needed for full M33 + AnyOpaque coverage. (Q11.)
- **§3.c — Reader on unknown `metaVersion`: return null.** Both bin and JSON paths return None/null for unknown meta-version, matching production reference. Forward-compat preserved. (Q12.)
- **§3.d — JSON `$mv` is numeric on write, accepted as numeric or string on read.** Elide-for-canonical preserved. (Q7.)
- **§3.e — Bidirectional emit + decode.** Both paths always exist for the unified byte-1 envelope. Future top-level multi-form would also be bidirectional. (Q9.)
- **§3.f — No `FireLogger` overloads.** API contract is `Either<BaboonCodecException, T>` only. (Q6.)
- **§3.g — `IBaboonGeneratedLatest` already emitted by compiler** (Q4); verify via grep at PR-4 implementation time.
- **§3.h — Per-domain facade codegen flag is per-target-prefixed** (Q14): `--cs-generate-domain-facade=true`, `--scala-generate-domain-facade=true`, ..., default on per target.
- **§3.i — C# `TypeIsAdt` widening conditional on what baboon emits.** Reference uses `IsAbstract || IsInterface`; baboon uses `IsInterface` only. Investigate at PR-5 implementation time. (Q13.)
- **§3.j — Reference impl is read-only and project-private.** No FireSdk-specific names, paths, or types may leak into this repo. Use the reference for semantic comparison only. (Q5.)

---

## 4. Risks and unknowns

### R1. m29 / m33 cross-language acceptance after byte-1 flip

The acceptance harness verifies semantic round-trip (200/200 today). Byte-1 flip changes the prefix byte but not the structural shape; tests should still pass. Mitigation: PR-1 runs `mdl --seq :build :test-acceptance` to confirm 200+ green.

### R2. Captured byte-16 fixtures in test corpus

`test/{cs,dt,jv,rs,sw,ts}-stub/.../AnyMetaCodec*` were updated by M32 commit `0d9d7165` to expect byte 16. PR-1 reverts. The git-revert footprint is well-known (M32 commit's stat enumerated them).

### R3. Per-domain facade codegen — backwards-compat with existing applications

Existing apps construct `BaboonCodecsFacade` manually and call `Register(...)` for each version. Generated `DomainXFacade` adds an alternative path; the manual path stays valid. Mitigation: opt-in via flag default-on but documented; existing code unaffected.

### R4. C# `TypeIsAdt` widening — may break for users relying on current narrow check

Unlikely but possible. Mitigation: PR-5 verifies what baboon emits; widens only if abstract-class ADTs are real. If interface-only, the widening is a no-op for current users but still safer for forward-compat.

### R5. Q11 ambiguity residual — top-level multi-form byte allocation

Q10 + Q11 resolved with "drop byte 16; future multi-form gets a new byte". But there's no concrete spec for the top-level multi-form yet. Mitigation: PR-7 reserves byte allocation policy in `docs/spec/codec-envelope.md` so the future allocation is unambiguous. No code change required now.

### R6. The proposal text vs production C# reference may have drifted

Per Q5 answer, the reference at `/tmp/exchange/Baboon` is the source of truth; the proposal text is a snapshot. Mitigation: each PR reads the relevant reference file before implementing; deviations recorded in `proposal.md` at close-out.

---

## 5. Acceptance gates

In order, before close-out:

1. `sbt baboonJVM/compile` clean after each PR.
2. `sbt baboonJS/compile` clean (cross-build coverage).
3. `mdl :test-acceptance` 200+ rows green.
4. `mdl --seq :build :test` GREEN (per CLAUDE.md `--seq` for <16GB RAM machines).
5. `mdl :test-service-acceptance` 81/81 (MFACADE doesn't touch services).
6. New conformance tests (PR-7) green.
7. Spec doc lands; cross-links resolve.

---

*End of plan document.*

# Baboon — Defect Ledger: `any` / AnyOpaque (issue #69)

Defects discovered during review rounds. One top-level section per PR; defects within a PR are numbered `PR-NN-DMM`. Entries are append-only — status flips, never deletes.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-01 — Parser + raw AST

### [PR-01-D01] Parser unconditionally reserves `any`, breaking back-compat for a user type named `any`
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:50-64`
**Description:** The plan §PR 1.1 Risks explicitly says: "reserve `any` as a parser-level keyword only when followed by `[` or at the top-level type position — preserve back-compat" and likens it to `opt`/`lst`. Pre-PR, `opt`/`lst` are plain user identifiers at the parser level; the typer resolves them to builtins. Post-PR, bare `any` always parses as `AnyRef(None, None)`. A legacy model `data any { ... }` can still be defined but cannot be referenced via `f: any`, `f: opt[any]`, `f: foo.any`, or `f: any.Foo`. Regression is silent; errors surface far from the offending token.
**Root cause:** `typeRef` dispatches to `anyTypeRef` unconditionally when the head name is `any`, and the generic fallback path `.filter`s out any user type whose last segment is `any`. Both are wrong: the filter breaks `foo.any`; the unconditional dispatch breaks `any` as a user type name.
**Fix:** `DefDto.typeRef` now parses the identifier path atomically via `nonGenericTypeRef`, then dispatches: if head is exactly `any` (single segment, no prefix) AND `[` follows, route to `anyTypeRefArgs`; otherwise fall through to generic `typeParams.?`. The `.filter(...)` was removed. `kw.any` was removed from `Keywords.scala` — `any` is not a reserved keyword at the parser level. `AnyParserTest` now proves bare `any`, `foo.any`, `any.Foo`, `opt[foo.any]`, and a DTO literally named `any` all parse as ordinary `Simple`/`Constructor` refs.

### [PR-01-D02] `any.Foo` silently succeeds as `AnyRef(None, None)`, consuming only "any" and leaving `.Foo` dangling
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:42-48`
**Description:** `parseTypeRef("any.Foo")` returns `Success(AnyRef(None, None), 3)` — three chars consumed, rest dangling. In field-def context (`f: any.Foo`) the outer grammar then fails far away with a confusing message. Fail-fast violation — the parser must reject `any.Foo` cleanly at the typeRef level. The executor tested `foo.any` (prefix-left) but not `any.Foo` (suffix-right).
**Suggested fix:** Fixing D01 eliminates this path (the `any`-specific dispatch only fires when `any[` is seen). Add a regression test `expectFailure("any.Foo")`.

### [PR-01-D03] `opt[foo.any]` silently truncates to `Simple(opt, [])` because of `.filter` + `typeParams.?` conspiracy
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:50-64`
**Description:** Pre-PR, `parseTypeRef("opt[foo.any]")` → `Constructor(opt, [Simple(any, [foo])])`. Post-PR, inner `typeRef` fails on `foo.any` (filter rejects), so `typeParams` fails min=1, so the outer `typeParams.?` gives `None`, so the parser returns `Simple(opt, [])` with only 3 chars consumed. Fail-fast + back-compat regression.
**Root cause:** The `.filter` path + optional `typeParams.?` together silently succeed with partial input.
**Suggested fix:** D01 fix resolves this. Add a regression test for `opt[foo.any]` round-trip.

### [PR-01-D04] Scope creep — 4 translator files (graphql, openapi) modified beyond pure whitespace; violates CLAUDE.md §5 Surgical Changes
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/{graphql,openapi}/*.scala` (4 files)
**Description:** The executor ran `mdl :fmt` on the whole tree, which reflowed unrelated translator files. `git diff --cached -w` still shows structural token rearrangement in `GqlBaboonTranslator.scala` and `OasBaboonTranslator.scala` (lambda-body reflowing, not pure whitespace). `GqlTypeTranslator.scala` and `OasTypeTranslator.scala` are true whitespace-only under `-w`. Regardless: none of these files belong in a parser PR. Executor incorrectly described all four as "whitespace-only".
**Root cause:** Whole-tree formatter behavior. Running `mdl :fmt` in a dirty working tree picks up drift in unrelated files.
**Suggested fix:** `git checkout HEAD -- baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/graphql/ baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/`. If the project wants those files reformatted, that's a separate commit on main first, then rebase.

### [PR-01-D05] Round-trip property test only exercises canonical `render` output; no whitespace-variant coverage
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala:115-130`
**Description:** The round-trip test parses `s.render` back and compares; since `render` emits a canonical form, whitespace permutations inside brackets (no-space-after-comma, multi-space) are never tested. The grammar accepts them today but nothing guards against regression.
**Suggested fix:** Add two focused tests: `any[domain:this,SomeType]` (no space after comma) and `any[ domain:current , SomeType ]` (multi-space), both asserting the same `AnyRef(...)` shape.

### [PR-01-D06] `AnyRef` dropped `prefix` field the plan specified, without documenting the deviation
**Status:** resolved (plan updated, intentional design)
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawTypeRef.scala:23`
**Description:** The plan's §PR 1.1 files-to-touch specified `AnyRef(qualifier, underlying, prefix: List[RawTypeName])`. Executor dropped `prefix` silently. The drop is defensible — `any` is a global builtin with no prefix — but the decision should be surfaced so PR 1.2 plumbing aligns.
**Fix:** Updated `docs/drafts/20260424-1738-any-opaque-plan.md` §PR 1.1 files-to-touch to reflect the no-prefix `AnyRef` shape and document the rationale. No code change — the executor's shape is correct.

### [PR-01-D07] Missing negative test: `any[domain:this, SomeType, Extra]` (qualifier + 2 types)
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala`
**Description:** Suite covers `any[SomeType, SomeOther]` but not qualifier + multiple types. Legitimate user mistake.
**Suggested fix:** Add `expectFailure("any[domain:this, SomeType, Extra]")`.

### [PR-01-D08] Missing negative test: `any[domain:this,]` (trailing comma after qualifier)
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala`
**Description:** Suite covers `any[,T]` but not `any[domain:this,]` or `any[T,]` (trailing comma). Parser rejects these today; no regression guard.
**Suggested fix:** Add `expectFailure("any[domain:this,]")` and `expectFailure("any[T,]")`.

### [PR-01-D09] Duplicated `import fastparse.SingleLineWhitespace.whitespace` in adjacent methods
**Status:** resolved (accepted — matches existing file convention)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:32, 43`
**Description:** Matches existing file style (other methods do the same). Flag only; not blocking.
**Fix:** No action. Leaving intact to match existing file convention; refactoring would violate CLAUDE.md §5 Surgical Changes.

### [PR-01-D10] `expectFailure` helper accepts partial-consume successes as "failure", masking real defects like D02/D03
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala:30-42`
**Description:** The test helper treats a `Success` that consumed fewer chars than the input as "failure at the typeRef level". Too lenient — silent partial-consume passes a negative test that should demand a clean Failure.
**Suggested fix:** Make `expectFailure` accept only `Parsed.Failure`. For cases where partial consumption is the expected behavior, use a distinct helper and assert the exact consumed-prefix length.

### [PR-01-D11] Placeholder `RuntimeException` in `BaboonTranslator.convertTpe` rather than a domain-typed issue
**Status:** resolved (deferred to PR 1.2 — explicit placeholder per plan §Q5)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:468-472`
**Description:** Placeholder is pre-authorised (plan §Q5). File's other error emission uses `Error2`-based `Either`. PR 1.2 replaces this anyway.
**Fix:** No action in PR 1.1. PR 1.2 will replace with a proper typed issue as part of `TypeRef.Any` plumbing.

### [PR-01-D12] Design doc and plan don't reflect that bare `any` parses as `Simple("any", Nil)`, not `AnyRef(None, None)`
**Status:** resolved (docs updated)
**Severity:** minor
**Location:** `docs/drafts/20260424-1738-any-opaque-fields.md` (Typed AST section); `docs/drafts/20260424-1738-any-opaque-plan.md` §PR 1.1 line 33, 46; §PR 1.2 line 71
**Description:** D01's back-compat fix changed parser semantics so bare `any` parses as `Simple(RawTypeName("any"), Nil)`, NOT `AnyRef(None, None)`. The `AnyRef` node only appears when brackets follow. Plan §PR 1.1 still implies all six forms produce `AnyRef`; plan §PR 1.2 still says "translate `RawTypeRef.AnyRef` into `TypeRef.Any`" without noting that PR 1.2 must ALSO recognise `Simple("any", Nil)` as variant A. If not documented, PR 1.2 will either miss this case (resolving bare `any` as a missing user type) or re-introduce `any`-is-reserved at the typer, undoing the D01 win.
**Suggested fix:** Update plan §PR 1.1 to state explicitly "bare `any` (no brackets) parses as `Simple(RawTypeName(\"any\"), Nil)`; only `any[...]` forms produce `AnyRef`." Update plan §PR 1.2 to require recognising both `AnyRef` and `Simple(name=\"any\", prefix=Nil)` as variant-A source. Update design doc Typed AST section accordingly.

### [PR-01-D13] `AnyRef(None, None)` is unreachable at the parser level but still definable; `render` output is non-round-trippable
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawTypeRef.scala:12-24`
**Description:** `AnyRef(None, None).render == "any"` but re-parsing `"any"` yields `Simple(RawTypeName("any"), Nil)`, not `AnyRef(None, None)`. Round-trip is broken for this one construction. Since the parser no longer produces `AnyRef(None, None)`, the only way to construct it is by hand — a foot-gun for tests, pretty-printers, or AST mutation passes. CLAUDE.md §"Fail fast" / "Explicit over implicit" argue against silently-unreachable constructors with asymmetric render behavior.
**Suggested fix:** Add a `require(qualifier.isDefined || underlying.isDefined, "AnyRef(None, None) is unreachable at parse level; bare `any` should be Simple(\"any\", Nil)")` in `AnyRef`. Update the test's round-trip special-case accordingly (or remove it).

### [PR-01-D14] `expectFailure` at document level doesn't detect partial-consume-at-typeref; regression-prone
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala:37-46`
**Description:** Helper asserts the full DTO fixture fails, but a silent partial-consume at the typeref level (the exact D02/D03 failure mode) would ALSO cause document-level failure — for the wrong reason. The helper cannot distinguish "typeRef rejected cleanly" from "typeRef accepted partially, outer grammar hit dangling characters". A future regression reintroducing partial-consume would pass this test.
**Suggested fix:** Add a second helper `expectTypeRefFailure(input: String)` that invokes `parseTypeRef` directly and asserts `Parsed.Failure`. Apply it to the main fail-fast cases (those that would be caught by tight typeref-level failure): `any[domain:bogus]`, `any[]`, `any [T]` etc.

### [PR-01-D15] `any [T]` (whitespace before `[`) silently truncates to `Simple("any", Nil)` and fails at the document level; asymmetric with `Foo [T]`
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:51-60`
**Description:** `Foo [T]` (space between type name and `[`) parses cleanly as `Constructor(Foo, [T])` thanks to `SingleLineWhitespace.whitespace` in `typeRef`'s `~` sequence. But `any [T]` does NOT go through that `~` — the new dispatch uses `flatMap` / `|` which don't consume whitespace between the identifier and the `[`. Result: `any [T]` succeeds as `Simple("any", Nil)` with `[T]` dangling. The DTO-level parse ultimately fails, but far from the true error site. Same failure mode as D02/D03.
**Root cause:** `flatMap`-based dispatch doesn't inherit the whitespace-consuming behavior of `~`.
**Suggested fix:** Make the dispatch tolerate whitespace before `[`. Either (a) restructure `typeRef` so the dispatch passes through a `~` sequence that eats whitespace, or (b) inside `anyTypeRefArgs`, match optional whitespace before `[` explicitly via `CharsWhileIn(" \t", 0) ~ "["`. Verify with positive test `any [T]` (must produce same AST as `any[T]`) and regression guard.

### [PR-01-D16] Missing coverage: `any[foo.any]`, `any[any]`, `any[any[T]]` (prefixed underlying and nested `any`)
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyParserTest.scala`
**Description:** `opt[foo.any]` is tested but `any[foo.any]` isn't. Nested `any` (like `any[any]`, `any[any[T]]`) is untested. These exercise recursive dispatch and lock in semantics for PR 1.2.
**Suggested fix:** Add three positive tests with shape assertions: `any[foo.any]` → `AnyRef(None, Some(Simple("any", ["foo"])))`, `any[any]` → `AnyRef(None, Some(Simple("any", Nil)))`, `any[any[T]]` → `AnyRef(None, Some(AnyRef(None, Some(Simple("T", Nil)))))`.

### [PR-01-D17] `anyQualifier` relies on caller's whitespace scope; fragile — a future `~` inside its body would silently break `domain: this`-rejection
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:26` (around `anyQualifier`)
**Description:** `anyQualifier` is designed to forbid whitespace around `:`. It does so by not using `~` inside its body, relying on the absence of whitespace-implicit. But there's nothing in the code that enforces this at compile time — a future editor could add `~` inside and the negative test `reject any[domain: this]` would be the only guard. Fragile.
**Suggested fix:** Add `import fastparse.NoWhitespace.noWhitespaceImplicit` inside `anyQualifier` to make the contract explicit and fail-fast on edits. OR expand the in-code comment to a loud warning about what a regression would look like.

---

## PR-02 — Typed AST + typer + placeholder cascade

### [PR-02-D01] `processRefs` new `TypeRef.Any` arm is shallow; comment overstates what it does
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala:135-140`
**Description:** The arm walks `underlying.toList.flatMap(u => domain.defs.meta.nodes(u.id))`, which only reaches one structural level of the underlying type. The pre-existing `TypeRef.Constructor` arm has the same shallow pattern. Not broken, but the comment "surface any foreign refs it carries" overpromises.
**Fix:** Softened the comment to: "Surface the underlying's top-level ref if it's a user type. Structural deep-walking into the underlying's fields is left to future passes (matches the shallow behaviour of the `TypeRef.Constructor` arm above)." Code unchanged.

### [PR-02-D02] `isCompatibleChange` comment overstates the spec's evolution rule
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TypeInfo.scala:150-154`
**Description:** Comment claims "Per spec §Evolution: any variant change or underlying-type change is breaking." The spec only asserts breaking for mismatched pairs; silent about e.g. `any[T] → any` (drop underlying, widening). Returning `false` is safe for PR 1.2, but the comment would mislead PR 1.3.
**Fix:** Softened comment to flag PR 1.2 conservatism and PR 1.3's expected refinement. `false` return kept — still correct.

### [PR-02-D03] LSP `HoverProvider` and `BaboonSchemeRenderer` throw on `TypeRef.Any` — breaks interactive tooling for any model with `any` fields
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/HoverProvider.scala:179`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/scheme/BaboonSchemeRenderer.scala:472`
**Description:** LSP hover renderer and scheme JSON renderer both invoke `anyNotSupportedYet`, which throws. LSP runs continuously in the editor; scheme renderer is invoked whenever `:scheme` output is requested. Throwing here crashes/disables hover and scheme output for any domain containing `any` — before codec milestones complete. This is a user-facing hostile behavior different in kind from codec placeholders (which only fire on explicit codegen).
**Fix:** Both sites now render the DSL form verbatim (e.g. `any`, `any[domain:this]`, `any[domain:current, Inner]`) instead of throwing. `BaboonSchemeRenderer` produces free-form type text (not JSON) as part of reconstructing `.baboon` source — the DSL form fits the surrounding `renderTypeRef` output. Both surfaces now handle `TypeRef.Any` without crashing; existing `SchemeRoundtripTest` suite continues to pass.

### [PR-02-D04] Missing test + undocumented behavior: unprefixed `any` reference always resolves to builtin, shadowing any top-level user type/alias named `any`
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:430-432`; `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyTyperTest.scala`
**Description:** The new `case RawTypeRef.Simple(RawTypeName("any"), Nil)` arm intercepts before `scopeSupport.resolveAlias`. A top-level `data any {...}` or `alias any = ...` is silently shadowed when referenced as unprefixed `any`. The existing "user type named any" test covers only the nested/prefixed case. This is intentional but undocumented.
**Fix:** Expanded the in-code comment in `BaboonTranslator.convertTpe` to state the shadowing rule explicitly (including the back-compat note that `data any {}` remains definable but only reachable via prefix). Added a test in `AnyTyperTest` that defines a top-level `data any { x: i32 }` next to `root data Holder { f: any }` and asserts `Holder.f.tpe == TypeRef.Any(Global, None)`. The DSL does not currently support a top-level `alias any = ...` syntax, so only data-type shadowing is exercised (noted in the test comment).

### [PR-02-D05] `AnyVariant` is placed at top level of `Typedef.scala`; convention is to nest inside the companion of its parent
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala:121`
**Description:** `TypeRef` companion nests `Scalar`/`Constructor`/`Any`. `Typedef` companion nests `Dto`/`Enum`/`Adt`/etc. `AnyVariant` is used only by `TypeRef.Any` and should nest inside `object TypeRef`.
**Fix:** Moved `sealed trait AnyVariant` + `object AnyVariant { ... metaKindByte ... }` inside `object TypeRef`. Access point is now `TypeRef.AnyVariant.{Global,ThisDom,Current}`. Added `import ... .TypeRef.AnyVariant` in `BaboonEnquiries.scala`, `BaboonTranslator.scala`, and `AnyTyperTest.scala` (the three sites that reference `AnyVariant` unqualified). No other import changes were required.

### [PR-02-D06] `anyNotSupportedYet` helper placed inside `object TypeRef` alongside data constructors — reads oddly
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala:117-118`
**Description:** Utility lives next to `Scalar`/`Constructor`/`Any`. When PR 1.3+ removes placeholders, deleting the helper will require another touch to `Typedef.scala`.
**Fix:** Extracted to `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/AnyPlaceholder.scala`. Call sites updated from `TypeRef.anyNotSupportedYet(...)` to `AnyPlaceholder.notSupportedYet(...)` across the ~47 cascade files. Added explicit `AnyPlaceholder` import in `PyCodecFixtureTranslator.scala` (the only file with a non-wildcard typer-model import). All other files use wildcard imports and need no change.

### [PR-02-D07] Silent fallthrough to pre-existing `case _ =>` catch-alls produces unhelpful / misleading errors when `TypeRef.Any` appears at runtime
**Status:** resolved
**Severity:** minor
**Location:** Files with pre-existing `case _ =>` that weren't modified: e.g. `translator/rust/RsConversionTranslator.scala:169`, `translator/typescript/TsConversionTranslator.scala:168`, `translator/graphql/GqlTypeTranslator.scala:67` (produces garbage `BaboonUnknown_any`), `translator/openapi/OasTypeTranslator.scala:86`, `translator/csharp/CSTypeInfo.scala:112` (returns default values silently).
**Description:** The cascade policy assumed all sites were exhaustive matches that require a new arm. Sites with pre-existing `case _ =>` catch-alls don't need one to compile — but `TypeRef.Any` falls through into generic errors ("Unsupported target field type", garbage names like `BaboonUnknown_any`, or silent default returns) that don't mention `any` or the site. Defeats the purpose of the `anyNotSupportedYet(site)` helper's grep-able error messages.
**Fix:** Added preemptive `case _: TypeRef.Any => AnyPlaceholder.notSupportedYet(site)` arms above the pre-existing catch-alls in all five flagged files (all confirmed outside the PR 1.2 diff). Schema-only sites (`GqlTypeTranslator` `typeRefStr` and `typeRefIdent`; `OasTypeTranslator` `typeRefSchema`) use sensible placeholders instead of the placeholder throw — `"BaboonAny"` for GraphQL (both arms) and `{"type":"object","description":"any (baboon-any envelope)"}` for OpenAPI — because the schema renderers must keep working before M11/M12. Non-schema sites (`RsConversionTranslator.InitializeWithDefault`, `TsConversionTranslator.InitializeWithDefault`, `CSTypeInfo.isCSValueType`) use `AnyPlaceholder.notSupportedYet(site)` matching the rest of the cascade. Files touched: `RsConversionTranslator.scala`, `TsConversionTranslator.scala`, `GqlTypeTranslator.scala` (2 arms), `OasTypeTranslator.scala`, `CSTypeInfo.scala` — 5 files, 6 new arms.

### [PR-02-D08] Missing test: top-level `data any { ... }` + unprefixed reference (see D04)
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyTyperTest.scala`
**Description:** See D04; this is the test-side of the same gap.
**Fix:** Covered by D04's fix — added a test under `"user type named `any`" should` that exercises the shadowing rule.

### [PR-02-D09] Missing test: `any[<builtin>]` (e.g. `any[i32]`) and `any[<constructor>]` (e.g. `any[lst[Inner]]`)
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyTyperTest.scala:20-37`
**Description:** All D1/D2/D3 typed-variant tests use `any[UserDTO]`. The typer presumably handles `any[i32]` and `any[lst[...]]` fine, but without tests PR 1.3 has no regression guard on the typed AST shape.
**Fix:** Added `e: any[i32]` and `g: any[lst[Inner]]` to the `Outer` DTO fixture in `AnyTyperTest`, plus two assertions confirming `TypeRef.Any(Global, Some(TypeRef.Scalar(Builtins.i32)))` and `TypeRef.Any(Global, Some(TypeRef.Constructor(lst, [Scalar(Inner)])))`.

### [PR-02-D10] Comment in `BaboonEnquiries.wrap(TypeRef.Any)` overstates coupling to spec §Evolution
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala:350-358`
**Description:** Comment says `shallowId` "changes when either shifts (evolution-breaking changes per spec §Evolution)". But compatibility is handled by `TypeInfo.isCompatibleChange`, not `shallowId`. `shallowId` differentiation serves structural equality, not evolution enforcement.
**Fix:** Reworded comment to: "Inline variant + underlying into the shallow id so each `any` shape has a distinct structural identity (used for equality / diff reports; evolution compatibility is enforced separately by `TypeInfo.isCompatibleChange`)."

### [PR-02-D11] `BinReprLen.Unknown()` for `TypeRef.Any` is pessimistic; could use `Range(minFloor, None)`
**Status:** resolved (accepted — deliberate conservatism until wire format is locked)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala:411-414`
**Description:** The wire format has a non-zero floor; `Range(min, None)` would be more precise. But `Unknown()` is safely conservative.
**Fix:** No action in PR 1.2. Revisit when wire format encoding lands in PR 2.2.

### [PR-02-D12] Cascade arms in F-effect contexts use `throw` instead of `F.fail` — bypasses structured error reporting
**Status:** resolved (accepted — placeholders removed per-language in M2+)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSConversionTranslator.scala:239` and similar in dart/java/scala/swift conversion translators.
**Description:** Sites inside F-context (bifunctor error channel) normally use `F.fail(BaboonIssue.of(...))`. New `Any` arms use `throw`, which typechecks because `Nothing <: F[...]` but bypasses the effect system.
**Fix:** No action in PR 1.2. Placeholders will be replaced per-language in M2+ with real logic that uses `F.fail` where appropriate.

### [PR-02-D13] GraphQL schema references `scalar BaboonAny` but never declares it — malformed SDL for any model using `any`
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/graphql/GqlBaboonTranslator.scala:124-131` (`builtinCustomScalars` set) and `:264-273` (`collectScalarsFromRef`)
**Description:** D07's fix has `GqlTypeTranslator.typeRefStr`/`typeRefIdent` return `"BaboonAny"` for `TypeRef.Any`. But `GqlBaboonTranslator` — which collects custom scalars at schema-emission time — has `builtinCustomScalars` not containing `BaboonAny`, and `collectScalarsFromRef` has a silent `case _ =>` fallthrough that ignores `TypeRef.Any`. A model with `any` fields will produce a GraphQL schema referencing `scalar BaboonAny` without declaring it — invalid SDL.
**Suggested fix:** Add `"BaboonAny"` to `builtinCustomScalars` and add an explicit case in `collectScalarsFromRef` for `TypeRef.Any` that returns `Set("BaboonAny")` (plus recursion into `underlying` if present). Per user guidance ("schemas are demo-only"), this is preferred over the fail-fast alternative — produces a valid, usable schema.

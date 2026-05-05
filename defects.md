# Baboon — Defect Ledger

> **Predecessor ledgers (frozen, chained):**
> - `docs/archive/20260505-m31-close-ledgers/defects.md` — PR-29P.x .. PR-31.x defects (M29 generics, M30 docstrings, M31 upstream defects).
> - `docs/archive/20260503-bab-any-anyopaque-ledgers/defects.md` — PR-01..PR-28.x defects (`any`/AnyOpaque, identifiers, ADT inheritance, codecs, map-key encoding).
>
> Reuse historical entries when investigating regressions in those areas.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-33.1

### [PR-33.1-D01] `[]` rejection test passes for the wrong reason; in-test gloss is misleading
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInheritanceParserTest.scala:158-160`
**Description:** The negative test for `+ Foo[]` passes, but for a different reason than the test comment asserts. `parentDef` is `("+" ~ nonGenericTypeRef ~ typeParams.?)`. When `typeParams` fails on `[]` (`rep(min=1)`), the surrounding `.?` swallows the failure and `parentDef` succeeds with `(Foo, None)`, leaving `[]` unconsumed. The outer `dtoEnclosed` then fails because `]` cannot start a new `dtoMember`. So the rejection is real, but it is enforced by "leftover bracket inside DTO body cannot start a new dtoMember", not by the NEList min=1 contract. A future refactor of `dtoMember.rep` could regress the test silently.
**Fix:** Rewrote the existing test's comment to accurately state "empty-bracket form is not accepted by the DTO body grammar" (not the misleading min=1 gloss). Added a sibling micro-test "parentDef alone rejects + Foo[]" that calls `c.defDto.parentDef(_)` directly on `+ Foo[]` and asserts `args.isEmpty && idx < input.length` — making the min=1 contract observable at the `parentDef` level so it is now a tripwire under future `dtoMember.rep` refactors.

### [PR-33.1-D02] Mixed-operator test exercises only `+` arms; `^` and `-` not represented
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInheritanceParserTest.scala:123-153`
**Description:** The mixed-body test exercises three `+` arms (`+ Foo; + Bar[i32]; + Baz`). Plan §PR-33.1 considers this satisfied — but no test places `+`, `-`, and `^` arms (with and without `[…]`) in the same body. The grammar admits this, and a future regression in `dtoMember`'s alternation order (`P(parentDef) | P(unparentDef) | P(intersectionDef)`) would not be caught.
**Fix:** Appended new test `parse mixed + Foo[i32]; - Bar[str]; ^ Baz[i32]` that parses the three-operator body and asserts the three members come back as `RawDtoMember.ParentDef`/`UnparentDef`/`IntersectionDef`, each with `args = Some(NEList(...))`. Now exercises the alternation order across all three operators in a single body.

### [PR-33.1-D03] No test for nested template argument `+ Foo[Bar[i32]]`
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInheritanceParserTest.scala`
**Description:** Plan §PR-33.1 acceptance does not explicitly require a nested-arg test, but `typeParams` recurses through `typeRef`, and `+ Foo[Bar[i32]]` should parse to `ParentDef(Foo, Some([Constructor(Bar, [Simple(i32)], Nil)]))`. PR-33.2 lowering must handle this; a parser regression would be discovered late.
**Fix:** Appended new test `parse + Foo[Bar[i32]] as ParentDef with nested Constructor arg` asserting `args == Some(NEList(RawTypeRef.Constructor(RawTypeName("Bar"), NEList(Simple(i32, Nil)), Nil)))`. The recursion through `typeParams` → `typeRef` → `typeParams` is now exercised at the structural-arm position.

### [PR-33.1-D04] Cross-line whitespace binding: `[…]` on a new line silently binds as args, no anchoring
**Status:** resolved (option (b) — documented + tripwire test; semantic tightening deferred)
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:116-129`
**Description:** `parentDef`/`unparentDef`/`intersectionDef` compose `nonGenericTypeRef ~ typeParams.?` under `BaboonWhitespace`, which consumes newlines. Source like `data X { + Foo\n  [i32]\n }` silently binds `[i32]` as `Foo`'s args, even though the user wrote them on a separate line. Today this is harmless because no other DTO-member alternative starts with `[`, so there is no ambiguity. But it is the same silent-binding hazard PR-30.2-D01 addressed for `//!` doc-suffix capture: a future grammar extension that introduces any `[…]`-starting construct inside a DTO body will regress this composition silently.
**Fix:** Applied option (b). Appended cross-line tripwire test `parse + Foo with [i32] on the next line` that pins the current cross-line binding behaviour: any future tightening will fail the test and force a deliberate decision. Added a 4-line comment above `parentDef` in `DefDto.scala:116` flagging the BaboonWhitespace cross-line composition contingency. Semantic tightening (option (a)) deferred — out of scope for parser-only PR-33.1.

### [PR-33.1-D05] `TemplateInstantiator.substituteDtoMember` catch-all comment is stale; will mislead PR-33.2
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:384-386`
**Description:** The catch-all in `substituteDtoMember` matches `ParentDef`, `UnparentDef`, `IntersectionDef`, `ContractRef` and returns them unchanged with the comment "no RawTypeRef to walk." After PR-33.1, those three cases now carry `args: Option[NEList[RawTypeRef]]` — which IS a list of `RawTypeRef`s that any code recursing through a member's `RawTypeRef`s must walk. PR-33.1 is parser-only and PR-33.2 explicitly owns the lowering, so this is not a functional defect of PR-33.1. But the stale comment will mislead PR-33.2's author into thinking the catch-all is correct as-is.
**Fix:** Updated the comment at `TemplateInstantiator.scala:385` to flag that `ParentDef.args`/`UnparentDef.args`/`IntersectionDef.args` (introduced in PR-33.1) carry `RawTypeRef`s that PR-33.2 must substitute through. The catch-all body itself is unchanged — PR-33.1 is parser-only and the substitution belongs in PR-33.2.

### [PR-33.1-D06] PR-33.2 hand-off comment is local to TemplateInstantiator; two other sites silently drop `args`
**Status:** resolved (Approach A — single-source-of-truth)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:243-258`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala:223-228`
**Description:** PR-33.1 added `args` to `RawDtoMember.ParentDef`/`UnparentDef`/`IntersectionDef`. Two non-test sites destructure these cases by type pattern (`p: RawDtoMember.ParentDef`) and read only `p.parent`, silently dropping `args`: `BaboonTranslator.dtoParentToDefs` callers (parent/unparent/intersection arms) and `BaboonEnquiries.hardDepsOfRawDefn` (`Seq(d.parent)`). Today this is harmless — the typer rejects template references at parent positions outside instantiation — but PR-33.2's lowering must walk those `args` for hard-dep resolution and substitution. D05's fix flagged only the `TemplateInstantiator.substituteDtoMember` site; an executor of PR-33.2 reading just that comment may produce a partial implementation.
**Fix:** Approach A. Extended the catch-all comment at `TemplateInstantiator.scala:388-395` into a three-site index naming `BaboonTranslator.scala` parent/unparent/intersection arms and `BaboonEnquiries.hardDepsOfRawDefn` ParentDef/UnparentDef/IntersectionDef sites. Added pointer comments at the six drop-sites: `BaboonTranslator.scala:~244,~253,~259` and `BaboonEnquiries.scala:~224,~227,~230`. Each pointer comment names "PR-33.2" verbatim so a future grep finds all related sites. Comment-only; zero behaviour change.

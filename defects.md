# Baboon — Defect Ledger

> **Predecessor ledgers (frozen, chained):**
> - `docs/archive/20260505-m31-close-ledgers/defects.md` — PR-29P.x .. PR-31.x defects (M29 generics, M30 docstrings, M31 upstream defects).
> - `docs/archive/20260503-bab-any-anyopaque-ledgers/defects.md` — PR-01..PR-28.x defects (`any`/AnyOpaque, identifiers, ADT inheritance, codecs, map-key encoding).
>
> Reuse historical entries when investigating regressions in those areas.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-33.3

### [PR-33.3-D02] Row 4 fixture exercises matrix #1 (template-in-body) instead of plan-mandated matrix #2 (forbidden type-arg)
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala:251-271` (`m33bad4ForbiddenTypeArg` fixture + test); underlying gap at `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:342-499` (`lowerOneArm`)
**Description:** Plan §4 row 4 specifies "`+ MyGeneric[T]` where `T` is a forbidden type per existing template-arg rules" — i.e. the type ARGUMENT itself is forbidden (matrix #2: nested template instantiation in arg position). The executor's fixture instead places `Other[T]` in MyGen's body field (`data MyGen[T] { v: Other[T] }`). The fired diagnostic IS `TemplateInstantiationInForbiddenPosition`, but it fires from `substituteTypeRef`'s matrix #1 check at `TemplateInstantiator.scala:1006-1014` — the same code path that would fire for an alias-RHS instantiation. The structural-arm pre-validation `lowerOneArm` does NOT check whether `args` themselves are template constructors — there is no equivalent of the matrix-#2 `args.collectFirst { … forbiddenInnerTemplate … }` block found in `processMember` at lines 686-708. Concretely, `+ MyGen[OtherTemplate[i32]]` over a benign `data MyGen[T] { v: T }` would silently pass — the actual plan §4 row 4 case is NOT covered, AND there is a real underlying validation gap.
**Fix:** Confirmed real validation gap (failing-first test produced `UnexpectedNonBuiltin`, not `TemplateInstantiationInForbiddenPosition`). Replaced fixture with genuine matrix-#2 shape `data Other[T]; data MyGen[T] { v: T }; root data Receiver { + MyGen[Other[i32]] }`. Extended `lowerOneArm` at `TemplateInstantiator.scala:422-441` with a matrix-#2 walk mirroring `processMember`'s alias-RHS check at L686-708: walks each `arg` and rejects any `RawTypeRef.Constructor` whose head names a registered template, emitting `TyperIssue.TemplateInstantiationInForbiddenPosition`. Reuses existing TyperIssue case — no 3-site exhaustive-match update. Test asserts `containingTemplateName="MyGen", instantiatedName="Other"`.

### [PR-33.3-D03] Row 6 fixture is single-Pkg cross-namespace, not cross-package — name misleads
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala:273-291` (`m33bad6CrossPkgLike` and the test name "m33_bad_6: …cross-pkg.baboon")
**Description:** The fixture has a single `model m33.bad6` declaration with `OtherTemplate` declared at top level, then a `+ otherpkg.OtherTemplate[i32]` reference. Resolves to prefix-derived `Owner.Ns(["otherpkg"])` and misses the registry — but this is cross-namespace within ONE Pkg, NOT cross-package. The test name suggests "cross-pkg" coverage; future readers debugging an actual cross-Pkg regression will not find what the name advertises. Plan §4 row 6 wording was "cross-package template via `+`" with explicit reference to spec §6 item 11 (deferred).
**Fix:** Renamed val `m33bad6CrossPkgLike` → `m33bad6NamespacePrefixMiss`, test name no longer mentions "cross-pkg", docstring now says "namespace-prefix miss". No multi-Pkg fixture added (deferred per spec §6 item 11).

### [PR-33.3-D04] Plan §4 row 2 (`TemplateNotInstantiated` for `+ MyGen` no brackets) is uncovered
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala` (no test added for row 2)
**Description:** Plan §4 row 2 lists `+ MyGeneric` (no brackets, head IS a registered template) → `TyperIssue.TemplateNotInstantiated` as required negative-path coverage. The executor's per-row mapping (rows 1, 3, 4, 6, 9, 11, 12) silently omits row 2. Wired-up code path exists: `TemplateInstantiator.validateNoBareTemplateRefs` (PR-33.2 D05 fix) emits the diagnostic from line 188.
**Fix:** Added test arm `m33_bad_2_template_not_instantiated` in `M29ValidatorTest.scala`. Fixture: `data MyGen[T] { v: T }; root data X { + MyGen }`. Asserts `TyperIssue.TemplateNotInstantiated(templateName="MyGen", aliasName="X")`. Pins the diagnostic at validator-level entry; complements PR-33.2's M33StructuralTemplateInstantiationTest coverage of the same case (no duplication).

### [PR-33.3-D05] Plan §3.d-mandated positive `+ ns.NsTemplate[i32]` cross-namespace structural-arm test missing
**Status:** resolved
**Severity:** minor
**Location:** `M29ValidatorTest.scala` (no test added) — plan §3.d / §4 row 5
**Description:** Plan §3.d states: "PR-33.3's negative tests must include a positive `+ ns.NsTemplate[i32]` case to verify cross-ns instantiation works in structural-arm position". Without it, a future bug breaking cross-ns lookup in `lowerOneArm`'s prefix→Owner.Ns conversion (line 386-390) would slip through (negative tests cannot isolate it).
**Fix:** Added test arm `m33_ok_cross_ns_structural_arm_plus` in `M29ValidatorTest.scala`. Fixture: `ns foo { data NsT[T] { v: T } } root data Receiver { + foo.NsT[i32] }`. Asserts `outcome.isRight` AND `Receiver` carries `v: i32` (TypeRef.Scalar `i32`). Used the file's existing positive-test scaffolding pattern (`domain.defs.meta.nodes` traversal).

### [PR-33.3-D06] Plan §4 row 7 (template self-instantiation via structural arm) is uncovered
**Status:** resolved
**Severity:** minor
**Location:** `M29ValidatorTest.scala` (no test added) — plan §4 row 7
**Description:** Plan §4 row 7 specifies `template X[T] { data X { + X[T] } }` as the close cousin of M29 matrix #5 — self-instantiation through a structural arm. Per §3.f should fire `CircularInheritance` via the cycle-detection set. Row 9 covers MUTUAL recursion (two templates) but not direct self-recursion (one template, single arm) — different cycleSet code paths.
**Fix:** Added test arm `m33_bad_7_template_self_instantiation` in `M29ValidatorTest.scala`. Fixture: `data X[T] { + X[T] }; root type Y = X[i32]`. Asserts `TyperIssue.CircularInheritance` fires AND the matrix is non-empty AND mentions `X` (verifying the PR-33.2 D01 synthetic-edge fix).

### [PR-33.3-D07] [PR-33.3-D01] deferral framing implies broader work than required; `.distinct` is preexisting
**Status:** resolved
**Severity:** nit
**Location:** `defects.md` `## [PR-33.3-D01]` entry; underlying preexisting code at `BaboonTranslator.scala:314-316`
**Description:** [PR-33.3-D01]'s entry attributes the silent-dedup behaviour to `.distinct` at `BaboonTranslator.scala:316`. This `.distinct` predates M33 (it is old translator code), so the issue is preexisting — not a regression introduced by PR-33.x. The "Fix (suggested)" mentions "careful audit of callers" implying broader scope than is actually required: a small local fix is feasible (compute `convertedDuplicateNames = converted.groupBy(_.name).filter(_._2.size > 1)`, fail with `NonUniqueFields` if non-empty BEFORE the `.distinct` line, leaving ADT contract dedup untouched).
**Fix:** Updated the `[PR-33.3-D01]` entry: added "Preexisting; predates M33" note to Description; replaced Fix-suggested with the scoped `convertedDuplicateNames = converted.groupBy(_.name).filter(_._2.size > 1)` approach leaving `.distinct` intact; updated Test reference to the new test name.

### [PR-33.3-D08] m33_bad_11 test docstring says "DEFERRED" but the test runs and asserts a passing outcome
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala:649-660`
**Description:** Test name string starts with "DEFERRED [PR-33.3-D01]" but the test body is fully active — asserts `outcome.isRight` and runs in normal suite. Reader scanning suite output sees "DEFERRED" line that actually executes and pins the (defective) idempotent-dedup behaviour. If [PR-33.3-D01] is later fixed, this test will FAIL and a maintainer reading "DEFERRED" may mistakenly mark it ignored rather than updating the assertion.
**Fix:** Renamed test to `m33_bad_11_duplicate_arm_silently_deduplicated_REGRESSION_GUARD`. Dropped "DEFERRED" from the name; updated docstring to "regression guard pinning current (defective) idempotent-dedup behaviour; will need updating when [PR-33.3-D01] is resolved". Assertion (`outcome.isRight`) unchanged — still pins current behaviour.

### [PR-33.3-D09] Row 1 fixture conflates user type-name with diagnostic case-name
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala:225-233` (`m33bad1NotATemplate` fixture)
**Description:** The fixture declares `data NotATemplate { v: str }` and asserts `TyperIssue.NotATemplate`. User type and diagnostic case share the exact name. The assertion `issue.head == "NotATemplate"` is checking the field-value of a case named NotATemplate where "NotATemplate" is the head name. If a future regression fires NotATemplate for the WRONG reason (e.g. prefix walk fails entirely), the head-string assertion is uninformative due to the naming collision.
**Fix:** Renamed `data NotATemplate { v: str }` → `data PlainDto { v: str }` in the fixture; `+ NotATemplate[i32]` → `+ PlainDto[i32]`; assertion updated to `issue.head == "PlainDto"`. Diagnostic case `TyperIssue.NotATemplate` unchanged.

### [PR-33.3-D01] Duplicate template-arm `+ MyGen[T]; + MyGen[T]` is silently deduplicated rather than rejected
**Status:** [ ] open
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:314-316` (`convertDto` — `withoutRemoved` computation applies `.distinct` before the `toUniqueMap` uniqueness check)
**Description:** The plan §4 row 11 expected `NonUniqueFields` to fire when the same template is included twice with the same type args (e.g. `data X { + MyGen[i32]; + MyGen[i32] }`). At runtime, both arms inline `v: i32`; the `.distinct` call at `BaboonTranslator.scala:316` deduplicates identical `Field` instances before `toUniqueMap` runs — so only one `v: i32` survives and the DTO compiles without error. `NonUniqueFields` fires only when two fields share the same name but DIFFER in type (e.g. `v: i32` vs `v: str`). The idempotent-duplicate case is silently accepted. **Preexisting; predates M33** — the `.distinct` call at `BaboonTranslator.scala:316` is older translator code; M33 only exposed the case via inlined fields.
**Fix (suggested):** Compute `convertedDuplicateNames = converted.groupBy(_.name).filter(_._2.size > 1)` BEFORE the `.distinct` call; fail with `TyperIssue.NonUniqueFields` if non-empty. Leave the `.distinct` call (and the ADT contract dedup that depends on it) intact.
**Test:** `M29ValidatorTest.scala` row 11 (`m33_bad_11_duplicate_arm_silently_deduplicated_REGRESSION_GUARD`) pins the current (defective) idempotent-dedup behaviour. Update the assertion to `NonUniqueFields` when this defect is resolved.

---

## PR-33.2

### [PR-33.2-D01] Synthetic `CircularInheritance` from recursion guard prints with empty payload
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:267-273, 326-331`; printer at `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala:528-542`
**Description:** Both depth-limit and cycle-set `CircularInheritance` instances pass `done = Seq.empty` and `matrix = AdjacencyList(Map.empty)`. The printer matches `ToposortError.UnexpectedLoop(_, matrix)` and prints `matrix.links.map(...).niceList()`; on an empty matrix, `niceList()` produces an empty string. The user sees the literal text "Circular inheritance have been found:" with NO trailing context — no template name, no cycle path. Operationally indistinguishable from a bug.
**Fix:** `TemplateInstantiator.scala:295-302` — the synthetic `ToposortError.UnexpectedLoop` matrix now carries one edge `(receivingTypeId → templateTypeId)` constructed from `TypeId.User(pkg, Owner.Toplevel, TypeName(...))` at both the depth-limit and cycle-set sites. `niceList()` renders a meaningful `Holder => Self`-shaped diagnostic. Reused existing `CircularInheritance` per §3.f — no new TyperIssue case for the recursion guard. The existing self-ref test extended with assertions on the rendered matrix text.

### [PR-33.2-D02] `- Template[Args]` and `^ Template[Args]` silently drop non-FieldDef members from the substituted body
**Status:** resolved
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:411-422` (`convertLoweredArm` Minus / Caret cases)
**Description:** For `- Template[Args]`, the comment claims parity with `- ParentDto`, but `- ParentDto` resolves the parent and uses `dtoParentToDefs` → `defn.fields`, which is the parent's FINAL flattened field list (including transitively-inherited fields from the parent's own `+` chain). The inline-substituted form for `-` collects ONLY `case f: RawDtoMember.FieldDef => UnfieldDef(...)` and drops every other member. So if the template body contains `+ ConcreteBase` (a concrete parent ref with `args = None`), `ConcreteBase`'s contributed fields are NOT removed by `- MyTemplate[T]`. Equivalent semantic gap for `^ Template[Args]` (line 419-422). No test exercises a template whose body contains a concrete `+ Base` arm; the gap is invisible today.
**Fix:** Added new TyperIssue case `TemplateBodyNotFlatForRemoval(templateName, receivingName, kind: "minus"|"caret", offendingMemberKind, meta)` at `TyperIssue.scala:730-742` with printer. Validation helper `checkFlatOrFail` in `TemplateInstantiator.scala:444-475`; `convertLoweredArm` (`TemplateInstantiator.scala:415-441`) now fails for `-`/`^` arms whose substituted body contains any non-FieldDef member, instead of silently dropping them. `+` arm preserves the full member list (concrete `+ Base` arms continue to compose). M29 3-site exhaustive-match update bundled per playbook: `lsp/features/DiagnosticsProvider.scala:130-133`, `lsp/state/WorkspaceState.scala:163, 245-247`, `.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala:1545`. Regression tests: D02-positive (concrete `+ Base` preserved through `+ Template[T]`), D02-negative-minus, D02-negative-caret in `M33StructuralTemplateInstantiationTest.scala`.

### [PR-33.2-D03] Test "template absent from Domain.defs" assertion is weak (would pass under buggy lowering)
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInstantiationTest.scala:236-243, 253, 268, 285, 311-312`
**Description:** All "template not in Domain.defs" assertions are `!names.contains("MyGen")`. But `MyGen` (the template name) is NEVER added to `Domain.defs` regardless of PR-33.2 — `templateRegistryBuilder.build` extracts templates and removes them from the non-template member list before any `DomainMember` is produced (`BaboonTyper.scala:417-418`). Assertion passes trivially even for a buggy lowering that synthesises `MyGen_i32` as a real `DomainMember`.
**Fix:** Replaced 5 sites of `!names.contains(...)` with strict-set assertions reflecting the post-`@root`-pruning domain (e.g. `names == Set("Holder")`). Surfaced an unexpected behaviour: with strict counts, the initial `Set("Wide", "Narrow")` etc. failed because `Wide` is consumed by `+ Wide` (its fields inlined) and is no longer transitively reachable from any root. Assertions corrected to the actual post-pruning set.

### [PR-33.2-D04] Depth-limit branch of recursion guard is untested
**Status:** resolved (natural-fixture approach; constructor-param attempt incompatible with distage)
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:263-274`
**Description:** The `selfRefStructuralFixture` (`template Self[T] { + Self[T] }`) fires the cycle-set branch at depth=1 — its cycle key collides on the second iteration. The depth-limit branch (`depth >= structuralArmRecursionLimit`) is unreachable from any test. A non-cyclic linearly-deepening shape (e.g. chain of >32 templates each instantiating the next at a distinct argument that keeps the cycle key fresh) would exercise it. Dead-code-untested; refactor that breaks the depth check would not regress any test.
**Fix:** Added `depthLimitFixture` (`template Deep[T] { + Deep[lst[T]] }; type X = Deep[i32]`). Each recursion step deepens the argument by one `lst[…]` so the cycle key never repeats and termination must come from the depth-limit branch (depth=32). Did NOT add a constructor parameter to `Impl` — initial attempt produced 33 test failures because distage cannot fill an unbound `Int` from a Scala default; reverted to the hard-coded `structuralArmRecursionLimit` and a self-deepening fixture. Documented the surprise in the surprises section of this PR's Completed entry.

### [PR-33.2-D05] `+ MyGen` (bare template name, no brackets) in DTO body produces a confusing diagnostic
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:222-241` (`lowerStructuralArmsInMembers`)
**Description:** When a user writes `+ MyGen` (no brackets) where MyGen IS a registered template, parsing produces `RawDtoMember.ParentDef(parent=ScopedRef(MyGen), args=None)`. `lowerStructuralArmsInMembers`'s pattern guards on `args.isDefined` so the bare-name case falls through unchanged. The non-template scope tree does not contain MyGen (extracted to the registry), so resolution fails with a generic ref-resolution error. User sees a confusing diagnostic when the right answer is "you forgot the type arguments".
**Fix:** Added `validateNoBareTemplateRefs` to the `TemplateInstantiator` trait and called it from `BaboonTyper.scala:419-425` BEFORE the toposort. The validator walks every DTO/Identifier/Contract/ADT-branch body and emits `TemplateNotInstantiated` for any `+/-/^ Foo` arm whose head names a registered template AND `args = None`. Architectural insight (surprise): cannot be fixed inside `lowerStructuralArmsInMembers` because that pass runs AFTER `BaboonTyper.order`, but `hardDepsOfRawDefn` already reports bare `+ MyGen` as a hard dep, so `resolveScopedRef` would fail with `NameNotFound` before lowering even sees the member list. New test cases (3) cover `+ MyGen`, `- MyGen`, `^ MyGen` bare-name forms.

### [PR-33.2-D06] `IntersectionFields` carrier loses per-field meta (uses arm-site armMeta for every field)
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:419-422` (`convertLoweredArm` Caret); `BaboonTranslator.scala:272-276`
**Description:** `IntersectionFields(rawFields, armMeta)` takes a single `meta = armMeta`. The translator maps each `rf` via `dtoFieldToDefs(rf, f.meta)` — passing the SAME `armMeta` to every field. Per-field docs/positions captured during template-body parsing (each `FieldDef` carried its own `RawNodeMeta`) are discarded. For diagnostics that anchor at the field's source position, every intersected field now points at the `^` operator's position, not the original field's declaration site in the template body. For `+` and `-` arms this is fine. The asymmetry is unique to `^`.
**Fix:** Changed `RawDtoMember.IntersectionFields(fields: Seq[RawField], …)` to `IntersectionFields(fields: Seq[RawDtoMember.FieldDef], …)` at `RawDtoMember.scala:38`. `convertLoweredArm` (Caret) collects FieldDef instances directly. `BaboonTranslator.scala:272-277` iterates `(fd.field, fd.meta)` so per-field meta is preserved. Verified `BaboonFamilyManager`'s `IntersectionFields(_, meta)` pattern (binds fields as `_`) still works — the change is in the type of the inner sequence, not the case-class arity.

### [PR-33.2-D07] tasks.md edit included in PR-33.2 working tree
**Status:** resolved (note-only; no functional change)
**Severity:** nit
**Location:** `tasks.md` (1 line — flipping PR-33.2 from `[ ]` to `[~]`)
**Description:** `tasks.md` was modified as part of starting PR-33.2 (per loop discipline — flip task to `[~]`). Per CLAUDE.md "Surgical Changes" guideline, every changed line should trace directly to the user's request. Tracking-file updates are acceptable — they are the loop's audit trail. Not a defect of the implementation itself.
**Fix:** Note in commit message that tasks.md is the M33 ledger, not a PR-33.2 source change. No code change required.

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

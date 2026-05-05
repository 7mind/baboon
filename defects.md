# Baboon — Defect Ledger

> **Predecessor ledgers (frozen, chained):**
> - `docs/archive/20260505-m31-close-ledgers/defects.md` — PR-29P.x .. PR-31.x defects (M29 generics, M30 docstrings, M31 upstream defects).
> - `docs/archive/20260503-bab-any-anyopaque-ledgers/defects.md` — PR-01..PR-28.x defects (`any`/AnyOpaque, identifiers, ADT inheritance, codecs, map-key encoding).
>
> Reuse historical entries when investigating regressions in those areas.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-33.7

### [PR-33.7-D03] Submodule-chain script's precondition allowlist hard-codes orchestrator-private files
**Status:** resolved
**Severity:** minor
**Location:** `scripts/m33-submodule-chain-commit.sh:66`
**Description:** The precondition check excludes `^ M editors/baboon-zed$` and `^?? proposal.md$` from the parent repo's expected dirty state. `proposal.md` is orchestrator-private (not in the project's git or `.gitignore`); another developer running this script encounters a hard failure with no carve-out for their own untracked files. The script should require a clean parent tree (modulo the submodule pointer) and let users stash/commit other state themselves.
**Fix:** `scripts/m33-submodule-chain-commit.sh` — dropped `^?? proposal.md$` carve-out; precondition now expects ONLY the submodule pointer (`^ M editors/baboon-zed$`); added opt-in `M33_CHAIN_ALLOW_DIRTY=path1,path2,…` env var that extends the allowlist per comma-separated path. Header comment updated to document the requirement and the opt-in env var.

### [PR-33.7-D04] `checkNoDuplicateConvertedFields` silently broadens `NonUniqueFields` to contract-diamond cases
**Status:** resolved (PR-33.8 — broad check reverted entirely; CI red on `pkg03.baboon` confirmed the broadening was incompatible with project's existing dedup behavior)
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:223-237` (new method) + invocation in `convertDto`
**Description:** The new pre-`.distinct` check operates on `converted`, which (`BaboonTranslator.scala:249-256`) absorbs ContractRef-flattened fields too. A DTO with `+ ContractA + ContractB` where both contracts share a field name will now fire `NonUniqueFields` where `.distinct` at L341 previously absorbed the diamond silently. 61/61 typer + 200/200 cross-language acceptance suggests no existing fixture exercises this, but the behavior change is wider than "duplicate template arm" — the original D01 was specifically about template-arm dedup. The broadening is arguably an improvement (silent diamond dedup masks user errors) but undocumented and unverified.
**Fix:** PR-33.8 hot-fix. Reverted `checkNoDuplicateConvertedFields` and its invocation in `BaboonTranslator.scala`. CI surfaced the predicted regression on `pkg03.baboon` line 153/162 — `testpkg.pkg0/[testpkg.pkg0/:#T4_A1]#B1` legitimately uses contract diamonds producing two `f2: #i32` that `.distinct` was silently absorbing. The broadening was incompatible with established project semantics; reopened `[PR-33.3-D01]` with the provenance-aware narrowing path documented as the future fix.

### [PR-33.6-D01] Close-log + tasks.md falsely claim the inner-submodule corpus file is "staged but not committed"
**Status:** resolved
**Severity:** minor
**Location:** `tasks.md:67` (PR-33.6 entry); `docs/logs/20260505-2005-m33-close-log.md:215-216`
**Description:** `git -C editors/baboon-zed/grammars/baboon status` shows `test/corpus/m33-template-arms.txt` under "Untracked files" — NOT staged. The outer `editors/baboon-zed` repo also has no staged changes (only `grammars/baboon (untracked content)` reported as modified). Both write-ups are factually wrong; an orchestrator following them would expect pre-staged content.
**Fix:** Reworded `tasks.md:67` and `docs/logs/20260505-2005-m33-close-log.md:215-216` to "Files present in the inner submodule's working tree as untracked entries; the orchestrator stages and commits them at close-out." Verified `git -C editors/baboon-zed/grammars/baboon status` shows the corpus file under "Untracked files" — matches the corrected wording.

### [PR-33.6-D02] M33 closed in tasks.md but `mdl :build :test` close-out gate was deferred
**Status:** resolved
**Severity:** minor (process)
**Location:** `docs/logs/20260505-2005-m33-close-log.md:284-291` (verification matrix rows 4 + 6)
**Description:** Both row 4 (`sbt baboonJVM/test`) and row 6 (`mdl --simple-log :build :test`) marked "deferred / not run in this PR's executor pass". CLAUDE.md is explicit: "Run mdl with the appropriate target before every commit and push: `mdl :build :test`". Closing M33 without running the documented pre-commit gate is a process violation. Combined with D01, the close posture is uncomfortably thin.
**Fix:** Ran `mdl --simple-log :build :test` (parallel) — failed on documented Kotlin daemon OOM (CLAUDE.md "<16GB RAM use --seq" workaround). Re-ran `mdl --simple-log --seq :build :test` — green. Total wall time 2132.3s (~36 min); all actions completed successfully ("Execution completed successfully", `success: true`). M33's `[x]` flip is now confirmed by the gate. Verification matrix row 6 backfilled in the close log.

### [PR-33.6-D03] Hover-test type-param assertion is vacuously true (`markdown.contains("T")` matches "Template")
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/lsp/features/LspFeaturesTest.scala:514`
**Description:** Test asserts `markdown.contains("T")` with comment "Hover should list type-param 'T'". Since the rendered markdown always contains "Template" (asserted by previous line), `contains("T")` cannot fail — asserts nothing about the type-param's actual presence. A regression dropping `[T]` from `renderTemplateInfo`'s output would not break this test.
**Fix:** `LspFeaturesTest.scala:516` — replaced `markdown.contains("T")` with `markdown.contains("[T]")`. Now pins the type-param-list rendering; would catch a regression that drops `[T]` from `renderTemplateInfo`'s output.

### [PR-33.6-D04] LSP-test docstring lies about fixture line numbering and DTO shape
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/lsp/features/LspFeaturesTest.scala:491-493`
**Description:** Comment claims "15: data MyGen[T] { v: T }" and "17: data Holder { + MyGen[i32] }". Actual fixture has `data MyGen[T] { v: T }` on (1-indexed) line 10 and the `+ MyGen[i32]` arm on line 13 — split across three lines, not the single-line `data Holder { + MyGen[i32] }` form. Test logic uses `lines.indexWhere` so it's correct; only the comment is wrong.
**Fix:** `LspFeaturesTest.scala:491-493` — comment corrected to actual 1-indexed layout (line 10 for `data MyGen[T] { v: T }`, lines 12-14 for the multi-line `root data Holder { …\n + MyGen[i32]\n}` shape).

### [PR-33.6-D05] Spec §9.1 claims legacy non-templated arms "behave exactly as before" — false for `+ MyGen` (head IS a template)
**Status:** resolved
**Severity:** minor
**Location:** `docs/spec/generics.md:837-838` (§9.1 closing sentence)
**Description:** §9.1 says legacy non-templated forms (`+ Page`, `- Stats`, `^ Page`) "continue to parse and behave exactly as before — the new clause is opt-in". This is FALSE when the head names a registered template: §9.6 ("No bare-template heads") and `validateNoBareTemplateRefs` actively reject `+ MyGen` with `TemplateNotInstantiated`. M33 is a behavioural change for the bare-template-head case, not pure opt-in. §9.1 contradicts §9.6.
**Fix:** `docs/spec/generics.md` §9.1 — appended qualifier "provided the head is not itself a registered template (see §9.6 'No bare-template heads')" to the closing sentence. Resolves the contradiction with §9.6.

### [PR-33.6-D06] Spec §2.6 supersession note is grammatically tangled
**Status:** resolved
**Severity:** nit
**Location:** `docs/spec/generics.md:320-324` (M33-update blockquote in §2.6)
**Description:** "`adt`-arm instantiation remains scoped only to *DTO/contract* bodies, not ADT bodies" — paradoxical at the surface ("adt-arm instantiation scoped to DTO/contract bodies" reads contradictorily). Intent: "template instantiation in structural-composition arms remains scoped to DTO/contract bodies; ADT inheritance arms are not widened in M33".
**Fix:** `docs/spec/generics.md` §2.6 — replaced the tangled "`adt`-arm instantiation remains scoped only to *DTO/contract* bodies, not ADT bodies" with: "Template instantiation in structural-composition arms (`+`/`-`/`^`) is widened only inside `data` and `contract` bodies; ADT inheritance arms remain restricted as in §2.6 above (see §9.6)."

### [PR-33.6-D07] Spec §9.7 attributes "Cycle / recursive-substitution guard" entirely to PR-33.4; bulk landed in PR-33.2
**Status:** resolved
**Severity:** nit
**Location:** `docs/spec/generics.md:976-977` (§9.7 PR-33.4 row)
**Description:** tasks.md's PR-33.4 entry: "Bulk of the planned work landed in PR-33.2 (recursion guard, cycle-set, depth-limit, self/mutual recursion tests) and PR-33.3 (negative-path pins). Residual: closed the empty-intersection edge case." Spec §9.7 collapses this: "PR-33.4 — Cycle / recursive-substitution guard; empty-body sentinel for `^`; cycle-key canonicalisation hardening." Overstates PR-33.4, understates PR-33.2.
**Fix:** `docs/spec/generics.md` §9.7 — split the single PR-33.4 row into two: PR-33.2 owns recursion-depth limit + cycle-set guard + self/mutual-recursion tests; PR-33.4 owns empty-`^`-body sentinel + cycle-key canonicalisation (`render` not `toString`) + regression-guard pins for `+ Empty[i32]` / `- Empty[i32]`. Attribution now matches `tasks.md`'s Completed entries.

### [PR-33.6-D08] Pre-existing stale hover string survives M33 close-out — `renderTemplateInfo` advertises only the alias instantiation form
**Status:** resolved (note-only — pre-existing, deferred)
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/HoverProvider.scala:165`
**Description:** Hover output reads `*Template — instantiate via `type Alias = $canonicalName[…]`*`. With M33 landed, `+ MyGen[i32]` / `- MyGen[i32]` / `^ MyGen[i32]` are also valid instantiation sites. PR-33.6 added an LSP smoke test exercising hover on `MyGen` inside a `+ MyGen[i32]` arm — but kept the rendered text suggesting only the alias form. Pre-existing (PR-29.8 vintage).
**Fix:** Note-only — pre-existing PR-29.8-vintage wording. M33 close-out scope is documentation + tests, not LSP UX upgrades. Deferred as a future LSP-polish PR.

---

## PR-33.5

### [PR-33.5-D01] m33-ok exercises only the `+` arm; `-` and `^` template-arm lowering paths are codegen-unverified at byte level
**Status:** resolved
**Severity:** minor (cross-language gate coverage gap)
**Location:** `test/conv-test/m33.baboon:36-39`
**Description:** Plan §5 explicitly recommended `data PageOnly { + Page[str]; - Stats[str] }` (`-` operator) and a `^` intersection example. The shipped fixture only contains `+ Page[i32]; + Stats[i32]` — both arms are `+`. In `TemplateInstantiator.scala:540-595`, ArmKind.Plus / Minus / Caret produce *different* intermediate `RawDtoMember` shapes (`FieldDef`, `UnfieldDef`, `IntersectionFields`) that are differently consumed by `BaboonTranslator.convertDto`. The cross-language byte-canonical gate, which the plan claims pins "the codegen surface", thus only covers the `+` path. A regression in the `-`/`^` lowering reduction in `convertDto` could ship undetected.
**Fix:** Extended both `test/conv-test/m33.baboon` and `baboon-compiler/src/test/resources/baboon/m33-ok/m33.baboon` with two additional consumer DTOs: `PageMinusStats { + Page[i32]; + Stats[i32]; - Stats[i32] }` (lowered shape `items, total`) and `PageOnly { + Page[i32]; + Stats[i32]; ^ Page[i32] }` (lowered shape `items, total`). Both added to `M33OkHolder` root. All 10 `CompatMain.*` updated with `CreateM33OkSample` (+ per-language equivalents) producing the new fields. `:test-acceptance` re-run: 200/200 passed; both `-` and `^` paths now exercised across all 10 backends and both wire formats.

### [PR-33.5-D02] Sample-value collision — `total = nObservations = 3 = items.length` masks swapped-field bugs
**Status:** resolved
**Severity:** minor
**Location:** every `CompatMain.*` (e.g. `test/conv-test-cs/ConvTest/CompatMain.cs:382-385`)
**Description:** Sample data is `items=[10,20,30], total=3, sum=60, nObservations=3` across all 10 backends. Both u32 fields (total, nObservations) carry the literal value 3, and items has 3 elements. A codegen defect that swapped the encoder positions of `total` and `nObservations`, or that read `items.size()` instead of the literal `total` field, would round-trip cleanly because the values are pairwise indistinguishable.
**Fix:** All 10 `CompatMain.*` updated with `pageWithStats: items=[10,20,30] (len 3), total=42, sum=60, nObservations=7`. Five distinct values (3, 5 from items.length+sum coincidence is benign; 42, 60, 7 + length 3 are pairwise distinct). New consumers got their own distinct values: `pageMinusStats: items=[100,200], total=99`; `pageOnlyIntersect: items=[1..5], total=5`. Cross-language byte-canonical agreement preserved (md5 `f340dd76…` across 9 backends; Swift diverges by sorted-key shape, consistent with m29 history).

### [PR-33.5-D03] No assertion that decoded value matches the canonical sample (round-trip-only check)
**Status:** resolved (note-only — project-wide convention, not introduced by PR-33.5)
**Severity:** nit
**Location:** every `readAndVerifyM33Ok` across 10 backends
**Description:** Each backend's `readAndVerifyM33Ok` decodes the blob, re-encodes, re-decodes, and compares `data == reDecoded` — a self-consistency check. There is no `assertEquals(data, expectedSample)` against the canonical sample. A backend that decodes any well-formed m33-ok blob into `M33OkHolder(IntPageWithStats([], 0, 0, 0))` and re-encodes into the same shape would PASS the round-trip but mask data loss. m29's path is identical; this is a project-wide pattern.
**Fix:** Note-only. PR-33.5 follows the existing m29 convention. A project-wide upgrade to value-content assertions is out of scope for this PR; documented as residual risk.

### [PR-33.5-D04] No mixed-arm-kind, nested template-arg, or cross-namespace coverage in the conv-test fixture
**Status:** resolved (note-only — typer-only coverage in M29ValidatorTest is the documented home)
**Severity:** minor
**Location:** `test/conv-test/m33.baboon`
**Description:** Plan §5 / §3.c / §3.d called out: mixed `+ ConcreteRef; + Template[Args]` arms; nested template-arg `+ Foo[Bar[i32]]`; cross-namespace structural-arm template (`+ ns.NsT[i32]`). None appear in the conv-test fixture. Cross-namespace IS covered by `m33ok5CrossNsStructuralArmPlus` in `M29ValidatorTest.scala` (typer-only), and nested template-arg by `M33StructuralTemplateInheritanceParserTest`. The risk that codegen-side handling of namespace-prefixed identity formation regresses without surfacing in cross-language byte gate is low — codegen sees a flat post-lowered DTO regardless of source-side prefixes.
**Fix:** Note-only. Cross-language coverage is restricted to `+ Template[Concrete]` at top-level; cross-ns / nested / mixed shapes are covered by typer-only tests. Documented in the PR's Completed entry.

### [PR-33.5-D05] No `bit`-typed, foreign-typed, ADT-typed, or `str`-typed concrete instantiation
**Status:** resolved (note-only — m29 covers str/non-template-arg variety; m33 is `+`-arm-shape canonical)
**Severity:** nit
**Location:** `test/conv-test/m33.baboon`
**Description:** Both templates instantiate at `i32` only. Different concrete type categories (`bit`, `str`, `f64`, foreign, ADT, enum) routed through `T` are not exercised. m29 already covers `IntPage`/`StrPage`/`ItemPage` with non-`i32` args; m33's role is to pin the structural-arm operator surface, not re-cover M29's substitution variety.
**Fix:** Note-only. Different concrete arg types are M29's coverage; M33 focuses on the new structural-arm operator paths.

### [PR-33.5-D06] Plan §5 enumerated four shapes; executor shipped one (scope-shrink note)
**Status:** resolved (cascaded from D01 fix)
**Severity:** minor (process)
**Location:** `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md` §5 vs `test/conv-test/m33.baboon`
**Description:** Plan §5 explicitly listed: (1) `+ Page[i32] + Stats[i32]`, (2) `-` example, (3) `^` intersection, (4) holder. Executor shipped only (1) and (4). Coverage component of the cross-language gate (everything the plan said the fixture should exercise) was reduced.
**Fix:** Resolved when D01's fixture extension landed. All four shapes now present.

---

## PR-33.4

### [PR-33.4-D02] Empty-body test asserts diagnostic class only; does not pin sentinel `kind="caret"` / `offendingMemberKind="empty body"`
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInstantiationTest.scala:594-604`
**Description:** The new `^ Empty[i32]` test asserts only `assertProducesTyperIssue[TyperIssue.TemplateBodyNotFlatForRemoval](outcome)` — class-only. It does not check `kind == "caret"` nor `offendingMemberKind == "empty body"`. The whole point of the PR-33.4 fix is the new sentinel-driven branch. The assertion is satisfied by ANY `TemplateBodyNotFlatForRemoval` surfacing through this fixture — including a hypothetical regression that fires the diagnostic for the wrong arm.
**Fix:** `M33StructuralTemplateInstantiationTest.scala:628-649` — extracts `TemplateBodyNotFlatForRemoval` issues from `outcome` (mirrors `selfRefStructural` pattern at L517-525) and asserts `kind == "caret"` AND `offendingMemberKind == "empty body"`. Class-level coverage retained.

### [PR-33.4-D03] Printer message for empty-body sentinel is paradoxical: "contains a non-field member (empty body)"
**Status:** resolved
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala:738-748` (printer); sentinel produced at `TemplateInstantiator.scala:568`
**Description:** The printer template reads "the substituted body contains a non-field member (${issue.offendingMemberKind})". For preexisting call-sites `offendingMemberKind` is a `getClass.getSimpleName` (e.g. `"ParentDef"`) and the sentence reads correctly. PR-33.4 introduces sentinel `"empty body"`, making the rendered message "the substituted body contains a non-field member (empty body)" — paradoxical (an empty body contains no member of any kind). The in-code comment at `TemplateInstantiator.scala:558` claiming the message is "informative enough" is wrong on inspection of actual printer output.
**Fix:** `TyperIssue.scala:746-751` — printer for `TemplateBodyNotFlatForRemoval` now branches on the `"empty body"` sentinel, emitting "the substituted body is empty (intersection over an empty field set would be a silent no-op; caught at lowering time…)" instead of the paradoxical "contains a non-field member (empty body)". `TemplateInstantiator.scala:553-558` — comment retracted; replaced with a pointer to the sentinel-aware printer branch.

### [PR-33.4-D04] Sentinel `"empty body"` violates the documented `offendingMemberKind` contract
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala:269-277` (case-class scaladoc); `TemplateInstantiator.scala:568`
**Description:** The case-class scaladoc defines `offendingMemberKind` as "the simple-class-name of the offending RawDtoMember subtype". The new use injects `"empty body"` — NOT a simple-class-name. Contract is silently broken at one call-site; the in-code comment at `TemplateInstantiator.scala:556-558` is the only place describing the convention break.
**Fix:** `TyperIssue.scala:269` — scaladoc on `offendingMemberKind` now reads "...the simple-class-name of the offending RawDtoMember subtype, OR the sentinel `\"empty body\"` when the substituted body is empty (PR-33.4)".

### [PR-33.4-D05] No negative-control test for `+ Empty[i32]` and no regression-pin for `- Empty[i32]` (deferred D01)
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInstantiationTest.scala`
**Description:** The PR adds one positive-fail test for `^ Empty[i32]`. Without a sibling positive-pass `+ Empty[i32]` (must compile, idempotent no-op) and a regression-pin for `- Empty[i32]` (currently silent no-op, deferred per [PR-33.4-D01]), nothing in the suite distinguishes between the diagnostic firing for the right operator versus a future refactor that accidentally swaps outcomes. Per the PR-33.3-D08 lesson: pin even defective behaviour as a regression guard.
**Fix:** `M33StructuralTemplateInstantiationTest.scala` — added `emptyBodyPlusFixture` (`Receiver + Empty[i32]` → zero fields) and `emptyBodyMinusFixture` (`Receiver + Foo[i32] - Empty[i32]` → `v: i32` survives), each with its own test arm. `- Empty[i32]` test carries the regression-guard comment naming `[PR-33.4-D01]`.

### [PR-33.4-D06] Cycle-key uses `RawTypeRef.toString` rather than the explicit `render` canonical form
**Status:** resolved
**Severity:** nit
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:449`
**Description:** The cycle-key tuple uses `argList.map(_.toString).mkString(",")`. `RawTypeRef` declares an explicit `render: String` method (`RawTypeRef.scala:6-17`) intended as the canonical display form. Auto-derived case-class toString is structurally equivalent for the current shape, but it is implicit equivalence rather than declared. If a non-canonical field (e.g. a parse-time hint) is later added to `RawTypeRef.{Simple,Constructor,AnyRef}`, auto-toString would silently bake it into cycle-keys, while `render` would not. Audit conclusion was correct *now* but fragile.
**Fix:** `TemplateInstantiator.scala:449` — `argList.map(_.toString).mkString(",")` changed to `argList.map(_.render).mkString(",")`. Same behaviour today; canonicalisation contract now explicit.

### [PR-33.4-D01] Empty template body under `-` is a silent no-op (analogous to the `^` gap fixed in this PR)
**Status:** [ ] open
**Severity:** minor
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala` (`convertLoweredArm` Minus case); `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:311-316` (`removedSet` / `withoutRemoved`)
**Description:** PR-33.4 fixed the empty-body-under-`^` gap (empty `IntersectionFields` → `if (intersectionSet.isEmpty)` short-circuit was a no-op). The analogous `^`-fix was: fail with `TemplateBodyNotFlatForRemoval(offendingMemberKind="empty body")` at lowering time. The `-` operator with an empty body is semantically different: removing an empty set of fields is idempotent (no fields are removed). Whether this should be a hard error or a silent no-op is a product decision. Current behaviour: silently accepted. This is not a silent corruption (unlike the `^` case which masked a "pass-through" masquerading as "intersection"), but it may indicate a user mistake. Deferred for a future product decision.
**Fix (suggested):** Either (a) emit a warning/error diagnostic at lowering time when `fieldDefs.isEmpty` in the `Minus` arm of `convertLoweredArm`, or (b) leave as a silent no-op with a code comment. Option (b) is correct for now (removing nothing is correct semantics). If a warning facility is added in a future milestone, this can be upgraded.

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
**Fix (suggested):** PR-33.7 attempted the broad pre-`.distinct` `groupBy(_.name)` approach. **CI failed** (`pkg03.baboon` line 153/162: `testpkg.pkg0/[testpkg.pkg0/:#T4_A1]#B1` legitimately produces two `f2: #i32` via a contract diamond). The broad check was reverted in PR-33.8 and `[PR-33.3-D01]` reopened. Future fix needs **provenance-aware narrowing**: detect duplicates only when produced by template-arm inline expansion (M33), not by contract-diamond inheritance. This requires threading provenance metadata through the conversion pipeline — non-trivial. Possible approach: track per-Field origin (which `RawDtoMember` produced it) and fire `NonUniqueFields` only when ≥2 origin entries are `RawDtoMember.ParentDef` with `args.isDefined`.
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

# M33 — Generic structural inheritance (extends BAB-A04 / M29)

**Date seeded:** 2026-05-05 15:00
**Predecessor plan:** `docs/drafts/20260503-2210-m29-generics-plan.md` (M29 / BAB-A04, closed 2026-05-04 — see `docs/logs/20260504-0040-m29-close-log.md`).
**Predecessor close-out drain:** `docs/logs/20260504-0527-m29-deferred-drain-log.md` (PR-29.12 .. 29.16, including PR-29.15 cross-namespace alias-RHS hardening).

---

## 1. Milestone summary

**Scope.** Allow a structural-inheritance arm in a `data` / `contract` body to be a template instantiation (e.g. `+ MyGeneric[i32]`, `- MyGeneric[i32]`, `^ MyGeneric[i32]`). Today the parser only accepts a `ScopedRef` (a name path) for the three operators (`DefDto.scala:116-129`); M33 widens the grammar and the typer pipeline so the head can be `Template[T1, …, Tn]`. The instantiation goes through the same monomorphisation machinery that M29 uses for alias-RHS instantiation (`TemplateInstantiator.scala`); the resulting concrete shape is then handed to the existing structural-composition pipeline (`BaboonTranslator.dtoParentToDefs` at `BaboonTranslator.scala:173-190` for `+`, with the `removed` / `intersectionLimiters` walks at `BaboonTranslator.scala:248-261` for `-` / `^`).

**Exit criteria.**
1. `data X { + MyGeneric[i32] }`, `- MyGeneric[i32]`, `^ MyGeneric[i32]` parse and lower correctly when `MyGeneric` is a registered template; the resulting `Typedef.Dto` carries the substituted-and-merged field list verbatim.
2. The transient instantiated template type **does not** appear as a `DomainMember.User` (M29 invariant: codegen never sees a template; only alias-id-keyed concrete types).
3. All nine codegen backends produce byte-identical output for pre-existing fixtures (no md5 churn outside `m33-*` paths).
4. Negative-path diagnostics for `+ NotATemplate[i32]`, `+ MyGeneric` (no args), `+ MyGeneric[i32, str]` (arity mismatch), forbidden type-arg, etc., emit precise messages.
5. Acceptance gate: `mdl :test-acceptance` 200/200 + the new m33 fixture rows; `mdl :test-service-acceptance` 81/81 unchanged; `sbt baboonJVM/compile` + `sbt baboonJS/compile` clean (any new TyperIssue case has its 3-site exhaustive-match update bundled per the M29 close-out playbook).

---

## 2. PR-level breakdown

Six PRs, sequenced. Each names the load-bearing files, the operational success command, and the M29 invariants the work must respect.

### PR-33.1 — Parser: optional `[…]` head on `+` / `-` / `^` arms

- **Scope.** Widen `parentDef`, `unparentDef`, `intersectionDef` so the operand can carry an optional type-argument list. Today they are `"+" ~ nonGenericTypeRef` (returns `ScopedRef`); we replace with a parser that returns `(ScopedRef, Option[NEList[RawTypeRef]])`. The `RawDtoMember.{ParentDef, UnparentDef, IntersectionDef}` case classes gain a new field `args: Option[NEList[RawTypeRef]]` (default `None`) so that all existing call sites (typer pre-pass + `BaboonTranslator.dtoParentToDefs`) compile unchanged when `args = None`.
- **Files touched.**
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala` (L116-129): rewrite the three rules to optionally match `typeParams` after the `nonGenericTypeRef`, returning the new pair shape. Reuse the existing `typeParams` rule at L14-18; do NOT use `templateHead` (which is for declaration-side type-params, bare identifier list).
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawDtoMember.scala`: add `args: Option[NEList[RawTypeRef]]` to all three of `ParentDef` / `UnparentDef` / `IntersectionDef`. Default `None` to preserve source compat.
  - `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AdtInheritanceParserTest.scala` and / or a new `M33StructuralTemplateInheritanceParserTest.scala`: positive parse tests for `+ Foo[i32]`, `- Foo[str]`, `^ Foo[Bar]`, `+ ns.Foo[i32]` (cross-namespace), and `+ Foo` (legacy, args = None).
- **AST/data-model changes.** New optional field on the three `RawDtoMember` cases; no new sealed-trait branch. Mirrors PR-29.2's "optional `typeParams` on existing case class" pattern (M29 plan §3.2 option (b)).
- **Success criterion.** `mdl --seq :build :test` green; new parser tests pass; existing fixtures unchanged. Grammar diff small enough that `:test-editors` (tree-sitter) will likely need a follow-up — call out explicitly so the executor doesn't silently leave a half-grammar update. (See PR-33.6 for the LSP/grammar bookkeeping.)
- **M29 invariants respected.** Locked decision #3 (alias-only instantiation) is widened by this milestone — but ONLY for the structural-composition operators (a strict, narrow extension). Field-position instantiation (`field: Foo[Bar]`) and nested instantiation in alias RHS (`type Y = Foo[Bar[i32]]`) remain forbidden; PR-33.3's negative-path tests must re-pin the existing matrix #1/#2 diagnostics to prove the widening did not leak.

### PR-33.2 — Typer: lower template instantiation in structural arms

- **Scope.** When `args.isDefined` on a `RawDtoMember.{ParentDef, UnparentDef, IntersectionDef}`, the typer must (a) instantiate the template into a synthesized concrete shape, (b) feed that shape into the existing structural-composition pipeline, (c) NOT register the synthesized template as a `DomainMember.User`.

  **Architectural lowering site (decision (a) in §3 below):** the new logic lives in `TemplateInstantiator` (extension of the existing `processMember` walk). When the instantiator sees a `RawTLDef.DTO` / `RawTLDef.Contract` whose body contains parent/unparent/intersection arms with `args.isDefined`, it:
  1. Resolves the head against `registry.templates` (using the same prefix-aware `resolveTemplateKey` shape currently used at `TemplateInstantiator.scala:575-586`).
  2. Validates arity vs `body.typeParams.size` — reuse `TyperIssue.TemplateArityMismatch` as-is.
  3. Substitutes the template body in place using the existing `substituteMembers` / `substituteTypeRef` helpers (which already handle prefix-aware nested-template detection from PR-29.15).
  4. Returns the substituted **list of `RawDtoMember`s** (from the template body) to be **inlined into the receiving DTO's member list** — i.e. the parent/unparent/intersection arm is replaced by the substituted body's field-defs **OR** is rewritten to point at a synthetic alias. (See §3 below for the chosen mechanism.)
  - The `+`/`-`/`^` semantic stays the same as today: `+` adds fields (current `dtoParentToDefs` is the precedent at `BaboonTranslator.scala:173-190`); `-` removes them (currently uses the same `dtoParentToDefs` reader at `BaboonTranslator.scala:248-255`); `^` intersects (`BaboonTranslator.scala:256-261`).
- **Files touched.**
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala`: extend the trait + `Impl` with a new pre-step that walks `RawTLDef.DTO` / `RawTLDef.Contract` member lists for `ParentDef|UnparentDef|IntersectionDef` arms with `args.isDefined`, substitutes, and rewrites them to a flat structural-arm shape consumable by `BaboonTranslator.dtoParentToDefs`. Add a recursion guard (use the body-walk safety check from PR-29.5: forbid the template body from carrying `args.isDefined` arms that re-instantiate the same template — same self-reference logic as the existing matrix #5 path).
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTyper.scala`: no pipeline-level shuffle is needed; the existing `templateInstantiator.instantiate` call at L434 already runs after `adtInheritanceExpander.expand` (L428) and before the second `buildScopes` (L441). Confirm by inspection that the rewrite for arms happens in the same pass.
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala`: **likely no change**. `dtoParentToDefs` (L173-190) reads the resolved id, looks it up in the `defined` map, and pulls fields from `Typedef.Dto`. After PR-33.2's lowering, the arm's `parent` (a `ScopedRef`) points at a real synthesized-and-registered concrete type; the existing reader works unchanged. **Verify in PR-33.2 that the synthesized type IS or IS NOT registered as a `DomainMember`.** This is the forking architectural decision (§3 below); the simpler path makes the arm-substitution inline (no intermediate `DomainMember`) and rewrites the receiving DTO's member list directly with the substituted field defs.
- **Success criterion.** `sbt 'testOnly *TemplateInstantiator*'` green with new positive cases (`+ MyGeneric[i32]`, `+ ns.MyGeneric[i32]`, mixed `+ Concrete; + MyGeneric[str]` arms in the same head). A typer unit test verifies that no `DomainMember.User` is emitted for the transient instantiated template (the synthesized type's id never appears in `Domain.defs`).
- **M29 invariants respected.** Locked decision #4 (no synthetic id `X<i32>` exposed) — the arm-substitution path either inlines fields directly (no alias id needed) OR creates a typer-internal-only alias whose id is suppressed before `DomainMember.User` emission. Either path keeps codegen blind to templates. The decision between "inline" and "internal alias" is the central architectural choice flagged in §3 below.

### PR-33.3 — Typer: negative-path diagnostics + 3-site exhaustive-match audit

- **Scope.** Pin the diagnostics for the misuse cases enumerated in §4 below. Most reuse existing `TyperIssue` cases; add at most ONE new case if the existing wording cannot be retargeted cleanly.
- **Files touched.**
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala`: emit the chosen `TyperIssue` at each of: arity mismatch (reuse `TemplateArityMismatch`); not-a-template head (reuse `NotATemplate`); bare template head missing brackets (reuse `TemplateNotInstantiated`); forbidden type argument (delegate to existing `convertTpe` validation post-substitution — re-uses whatever rule fires today for the substituted RHS). The "template instantiation in another template's body via structural arm with the parent's `T`" case **may** require a new `TyperIssue` — see §4 enumeration below.
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala`: ONLY if §4 confirms a new case is required. If so, add the case + its `IssuePrinter`.
  - **3-site exhaustive-match update** (mandatory if any new case is added — per CLAUDE.md guidance and the M29 close-out playbook):
    1. `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/DiagnosticsProvider.scala::extractTyperIssueInfo` — adds a printer-driven message arm.
    2. `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/state/WorkspaceState.scala::extractTyperIssuePointer` — adds a meta-pointer arm.
    3. `baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala::extractTyperIssuePointer` — same arm on the JS side. The `formatIssue` site has a `case _` fallback and does NOT block compile, but a precise arm there improves diagnostics quality.
- **Tests added.** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala` (or a new sibling `M33StructuralTemplateValidatorTest.scala` — see §5 for naming guidance) gains six negative cases mirroring §4 below.
- **Success criterion.** `sbt baboonJVM/compile` PASS, `sbt baboonJS/compile` PASS (cross-build catches any missed JS-side site), `sbt 'testOnly *M29Validator* *TemplateInstantiator*'` green.
- **M29 invariants respected.** The 3-site exhaustive-match obligation from M29 close-out is honoured. If only existing TyperIssue cases are reused, the cross-build is a no-op; if a new case is added it MUST be bundled into one touch per file (M29 close-out lesson).

### PR-33.4 — Cycle / self-reference detection (template ↔ structural arm)

- **Scope.** Two adversarial shapes need explicit handling:
  1. **Template instantiated inside its own body via a structural arm with its own type-param** — `template Inner[T] { data Inner { + Inner[T] } }`. After substitution this becomes `Inner` referring to itself with the alias-id; the existing `BaboonTyper.order` toposort (`BaboonTyper.scala:473-496`) catches it via `CircularInheritance`, but the wording will say "circular inheritance" not "template self-reference". Decide in PR-33.4 whether to upgrade the diagnostic or accept the existing wording.
  2. **Template whose body uses `+ OtherTemplate[T]`** — i.e. `template Outer[U] { data { + Inner[U] } }`. This is legitimate **iff** the substitution chain terminates. After substitution, `Outer[i32]` produces a concrete type whose body contains `+ Inner[i32]` — that arm itself must then be re-substituted. The PR-29.5 substitution walk is recursive over `RawTypeRef` but does NOT recurse into "fresh structural-arm template instantiations introduced by substitution"; PR-33.4 must thread that recursion. Concretely: after `substituteMembers` produces a new `ParentDef(args = Some(Subst(U → i32)))`, the instantiator must re-enter itself to substitute that arm too. Use a depth-limited recursion (e.g. 32) or a fixpoint with cycle-detection set keyed by `(receivingTypeId, templateId, argTuple)`.
- **Files touched.**
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala`: add the structural-arm recursion + cycle-detection set. The existing M29 toposort catches the post-substitution cycle for free if substitution terminates and produces a self-pointing concrete type.
  - Optional: `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala` — IF a new `StructuralArmTemplateCycle` case is added (only if the user-facing wording demands it).
- **Tests added.** New positive case "Outer over Inner via structural arm" and negative case "Inner instantiates itself via structural arm" in `TemplateInstantiatorTest.scala`. Place in `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/`.
- **Success criterion.** `sbt 'testOnly *TemplateInstantiator* *M29ValidatorTest* *M29TemplateCycleValidatorTest*'` green.
- **M29 invariants respected.** Locked decision #2 (DAG-only) — the recursion guard ensures self-instantiation terminates with a precise diagnostic, not a stack overflow.

### PR-33.5 — Cross-language acceptance fixture `m33-ok`

- **Scope.** Ship the canonical positive fixture under `test/conv-test/m33.baboon` (mirroring `test/conv-test/m29.baboon`'s pattern). Verify all nine backends round-trip JSON + UEBA byte-identically.
- **Files touched.**
  - **NEW** `test/conv-test/m33.baboon`: see §5 for the recommended source body. Use the same `model convtest.m33ok` / `version "1.0.0"` header and `: derived[json], derived[ueba]` annotations as `m29.baboon`.
  - `baboon-compiler/src/test/resources/baboon/m33-ok/m33.baboon`: typer-side fixture (compile-only validation).
  - Per-backend conv-test harnesses under `test/conv-test-{cs,sc,py,rs,ts,kt,kt-kmp,jv,dt,sw}/`: register the new fixture's root types in each backend's compat table. Crib the registration shape from how `m29.baboon` is wired — git log for `M29OkHolder` will show the exact pattern.
- **Tests added.** Cross-language parity: encode-A → decode-B → re-encode-B → byte-compare across all 9 backend pairs (the existing `:test-acceptance` matrix). 200/200 must remain 200/200 plus the new m33 rows.
- **Success criterion.** `mdl --seq :build :test-acceptance` green; the new m33 rows pass; existing m29 + pkg0 + … rows unchanged.
- **M29 invariants respected.**
  - **Wire-format invariants from M28-N01/N02:** the fixture must use canonical datetime / map-key shapes. Crib from `m28-ok/` if any `tso` / `tsu` field appears.
  - **No fixture md5 churn outside `m33-ok/`:** `git diff --stat target/test-regular/` after PR-33.5 must show only `m33-*` paths.

### PR-33.6 — LSP smoke + close-out

- **Scope.** End-of-milestone close-out mirroring PR-29.11.
- **Files touched.**
  - `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/DiagnosticsProvider.scala`, `.../HoverProvider.scala`, `.../CompletionProvider.scala`, `.../DefinitionProvider.scala`: smoke tests under `LspFeaturesTest.scala` for hover-on-`+ MyGeneric[i32]` and completion in the position right after `+ ` at a DTO body.
  - Tree-sitter grammar files under `editors/`: extend the `+` / `-` / `^` rule in the `data`/`contract` body to allow the optional `[…]` head. Tag whether this requires a submodule pointer bump (PR-29.8 surfaced this gotcha — see `docs/logs/20260504-0527-m29-deferred-drain-log.md`).
  - `docs/spec/generics.md`: extend §3 (or wherever structural inheritance is described) with the new arm syntax and its monomorphisation contract.
  - `tasks.md`: mark M33 closed.
  - `docs/logs/<YYYYMMDD-HHMM>-m33-close-log.md`: session log.
- **Success criterion.** `mdl --seq :build :test :test-editors` green; LSP smoke passes; spec doc lands.
- **M29 invariants respected.** Tree-sitter chain: per the PR-29.8 log, ensure the editor submodule pointer is bumped explicitly; do not let `:test-editors` pass against a dirty working tree.

---

## 3. Architectural decisions (must resolve before code lands)

### 3.a — Where the instantiation-then-inheritance lowering happens

**Resolved (planner):** `TemplateInstantiator.Impl.processMember` in `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala`, extended to walk `RawTLDef.DTO` and `RawTLDef.Contract` member lists and rewrite their `RawDtoMember.{ParentDef, UnparentDef, IntersectionDef}` arms when `args.isDefined`. Rationale: `TemplateInstantiator` already runs at the right pipeline slot (`BaboonTyper.scala:434`, between ADT inheritance expansion at L428 and the second `buildScopes` at L441) and already owns the substitution helpers (`substituteMembers`, `substituteTypeRef`, `resolveTemplateKey`). Putting the lowering elsewhere (e.g. inside `BaboonTranslator.dtoParentToDefs`) would require translator-time access to the template registry, which is a layering violation — the translator operates on the post-substitution raw AST by design (M29 §3.6 option K).

### 3.b — How the transient instantiated type is prevented from being registered as a `DomainMember.User`

**Resolved by orchestrator (2026-05-05): Option I — inline substitution.**

The instantiator substitutes the template body and **inlines the resulting field-defs / contract-refs directly into the receiving DTO's `members` list**, replacing the structural-arm node. Concretely: `+ MyGeneric[i32]` with `template MyGeneric[T] { data { v: T } }` becomes a literal `RawDtoMember.FieldDef(name="v", tpe=Simple(i32))` appended to the receiving DTO's member list. No transient `RawTLDef.DTO` is produced; no `DomainMember.User` is registered. This is structurally identical to how `AdtInheritanceExpander` substitutes `+ X` ADT-arm refs by copying the target's literal branches (`AdtInheritanceExpander.scala:198-213`).

Rationale: zero new id-space pollution; trivially honours M29 invariant "codegen never sees a template" because no synthesized type exists at all. The alternative (Option II — synthetic alias name + post-pass strip) was rejected because the M29 close-out repeatedly emphasised "no synthetic id exposed", and even an internal-only synthetic id risks accidentally surviving into `Typedef.Dto.contracts` references or evolution-diff machinery.

### 3.c — Composition with operators and mixed concrete+template refs

**Resolved (planner-side):** the new syntax composes uniformly with all three operators `+`/`-`/`^`. Mixed concrete + template arms in the same head (`+ Concrete; + MyGeneric[i32]; - SomeOther`) are supported and order-preserving. Rationale: PR-33.1's parser treats the `args` field as an Option on each of the three arm cases; `TemplateInstantiator` substitutes each arm independently before the receiving DTO ever reaches `BaboonTranslator.dtoParentToDefs`. The arm-order semantics from `BaboonTranslator.convertDto` (L232-310) are preserved because substitution rewrites each arm in place. Negative case to pin in PR-33.3: `- MyGeneric[i32]` where `MyGeneric` was never `+`-included produces the same diagnostic as today's `- NonParent` (existing `WrongParent` / no-op behaviour — verify by reading `convertDto.removed` semantics).

### 3.d — Cross-namespace templates

**Resolved (planner-side):** YES, this rides PR-29.15's hardened cross-ns alias-RHS instantiation. PR-29.15 hardened `TemplateInstantiator.resolveTemplateKey` (`TemplateInstantiator.scala:575-586`) to honour `prefix.nonEmpty` → `Owner.Ns(prefix)` for cross-namespace lookups. PR-33's new structural-arm path uses the same resolver. Cross-package (different `Pkg`) remains out of scope per PR-29.15's documented limitation (spec §6 item 11).

PR-33.3's negative tests must include a positive `+ ns.NsTemplate[i32]` case to verify cross-ns instantiation works in structural-arm position, and a negative case `+ otherpkg.OtherTemplate[i32]` to pin the deferred-cross-package wording.

### 3.e — Contracts-edge for evolution-diff

**Resolved by orchestrator (2026-05-05): no contracts edge.**

The receiving DTO's `Typedef.Dto.contracts` field does NOT record the template-instantiation as a contract-like dependency edge. Treat identically to a pre-M33 `+ ConcreteRef`, which today drops the parent identity in `dtoParentToDefs`. Rationale: M29's invariant is "codegen never sees a template", and the evolution-diff machinery already handles `+ ConcreteRef` flattening without a parent-identity edge. If a future evolution-diff consumer needs the linkage, it can be added as a separate, opt-in mechanism at that time.

### 3.f — Diagnostic wording for self-instantiation cycles (rows 7 / 9 in §4)

**Resolved by orchestrator (2026-05-05): reuse existing `CircularInheritance` first.**

PR-33.4 ships the recursion guard + cycle-detection set, but emits the existing `CircularInheritance` `TyperIssue` rather than a new `StructuralArmTemplateSelfReference` / `StructuralArmTemplateCycle` case. Adding a new case forces a 3-site exhaustive-match update (M29 close-out cost) and the wording difference is small. If the existing wording proves user-confusing in PR-33.4 review, the decision reopens and the loop adds the new case (with the bundled 3-site update).

---

## 4. Negative-path coverage

Each row below is a fixture under `baboon-compiler/src/test/resources/baboon/m33-bad-*/` plus a unit-test arm in `M29ValidatorTest.scala` (or a new `M33StructuralTemplateValidatorTest.scala` — see §5).

| # | Misuse | Existing TyperIssue case (reuse) | New case required? |
|---|---|---|---|
| 1 | `+ NotATemplate[i32]` (head resolves to a non-template, non-builtin ref) | `TyperIssue.NotATemplate` (already at `TyperIssue.scala`, used in `TemplateInstantiator.processMember` at line ~150) | NO — reuse |
| 2 | `+ MyGeneric` (no brackets, head IS a registered template) | `TyperIssue.TemplateNotInstantiated` (already used for the same shape on alias RHS) | NO — reuse |
| 3 | `+ MyGeneric[i32, str]` (arity mismatch) | `TyperIssue.TemplateArityMismatch` | NO — reuse |
| 4 | `+ MyGeneric[T]` where `T` is a forbidden type per existing template-arg rules | Existing `convertTpe` arm rules fire post-substitution (likely `CollectionExpected`, `EmptyGenericArgs`, or whatever rule applies to the substituted-in type) | NO — reuse the existing post-substitution validators |
| 5 | Cross-namespace template via `+`: `+ ns.NsTemplate[i32]` (positive case) | n/a (positive) — PR-29.15 already hardened the resolver | NO |
| 6 | Cross-package template via `+`: `+ otherpkg.OtherTemplate[i32]` | Existing cross-package rejection (template registry is per-`Pkg`, lookup misses) → `NotATemplate` will fire because the head doesn't hit the registry. Spec §6 item 11 limits this scope per PR-29.15. | NO — reuse, verify wording |
| 7 | Template instantiated inside its own body via structural arm: `template X[T] { data X { + X[T] } }` | After substitution, post-toposort `BaboonTyper.order` (L489-491) fires `CircularInheritance`. Wording will say "circular inheritance" not "template self-reference". | NO — reuse `CircularInheritance` per §3.f. |
| 8 | Template whose body uses `+ OtherTemplate[T]`: `template Outer[U] { data { + Inner[U] } }` (positive — must work) | n/a — PR-33.4's recursive substitution makes this work. | NO |
| 9 | Template whose body uses `+ OtherTemplate[T]` with a cycle: `template A[U] { data { + B[U] } }; template B[U] { data { + A[U] } }`, instantiated via `type X = A[i32]` | Recursive substitution diverges → guard fires at depth limit OR cycle-detection set. | NO — reuse `CircularInheritance` per §3.f. |
| 10 | Template instantiation with another template's parameter: `template Outer[U] { data { + Inner[U] } }` — verify `U` substitutes correctly into `Inner`'s instantiation when `Outer[i32]` is materialised | n/a (positive) — must work via recursive substitution | NO |
| 11 | Inlined-then-redundant: `data X { + MyGen[i32]; + MyGen[i32] }` (same arm twice) | Existing `NonUniqueFields` (`BaboonTranslator.scala:316-319`) fires after both substitutions inline duplicate fields. | NO — reuse; pin the wording |
| 12 | Inlined-then-conflicting: `data X { + MyGen[i32]; v: str }` where `MyGen[T]` produces a field named `v: T` | Existing `NonUniqueFields` | NO — reuse |

**Adversarial / non-obvious cases highlighted explicitly:**
- Row 7 (self-instantiation via structural arm) is the close cousin of M29's matrix #5; the M29 path is field-position; M33's is structural-arm-position. PR-33.4 must own the recursion-guard.
- Row 8 + 10 (template body uses another template via structural arm with a parent type-param) is the most complex shape and the least-obvious-to-an-executor. PR-33.4's tests MUST include this positive case explicitly. The recursion is: substitute `U → i32` in `Outer`'s body, which produces `+ Inner[i32]`, which the instantiator must then re-substitute by entering its own instantiation logic with the new arm.
- Row 9 (mutual recursion through structural arms) is the genuine cycle — depth-limit guard or cycle-detection-set in `TemplateInstantiator` must catch it before the post-substitution toposort would (because the toposort runs on receivingTypeId, not on intermediate template instantiations).

---

## 5. Tests / fixtures

### Positive cross-language fixture

- **Path:** `test/conv-test/m33.baboon` (sibling of `test/conv-test/m29.baboon`).
- **Recommended source body:**
  - Package: `model convtest.m33ok` / `version "1.0.0"`.
  - Two templates: `data Page[T] { items: lst[T]; total: u32 }` and `data Stats[T] { sum: T; count: u32 }`.
  - One concrete DTO using `+ Template[Concrete]` in its head: `data IntPageWithStats { + Page[i32]; + Stats[i32] }` exercises the `+` operator with two templates instantiated at the same concrete type.
  - One that uses `-`: `data PageOnly { + Page[str]; - Stats[str] }` (Stats is not part of PageOnly's parents — pin existing `WrongParent`-or-similar wording, or use a contract-based example).
  - One using `^`: a structural intersection over two templates.
  - One root-aliased holder for cross-language acceptance: `root data M33OkHolder : derived[json], derived[ueba] { … }`.
- **Acceptance harness rows:** all 9 backends × source/sink pairs round-trip JSON + UEBA byte-identically.

### Negative-path validator tests

- **File:** extend `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala` (the M29 successor) — keep all M33 negatives there to stay close to the M29 cases. Naming convention: `m33_bad_<rowN>_<short-tag>`.
- Fixtures under `baboon-compiler/src/test/resources/baboon/m33-bad-{1..9,11,12}/`.
- Each test body: load fixture, run typer, assert the resulting `BaboonIssue` has exactly one `TyperIssue` of the expected case.

### Positive typer-only tests

- **File:** extend `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/TemplateInstantiatorTest.scala`.
- Cases:
  - `+ MyGeneric[i32]` — fields inlined.
  - `- MyGeneric[i32]` — fields removed (assert receiving DTO loses the named fields).
  - `^ MyGeneric[i32]` — fields intersected.
  - `+ ns.NsTemplate[i32]` — cross-namespace.
  - `+ Concrete; + MyGeneric[i32]` — mixed.
  - `+ MyGeneric[T]` inside another template body — recursive substitution (row 8/10).

### Per-language emission tests

**No emission test additions expected.** The M29 architectural bet (codegen never sees a template) holds: after PR-33.2's inline substitution, the receiving DTO is a vanilla `Typedef.Dto` with literal field-defs. If the cross-language acceptance in PR-33.5 passes, the codegen surface is provably unchanged. **Call out explicitly in PR-33.5's PR description**: "no per-language emission test additions; cross-language acceptance is the gate."

---

## 6. Risks and unknowns

### R1. Does the parser ALREADY accept `+ Foo[T]` and silently drop the args?

**Investigation result (cited):** NO. The current parser explicitly uses `nonGenericTypeRef` at `DefDto.scala:116` (parentDef), L121 (unparentDef), L128 (intersectionDef). `nonGenericTypeRef` (L31-33) returns a `ScopedRef` and consumes only `idt.symbolSeq` — it cannot consume `[…]`. A `+ Foo[T]` input today fails to parse cleanly: `Foo` parses, then `[T]` is unconsumed and the dto-member loop terminates, leaving the `[T]` to be misparsed as a top-level construct → parse error. **Verify in PR-33.1** by writing a test that today's parser rejects `+ Foo[T]`; this is the regression-guard before the widening.

### R2. `BaboonTranslator.dtoParentToDefs` reads from `defined: Map[TypeId, DomainMember]` (L173-190)

If Option I in §3.b is chosen (inline substitution), the structural-arm rewriting must happen BEFORE `BaboonTranslator.translate` is invoked (i.e. inside `TemplateInstantiator`, which runs at `BaboonTyper.scala:434` BEFORE the second `buildScopes` and the `translator.translate()` loop at L446-458). Verify the ordering by tracing pipeline calls. The risk is that an executor naively places the rewrite inside the translator and breaks the M29 invariant.

### R3. Tree-sitter editor grammar drift

PR-33.6 must bump the tree-sitter grammar in `editors/`; per PR-29.8 history, the submodule chain (3-level) is easy to miss. Mitigation: PR-33.6 explicitly lists the submodule pointer-bump as a task; `:test-editors` must run against a clean working tree.

### R4. Codegen byte-stability for existing fixtures

The M29 architectural bet (codegen unchanged) MUST be re-verified in PR-33.5. Mitigation: `git diff --stat target/test-regular/ target/test-wrapped/` after PR-33.5; only `m33-*` paths must appear.

### R5. JS-side cross-build regression

Any new `TyperIssue` case in PR-33.3 / PR-33.4 MUST update the BaboonJS-side exhaustive match (`baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala::extractTyperIssuePointer`). Mitigation: bundle all new cases per PR per file (M29 close-out lesson, codified in CLAUDE.md). Per §3.f, no new cases are planned, so this risk is dormant unless §3.f reopens.

### R6. NOT-deferrable: structural-arm template instantiation in template body (§4 row 8/10)

This is THE non-trivial recursion. If PR-33.4 ships without recursive substitution, `template Outer[U] { data { + Inner[U] } }` produces a half-substituted intermediate that breaks downstream — silent corruption. Must be implemented in PR-33.4, NOT deferred.

---

## 7. Acceptance gates

In order of execution, before close-out:

1. `mdl --simple-log :build` — full cross-build (`sbt +compile`); verifies JVM + JS exhaustive-match.
2. `sbt clean compile` — required if PR-33.6 touches `baboon-compiler/src/main/resources/baboon-runtime/` (per CLAUDE.md PortableResource macro caching).
3. `sbt baboonJVM/compile` and `sbt baboonJS/compile` — explicit cross-build coverage of any new `TyperIssue` exhaustive-match.
4. `sbt 'testOnly *TemplateInstantiator* *M29Validator* *M29TemplateCycleValidator* *LspFeatures*'` — targeted typer + validator + LSP verification.
5. `sbt baboonJVM/test` — full JVM test suite (target: 393+/393+ passing — exact count depends on m33 additions).
6. `mdl :test-acceptance` — cross-language wire-form, must remain at 200/200 baseline plus new m33 rows.
7. `mdl :test-service-acceptance` — must stay at 81/81 (M33 does NOT touch services).
8. `mdl :test-editors` — tree-sitter; only after PR-33.6's grammar bump.
9. `mdl :build :test` — final close-out gate.
10. `mdl :fmt` — format hygiene.

Use `mdl --seq :build :test` for executor close gates (sequential test execution avoids the Kotlin-daemon OOM on <16 GB RAM laptops; CI uses default parallelism).

---

*End of plan document.*

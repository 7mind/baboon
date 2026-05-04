# M30 — Docstring / comment preservation — implementation plan

Authoritative plan for milestone M30. Mirrors the M29 plan-doc shape
(`docs/drafts/20260503-2210-m29-generics-plan.md`).

## 1. Goal and scope

Preserve `/** … */` prefix doc comments on type definitions, method
definitions, and fields, plus `//! …` postfix doc comments on fields, and
emit them in idiomatic form from every backend (Scala, C#, Python, Rust,
TypeScript, Kotlin incl. KMP, Java, Dart, Swift, GraphQL SDL, OpenAPI 3.1).

Docs are **not** part of the wire format, are **not** evolution-tracked,
and are **not** load-bearing for codecs. They flow:

```
.baboon source → RawNodeMeta.docs → typed Domain (Docs on Field /
                                    MethodDef / DomainMember.User)
                → per-backend renderer (DocFormat.render)
                → emitted source comments
```

Out of scope:

- tree-sitter editor-grammar updates (deferred submodule PR per
  `[PR-29.8-D01]` precedent);
- preserving non-doc comments (`//`, `/* … */`);
- preserving inline doc placement at sub-statement granularity;
- Markdown / RST normalisation of doc body content beyond the cleanup
  rule below.

## 2. Locked architectural decisions (proposed; (4) and (5) need user
sign-off before PR-30.1)

1. **Parser strategy — anchored doc rule, keep `ScalaWhitespace`.** Add a
   small `DocComments` parser building block. At each grammar position
   where a doc may legally precede a declaration (top-level
   `meta.member`, service-method `meta.member` for `def`, dto-member
   `meta.withMeta(fieldDef)`), insert an explicit `docComments.?` rule
   **before** invoking `meta.member` / `meta.withMeta`. Inside
   `docComments` use `NoWhitespace` so the rule cannot accidentally
   swallow following whitespace and mis-bind the doc. Postfix `//!` on
   fields is captured at the end of `fieldDef`, anchored by
   `(NLC | end-of-input)`.
   Justification: lowest blast radius — keeps the existing 84+ parser
   tests intact, avoids a custom `Whitespace` (which would touch every
   `~` site and drag every fixture into churn), unambiguously binds doc
   → declaration because `meta.member` and `fieldDef` are exact
   one-spot positions where `RawNodeMeta` is currently produced.

2. **Where docs land on raw AST.** Extend `RawNodeMeta`:

   ```scala
   case class RawNodeMeta(pos: InputPointer, docs: RawDocs = RawDocs.empty)
   case class RawDocs(prefix: List[RawDocComment], suffix: List[RawDocComment])
   case class RawDocComment(raw: String, pos: InputPointer)
   ```

   Cleanup (strip `/**`, `*/`, leading `*`, normalise to a single
   trimmed string per logical paragraph) happens at **emission time**
   in a small `DocFormat` helper colocated with each backend's
   `*TreeTools.scala`.

3. **Threading docs into the typed model.** Extend `Field`,
   `Typedef.MethodDef`, and `DomainMember.User` with
   `docs: Docs = Docs.empty` where `Docs` is the typed-side
   counterpart of `RawDocs`:

   ```scala
   case class Docs(prefix: List[DocComment], suffix: List[DocComment])
   case class DocComment(raw: String, cleaned: String)
   ```

   `cleaned` is computed once via `DocFormat.clean(raw)` (strips
   delimiters and `*` indentation; leaves the inner text). Backends
   consume `cleaned` and may further wrap/escape per language.

4. **Template monomorphisation interaction (M29).** Default proposed,
   needs user sign-off:
   - Docs on the template body's fields propagate verbatim to the
     materialised concrete DTO (substitution carries `RawNodeMeta.docs`
     across).
   - Docs on the alias (`/** … */ type Y = X[Foo]`) override the
     template's *type-level* docs. Field-level docs from the template
     body are **not** overridden by alias docs.
   - Docs on the template's *type* declaration
     (`/** … */ data X[T] { … }`) are the fallback when the alias has
     no docs; otherwise they are dropped.

5. **Python attribute-doc convention.** Default proposed, needs user
   sign-off: emit a triple-quoted attribute docstring as a separate
   string-literal statement immediately following the assignment, per
   PEP 257 attribute-docstring form. Class-level docstring uses the
   standard "first statement is the docstring" pattern. Method docstring
   same.

6. **Per-backend emission idioms.** Locked in spec:
   - Scala / Java / Kotlin (incl. KMP) / TypeScript: Javadoc-style
     `/** … */` with `*` line prefix, before the symbol.
   - C#: `/// <summary>…</summary>` XML doc; multi-paragraph splits
     across `<summary>` / `<remarks>`.
   - Python: per (5).
   - Rust: outer-line `///` before the item. **No `//!`** — `//!` is
     module-inner. Postfix `//!` doc on a baboon field maps to a `///`
     line **before** the field in Rust output.
   - Dart: `///` Dart doc-comments.
   - Swift: `///` (apple-doc default).
   - GraphQL SDL: `"""…"""` block string description before the
     type/field/argument.
   - OpenAPI 3.1: `description` JSON Schema key.

7. **No wire-format change.** Docs are NOT transmitted; existing
   non-`m30-*` fixtures must produce byte-identical output.

8. **Stacked prefix docs.** Multiple `/** … */` blocks stacking with
   no intervening declaration: accumulate as a `List[RawDocComment]`,
   emit concatenated with blank line. (Open-question (3) — confirm with
   user.)

9. **Cleanup-time policy.** Raw preserved at parser; clean once at
   typer-stage `Docs.cleaned`; backends use `cleaned` and may further
   escape. (Open-question (4) — confirm.)

## 3. Pipeline placement

```
parse → RawTLDef tree
   (RawNodeMeta now carries docs from anchored prefix/suffix rules)
   ↓
TemplateRegistryBuilder         ← unchanged; cached template body retains
                                  doc-bearing meta
TemplateInstantiator            ← substitution preserves meta.docs
                                  verbatim; alias-level RawNodeMeta.docs
                                  overrides template-type docs per
                                  decision (4)
   ↓
BaboonTranslator.convert*       ← thread RawNodeMeta.docs → Field.docs /
                                  MethodDef.docs / DomainMember.User.docs
   ↓
Domain → backends               ← each backend's *DefnTranslator emits
                                  via DocFormat helper
```

## 4. PR breakdown

### PR-30.1 — Spec doc

- Goal: ship `docs/spec/docstrings.md` covering surface syntax, content
  cleanup rule, anchoring rules, propagation under template
  monomorphisation, per-backend idioms, scope-out list.
- Touch list: `docs/spec/docstrings.md` (new); cross-link from
  `docs/spec/generics.md` for the propagation rule.
- Tests: none (doc-only).
- Success: `git status` clean except for the new doc; spec-vs-parser
  syntax tracked one-to-one with PR-30.2.
- Dependencies: locked decisions (4) and (5) signed off.
- Risk: spec text and PR-30.2 parser-error wording must track each other.

### PR-30.2 — Parser: doc-comment capture

- Goal: parse `/** … */` and `//! …` and surface them in
  `RawNodeMeta.docs`.
- Touch list:
  - `parser/defns/base/DefDocs.scala` (new) — `prefixDoc[$: P]`,
    `prefixDocs[$: P]`, `suffixDoc[$: P]`. Implement under
    `NoWhitespace`.
  - `parser/model/RawNodeMeta.scala` — extend with `docs` (defaulted);
    add `RawDocs`, `RawDocComment`.
  - `parser/defns/DefMeta.scala` — extend `withMeta` and `member` to
    optionally consume `prefixDocs` before the body and stitch them
    into the resulting `RawNodeMeta`.
  - `parser/defns/DefDto.scala` — wire suffix-doc capture inside
    `fieldDef` (and `dtoMember`'s `FieldDef` branch) by extending
    `fieldDef` to read an optional `//! …` after the field.
  - `parser/defns/DefService.scala` — verify methods inherit the prefix-
    doc capture from the shared `meta.member` hook.
  - `parser/defns/DefAdt.scala`, `DefContract.scala`, `DefEnum.scala`,
    `DefForeign.scala`, `DefModel.scala` — verify each `meta.member`
    site picks up prefix docs without further change.
- Tests (new parser unit tests): prefix `/** */`; multi-line
  `/** \n * x \n */`; **stacked prefix `/** … */ /** … */` blocks
  produce `ParserIssue.StackedDocComments`** (per Q3 lock); suffix
  `//!` on a field; suffix `//!` followed by another field on the next
  line does NOT bind to the next field; plain `//` and `/* */`
  comments NOT captured; doc on a line by itself between two
  declarations binds to the **following** declaration; empty `/***/`
  edge case (decision pending Q4c).
- Success: `mdl :build :test` green; existing parser tests unchanged;
  no fixture md5 churn.
- Dependencies: PR-30.1.
- Risk: FastParse `ScalaWhitespace` already swallows `/** … */` as
  ordinary block comments. The anchored rule must intercept **before**
  the implicit whitespace boundary fires. Mitigation: write a parser-
  only spike test first; confirm `ScalaWhitespace` behaviour; route
  the prefix-doc consumption through a `NoWhitespace`-bounded segment
  inside `meta.member` / `meta.withMeta` that runs before the implicit
  `ScalaWhitespace` handover.

### PR-30.3 — Typer / domain model: thread docs

- Goal: transport `RawNodeMeta.docs` into `Field.docs`,
  `Typedef.MethodDef.docs`, and `DomainMember.User.docs`.
- Touch list:
  - `typer/model/Typedef.scala` — extend `Field`, `MethodDef` with
    `docs: Docs = Docs.empty`; add `case class Docs`, `case class
    DocComment`.
  - `typer/model/DomainMember.scala` — extend `User` with `docs`.
  - `typer/BaboonTranslator.scala` — propagate `RawNodeMeta.docs`
    through `dtoFieldToDefs`, `convertEnum`, `convertDto`, the
    method-conversion path, and per-`DomainMember.User` construction.
  - `typer/TemplateInstantiator.scala` — substitution preserves
    `meta.docs` verbatim; apply alias-overrides-template-type rule per
    decision (4).
  - `typer/AdtInheritanceExpander.scala` — verify ADT-arm rewriting
    preserves arm DTO docs.
- Tests: typer unit tests over a doc-bearing fixture; template
  propagation test (`data Box[T] { /** elem */ value: T }`,
  `/** an int box */ type IB = Box[i32]`).
- Success: `mdl :build :test` green; **no fixture md5 churn** (see
  cross-cutting concern (4)).
- Dependencies: PR-30.2.
- Risk: every constructor-call site for `Field`, `MethodDef`,
  `DomainMember.User` needs the new default. Defaulted parameters
  mitigate, but exhaustive `case Field(n, t, p) =>` patterns will fail
  to compile after the field is added. Audit codec generators and
  conversion translators; add wildcard slot where docs are not consumed.

### PR-30.4 — Backend: Scala

- Goal: emit Javadoc-style docs.
- Touch: `translator/scl/ScDefnTranslator.scala`, `ScTreeTools.scala`
  (helper `renderDocs(docs: Docs, indent: String): String`); class /
  case-class / method / field headers call `renderDocs`.
- Success: `mdl :test-scala-regular` green; new fixture's Scala output
  contains expected `/** … */`.
- Dependencies: PR-30.3.

### PR-30.5 — Backend: C#

- Goal: `/// <summary>…</summary>` XML doc.
- Touch: `translator/csharp/CSDefnTranslator.scala`, `CSTreeTools.scala`
  (helper `renderXmlDocs`); XML-text escape `<`, `>`, `&`.
- Success: `mdl :test-cs-regular` green.
- Risk: escaping. Document escape rule in spec.

### PR-30.6 — Backend: Python

- Goal: triple-quoted class/method docstrings per Q2 lock; field docs
  folded into the class docstring as a Google/Sphinx `Attributes:`
  section keyed by field name.
- Touch: `translator/python/PyDefnTranslator.scala`. Class-level /
  method-level: `"""…"""` as first statement. Field-level: NO per-
  field statement; the class docstring renderer composes
  `class_doc + "\n\nAttributes:\n    {name}: {field_doc}\n…"` from the
  parent class's `Docs` plus each `Field.docs`. ADT arms inherit the
  same shape: each arm class folds its own field docs into its arm
  docstring.
- Risk: `"""` escaping when docs contain triple-quote — replace with
  `\"\"\"`. Indentation alignment of multi-paragraph field docs inside
  the `Attributes:` section: continuation lines indent by 8 spaces
  (4 for `Attributes:` block + 4 for continuation) per Sphinx/Google
  convention.

### PR-30.7 — Backend: Rust

- Goal: `///` outer-line doc comments.
- Touch: `translator/rust/RsDefnTranslator.scala`, `RsTreeTools.scala`;
  field suffix `//!` → `///` BEFORE the field per decision (6).
- Risk: low.

### PR-30.8 — Backend: TypeScript

- Goal: Javadoc.
- Touch: `translator/typescript/TsDefnTranslator.scala` and tools.
- Risk: low.

### PR-30.9 — Backend: Kotlin (incl. KMP)

- Goal: KDoc (`/** … */`).
- Touch: `translator/kotlin/Kt*DefnTranslator.scala`. Verify whether
  KMP shares the JVM emitter; if shared, single PR; if forked, two
  parallel sub-PRs.
- Risk: emitter-sharing — verify in PR-30.3 close-out.

### PR-30.10 — Backend: Java

- Goal: Javadoc.
- Touch: `translator/java/JvDefnTranslator.scala` and tools;
  HTML-escape `<`, `>`, `&` for Javadoc compatibility.
- Risk: low.

### PR-30.11 — Backend: Dart

- Goal: `///` Dart doc-comments.
- Touch: `translator/dart/DtDefnTranslator.scala` and tools.
- Risk: low.

### PR-30.12 — Backend: Swift

- Goal: `///` Swift docs.
- Touch: `translator/swift/SwDefnTranslator.scala` and tools.
- Risk: low.

### PR-30.13 — Backend: GraphQL SDL + OpenAPI 3.1

- Goal: GraphQL `"""…"""`; OpenAPI `description` keys. Bundled because
  each is small.
- Touch: `translator/graphql/{GqlBaboonTranslator, GqlTypeTranslator}.scala`;
  `translator/openapi/{OasBaboonTranslator, OasTypeTranslator}.scala`.
- Risk: GraphQL block-string `"""` escaping; OpenAPI `description` is
  plain JSON string.

### PR-30.14 — LSP: hover surfaces docs

- Goal: hover renders cleaned doc body for type/field/method when
  present.
- Touch: `lsp/features/HoverProvider.scala`. Optionally
  `CompletionProvider.scala` adds `documentation` to `CompletionItem`.
- Tests: LSP unit tests for a doc-bearing fixture.
- Risk: low. **No new `TyperIssue`**; the M29 3-site exhaustive-match
  audit is NOT required for this milestone.

### PR-30.15 — Cross-language smoke fixture `m30-ok` + close-out

- Goal: a single fixture covering every supported doc position (type,
  ADT, ADT-arm, contract, service, service-method, enum-member,
  field-prefix, field-suffix, type-alias). All 11 backends emit docs
  in their output.
- Touch:
  - `baboon-compiler/src/test/resources/baboon/m30-ok/m30.baboon` (new).
  - Per-backend stub goldens / golden refresh under
    `test/{cs,sc,py,rs,ts,kt,jv,dt,sw}-stub/`. GraphQL/OpenAPI goldens
    under their respective fixture dirs.
  - `tasks.md` close-out (orchestrator-driven).
  - `docs/logs/<YYYYMMDD-HHMM>-m30-docstrings-log.md` (new).
- Tests: `mdl :test :test-acceptance` green; `git diff --stat` shows
  changes only in `m30-ok/` paths and new goldens. **All other
  fixtures byte-identical.**
- Risk: a backend's `Field` constructor pattern matched without
  wildcard would break compile — caught earlier by PR-30.3 audit.

## 5. Cross-cutting concerns

1. **Constructor-pattern audit for `Field` / `MethodDef` /
   `DomainMember.User`.** Defaulted `docs` slot; audit `case Field(n,
   t, p) =>` etc. patterns, especially in codecs
   (`*JsonCodecGenerator.scala`, `*UEBACodecGenerator.scala`) and
   conversion translators. Use wildcard `case Field(n, t, p, _) =>`
   where docs are not consumed.
2. **No new `TyperIssue` cases expected.** No 3-site exhaustive-match
   update needed for M30.
3. **External wire format unchanged.** Docs do NOT enter JSON / UEBA /
   digest hashes.
4. **Schema-id digest stability — load-bearing.** PR-30.3's audit MUST
   verify `ShallowSchemaId` / `DeepSchemaId` for any existing fixture
   does NOT churn after `docs` is added to `Field` / `MethodDef`. If
   the digest computation walks `Field` case-class fields naively,
   exclude `docs` explicitly via a structural-only projection. This is
   the most likely path to silent fixture md5 churn.
5. **Tree-sitter editor grammar.** Out of scope per `[PR-29.8-D01]`
   precedent. Note in plan and spec.
6. **Local OOM on Kotlin.** Use `mdl --seq :build :test` for executor
   close gates.
7. **Resource-file caching.** No runtime resource changes expected; if
   a backend needs a doc-formatting helper, prefer codegen-side
   formatting over a runtime helper.

## 6. Locked answers (user 2026-05-04)

- **Q1 — template doc propagation.** Type-level doc on the synthesized
  DTO = **alias doc + blank line + template-type doc**, in that order.
  Both are emitted when both exist; either alone is emitted when only
  one exists. Field docs propagate verbatim from the template body
  (alias declarations have no field docs). This **revises decision (4)**
  in §2.
- **Q2 — Python field-doc convention.** Python field docs are folded
  into the **class-level docstring** as a Sphinx/Google-style
  `Attributes:` section keyed by field name. Python field-level emission
  produces NO per-field docstring statement. Class/method docstrings
  use the standard PEP-257 `"""…"""` first-statement form. This
  **revises decision (5)** in §2.
- **Q3 — stacked prefix docs.** Two `/** … */` blocks back-to-back
  with no declaration between them is a **parser error** —
  `ParserIssue.StackedDocComments(pos)`. This **revises decision (8)**
  in §2 and removes "accumulate as `List[RawDocComment]`": prefix docs
  are at most one block per declaration. Update RawDocs to
  `prefix: Option[RawDocComment]`. (Suffix `//!` on fields is also at
  most one — already a single-line form.)
- **Q4a — cleanup ownership.** Single typer-stage cleanup function
  `DocFormat.clean(raw: String): String`. `DocComment(raw, cleaned)`
  carries both. Backends consume `cleaned` and apply per-language
  escaping (XML for C#, `"""` for Python / GraphQL, HTML for Java).
- **Q4b — canonical cleaned form.** Strip `/**` and `*/`. For
  multi-line: compute the common leading whitespace + `*` prefix
  across interior lines and strip it; preserve paragraph structure by
  keeping internal blank lines as paragraph separators; collapse
  leading and trailing blank lines; trim trailing whitespace per line.
  For postfix `//! …`: strip the `//!` marker and a single optional
  leading space; result is the trimmed inline text.
- **Q4c — empty / whitespace-only doc bodies.** Silently drop. No
  `Docs` is attached to the carrier node. No diagnostic.

## 7. Critical files for implementation

## 7. Critical files for implementation

- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefMeta.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawNodeMeta.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/DomainMember.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala`
- per-backend `translator/<lang>/{*DefnTranslator, *TreeTools}.scala`
- `lsp/features/HoverProvider.scala`

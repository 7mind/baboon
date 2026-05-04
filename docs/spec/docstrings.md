# Docstrings — Source-level Doc-Comment Preservation (M30 / BAB-A05)

**Status:** authoritative specification for doc-comment preservation in
the Baboon DML. Implementation is delivered across PR-30.2 .. PR-30.15
of milestone M30; the per-PR breakdown lives in
`docs/drafts/20260504-1213-m30-docstrings-plan.md` and `tasks.md`
("Milestone M30 — PR breakdown"). The locked design decisions (Q1–Q4,
user-blessed 2026-05-04) are reproduced verbatim from
`docs/drafts/20260504-1213-m30-docstrings-plan.md` §6 and supersede the
proposed defaults captured in §2 of that plan.

This document is the single source of truth. When the compiler and this
document disagree, the document wins and the compiler is wrong.

---

## 1. Overview

A **doc comment** in Baboon is one of two surface forms:

- a **prefix doc comment** opened by `/**` and closed by `*/`, attached
  to the immediately following declaration (type definition, service
  method, or field);
- a **postfix line doc comment** opened by `//!` and terminated at the
  end of the source line, attached to the field whose definition shares
  that line.

Doc comments are a **source-level** annotation. They are NOT part of any
wire format (JSON, UEBA), they do NOT participate in evolution diffs or
conversions, they do NOT contribute to schema digests
(`ShallowSchemaId` / `DeepSchemaId`), and they do NOT affect codec
generation. Two `.baboon` files differing only in their doc comments
produce byte-identical wire output and byte-identical schema digests.
Doc comments flow through the compiler as inert metadata so that each
backend can re-emit them as idiomatic doc comments in the generated
source.

Plain non-doc comments — `// …` line comments and `/* … */` block
comments without the second `*` — are ignored by the parser and do not
appear in the typed model or generated source (see §9).

---

## 2. Surface syntax

### 2.1 Prefix doc on type declarations

A prefix doc precedes any of the seven type-declaration kinds:
`data`, `adt`, `enum`, `contract`, `service`, `foreign`, and `type`
aliases. The doc binds to the **immediately following** declaration.
Intervening blank lines are permitted and do not break the binding;
an intervening declaration does break the binding.

```text
/** A simple non-template DTO used as a concrete type argument below. */
data Item {
  name:  str
  price: f64
}

/**
 * Paged-results shape.
 *
 * `T` is the element type carried by `items`.
 */
data Page[T] {
  items: lst[T]
  total: u32
}

/** Convenience alias for an integer page. */
root type IntPage = Page[i32] : derived[json], derived[ueba]
```

The example uses `data` and `type`; the same prefix-doc form applies
uniformly to `adt`, `enum`, `contract`, `service`, and `foreign`
declarations.

### 2.2 Prefix doc on service methods

A prefix doc inside a `service` body binds to the immediately following
`def` declaration:

```text
service Crud[K, V] {
  /** Fetch a value by key. */
  def get (K): V

  /** Store a value and return the assigned key. */
  def put (V): K
}
```

### 2.3 Prefix doc on fields

A prefix doc inside a `data`, ADT-arm DTO, or `contract` body binds to
the immediately following field declaration:

```text
data Item {
  /** Display name of the item. */
  name:  str
  /** Unit price in store currency. */
  price: f64
}

adt Envelope[T, E] {
  data Ok  {
    /** The successful payload. */
    value: T
  }
  data Err {
    /** The error description. */
    error: E
  }
}
```

### 2.4 Postfix line doc on fields

A postfix `//!` line doc is **field-only**: it appears at the end of a
field-definition line and binds to that same field. The marker
terminates at the source line's end.

```text
data User {
  id:   uid  //! the user id
  name: str  //! display name
}
```

Postfix `//!` is **not accepted** on type declarations, on service
methods, on ADT branch headers, on enum members, or on any other
position. A `//!` outside a field-definition line is a parser error
(or, equivalently, fails to match because the postfix-doc rule is
anchored to the end of `fieldDef`).

### 2.5 Combining prefix and postfix on a field

A field may carry both a prefix doc and a postfix doc; both are kept
on the typed `Field` as two distinct cleaned strings.

```text
data User {
  /** The unique user identifier, assigned at registration time. */
  id: uid  //! never reused after deletion
}
```

The combining semantics in emission output are backend-specific
(see §7).

### 2.6 Doc-comment delimiters and what does *not* parse as a doc

The grammar is exact about which delimiter sequences open a doc
comment:

- `/**` opens a prefix doc; `*/` closes it. Only the literal three-
  character sequence `/**` qualifies. `/*` followed by anything other
  than `*` opens a regular block comment, which is ignored.
- `//!` opens a postfix doc; the marker terminates at end-of-line. Only
  the literal three-character sequence `//!` qualifies. `//` followed
  by anything other than `!` opens a regular line comment, which is
  ignored.
- The degenerate sequences `/** */`, `/**/`, and `/**\n*/` (a `/**`
  opener immediately followed by `*/` with only whitespace or no body
  between them) are recognised as **empty doc comments** by the
  parser. Per the Q4c lock (§5.4) the cleanup pass observes that the
  cleaned body is empty and silently drops the doc; no diagnostic is
  produced and the carrier node carries no `Docs` for that slot.

The intent is that there is exactly one rule for each marker, and
edge cases at the boundary ("did `/**/` open a doc?") resolve into the
silently-dropped empty-doc path rather than into a separate diagnostic
class.

---

## 3. Where doc comments are accepted

### 3.1 Prefix doc positions

A prefix `/** … */` doc is grammar-anchored at the following positions:

- before any **top-level type declaration** inside any namespace:
  `data`, `adt`, `enum`, `contract`, `service`, `foreign`, `type`
  alias;

  Plain non-template aliases (`type Y = X` where the RHS is not a
  template instantiation) do not appear in the typed model as a
  standalone `DomainMember.User`; a doc on such an alias is silently
  dropped, since it has no emission carrier. This is consistent with
  the existing typer behaviour where plain aliases are resolved
  transparently. Only template-instantiation aliases (`type Y = X[Foo]`,
  where `X` is a template) materialise a synthesized concrete type that
  carries the merged doc per §6.
- before any **ADT branch declaration** inside an `adt` body
  (`data Ok { … }`, `data Err { … }`);
- before any **service method declaration** (`def name (…): …`)
  inside a `service` body;
- before any **field declaration** inside a `data` body, an ADT-branch
  DTO body, or a `contract` body.

### 3.2 Non-positions for prefix docs

The following positions do **not** accept a prefix doc and a `/** … */`
appearing there is either a parser error or a doc that fails to bind
to anything:

- the model header (`model x.y.z`) — there is no carrier node;
- the version header (`version "x.y.z"`) — same;
- a namespace opener (`ns x { … }`) — namespaces are scoping
  constructs without their own metadata carrier;
- an `import` clause;
- inside a `foreign` body, on individual per-language mapping lines
  (`scala = "…"`, `cs = "…"`, etc.) — only the `foreign` declaration
  itself accepts a prefix doc, not its language-mapping entries;
- on individual **enum values** — only the `enum` type as a whole
  accepts a prefix doc; per-enum-value docs are deferred (see §9);
- **ADT inheritance arms** (`+ Ref`, `- Ref`, `^ Ref`) do NOT accept
  prefix docs. They are not declarations — they are structural
  composition operators on the parent ADT. A doc above an inheritance
  arm is silently dropped — the doc has no carrier in the typed model
  since inheritance arms are resolved structurally during ADT
  inheritance expansion and do not appear as standalone members. No
  parser diagnostic is produced.

### 3.3 Postfix doc position

A postfix `//!` doc is accepted **only** at the end of a field-
definition line, before the line's terminating newline. Fields are
newline-separated by the existing grammar, so a postfix `//!` cannot
straddle a multi-field expression. Postfix `//!` in any other position
is rejected by the grammar (the rule does not match at that position).

---

## 4. Stacked prefix doc comments (Q3 lock)

Two `/** … */` blocks back-to-back with no intervening declaration are
a **parser error**:

```text
/** First block. */
/** Second block. */
data Foo {
  x: i32
}
```

This source produces `ParserIssue.StackedDocComments(pos)`, where
`pos` is the position of the **second** block. The diagnostic message
recommends merging the two blocks into one coherent prefix doc.

The legal alternatives are either to merge the two blocks into a
single multi-paragraph `/** … */` (paragraphs separated by a blank
line), or — when the two blocks should bind to two different
declarations — to interpose the second declaration between them.

**Rationale.** Stacked prefix docs raise an ambiguity ("which block is
the doc?") that cannot be resolved by a local rule without privileging
one block over the other. Forcing a single coherent block per
declaration removes the ambiguity at parse time and matches the
practice of every emission target listed in §7. The Q3 lock chose the
stricter rule precisely so that the question "which doc binds?" has a
single mechanical answer: there is at most one prefix doc per
declaration in the raw AST, by construction.

The corresponding raw-AST shape carries this invariant. Per the plan
(§6 Q3 lock):

```scala
case class RawDocs(prefix: Option[RawDocComment], suffix: Option[RawDocComment])
```

`prefix` is `Option`, not `List` — there is no representation in the
typed model for a stacked prefix.

(Postfix `//!` is single-line by construction, so the analogous
question — "may two postfix docs stack on one field?" — does not
arise. A field has at most one terminating newline and therefore at
most one trailing `//!`.)

---

## 5. Doc-body cleanup rule (Q4 locks)

### 5.1 Ownership

Doc-body cleanup is performed once, in the typer, by a single
canonical function `DocFormat.clean(raw: String): String`. The typed-
side `DocComment` carries both the original raw text and the cleaned
text:

```scala
case class Docs(prefix: Option[DocComment], suffix: Option[DocComment])
case class DocComment(raw: String, cleaned: String)
```

Backends consume `cleaned`. When a backend's emission target requires
escaping (XML for C#, HTML for Java's Javadoc, `"""` for Python or
GraphQL block strings), the backend applies that escaping at emission
time on top of `cleaned`. The escaping is deliberately **not** baked
into `cleaned` because the cleaned form is intended to be the canonical
plain-text representation shared across all backends; per-language
escaping is the responsibility of each backend's renderer.

### 5.2 Cleanup algorithm for prefix `/** … */`

Given the raw byte sequence between the opening `/**` and the closing
`*/` (exclusive of both delimiters), the cleanup function performs the
following steps in order:

1. **Strip the delimiters.** The opening `/**` and the closing `*/`
   are removed. The body is what remains.
2. **Split on line boundaries.** Use the source-file line separator
   (`\n`, with `\r\n` collapsed to `\n` first).
3. **Strip the common leading prefix.**
   a. **Identify content lines.** A *content line* is an interior line
      that contains at least one character beyond what the conventional
      `\s*\*\s*` Javadoc separator supplies. Concretely, a line that
      matches `\s*\*?\s*$` end-to-end (whitespace-only, or whitespace
      followed by an optional `*` followed by whitespace, with no
      further content) is a *separator line* and is **excluded** from
      prefix computation. Purely blank lines are also excluded.
   b. **Compute the common prefix.** Compute the longest string of the
      form `\s*\*?\s*` (any leading whitespace, an optional single `*`,
      and optionally further whitespace) that is a prefix of every
      content line. If there are no content lines the body is treated
      as empty and falls through to step 5's collapse rule.
   c. **Strip the prefix from every line.** Apply the computed prefix
      as a prefix-strip to every interior line, including separator
      lines. A separator line that is shorter than the computed prefix
      is reduced to the empty string (no index-past-end error). This
      removes the conventional `*` continuation marker on each line of
      a multi-line Javadoc-style block.
4. **Right-trim each line.** Trailing whitespace at the end of each
   line is removed.
5. **Collapse leading and trailing blank lines.** Any number of fully-
   blank lines at the very start or very end of the body is removed.
6. **Preserve internal blank lines.** Blank lines between non-blank
   lines are kept verbatim, treated as paragraph separators.

The result is a single `String` with `\n` line separators, no leading
or trailing blank lines, no trailing whitespace per line, and no
Javadoc continuation markers.

**Bodies without `*` continuation markers.** The same algorithm
handles the common TS / Kotlin / Rust style where interior lines carry
no `*` prefix. The `\*?` term in the prefix pattern contributes
nothing, and the common prefix degenerates to common leading
whitespace. For example:

```text
/**
  text without star
  more text
  */
```

Step 3b finds the common prefix `  ` (two spaces) over the two content
lines; step 3c strips it from each. Output:

```text
text without star
more text
```

### 5.3 Cleanup algorithm for postfix `//! …`

Given the raw byte sequence after the `//!` marker and before the
end-of-line:

1. Strip the `//!` marker.
2. Strip a single optional leading space (so that the conventional
   `//! the user id` form yields `the user id`, not ` the user id`).
3. Right-trim trailing whitespace.

Postfix docs are single-line by construction; there is no paragraph
structure to preserve.

### 5.4 Empty / whitespace-only docs

If `clean(raw)` returns an empty string or a string that is purely
whitespace, the doc is **silently dropped**: the carrier node carries
no `Docs` entry for that slot and no diagnostic is produced. This rule
applies uniformly to prefix and postfix docs, and to the degenerate
empty-doc forms `/**/`, `/** */`, and `/**\n*/` mentioned in §2.6.

### 5.5 Worked example

Input:

```text
/**
 *  First paragraph.
 *  Continued.
 *
 *  Second paragraph.
 */
```

Step 1 strips the delimiters. Step 2 splits on line boundaries,
giving four interior lines:

```text
 *  First paragraph.
 *  Continued.
 *
 *  Second paragraph.
```

Step 3a classifies them: the third line (` *`) matches `\s*\*?\s*$`
end-to-end — it is a separator line and is excluded from prefix
computation. Steps 3b–3c compute the common prefix ` *  ` (one space,
asterisk, two spaces) over the three content lines, then strip it from
every interior line including the separator. The separator line ` *`
is two characters long and is entirely consumed by the strip, leaving
the empty string. Steps 4–6 right-trim each line and collapse the
leading and trailing blank lines. Output:

```text
First paragraph.
Continued.

Second paragraph.
```

Interior whitespace beyond the common leading prefix is preserved
verbatim — step 3 strips only the common leading prefix.

**Edge case — separator shorter than the common prefix.** If a
separator line is shorter than the computed prefix (e.g. the line is
`*` while the prefix is ` *  `), the strip consumes only as many
characters as are present; the line becomes the empty string. No
index-past-end error occurs. This is the same rule stated in step 3c
above.

---

## 6. Template monomorphisation interaction (Q1 lock)

This section specifies how doc comments propagate through the M29
template-monomorphisation pipeline. See `docs/spec/generics.md` §3 for
the monomorphisation rule itself.

### 6.1 Statement of the rule

Given a template `X[T1, …, Tn]` and an alias `type Y = X[A1, …, An]`,
the synthesized concrete type emitted under the identity `Y` carries:

- **Type-level doc** = alias-doc, a blank line, then template-doc when
  both are present; alias-doc alone when only the alias has a doc;
  template-doc alone when only the template-type has a doc; no doc
  when neither has one. The separator is exactly one blank line (a
  single `\n\n` join in the cleaned string representation).
- **Field-level docs** = the docs declared on the corresponding fields
  in the template body, propagated **verbatim**. Aliases never carry
  field-level docs because the alias surface form has no field
  positions.

### 6.2 Worked examples

Template with field doc only (alias has no doc, template type has no
doc):

```text
data Box[T] {
  /** the carried element */
  value: T
}

type IntBox = Box[i32]
```

Synthesized:

```text
data IntBox {
  /** the carried element */
  value: i32
}
```

Both alias-doc and template-doc present:

```text
/** Paged-results shape, generic over the element type. */
data Page[T] {
  /** the elements on this page */
  items: lst[T]
  /** total elements across all pages */
  total: u32
}

/** A page of integers. */
type IntPage = Page[i32]
```

The synthesized `IntPage` carries the merged type-level doc:

```text
A page of integers.

Paged-results shape, generic over the element type.
```

and the two field docs propagated verbatim from the template body.

**Alias-doc only.** `data Bag[T] { items: lst[T] }` plus
`/** Bag of integers. */ type IntBag = Bag[i32]` synthesizes `IntBag`
with the alias's doc as its type-level doc and no field docs (the
template body has none).

**Neither.** Synthesized type carries no type-level doc; field docs
are still propagated from the template body if any exist there.

### 6.3 Implementation note

The doc merge is performed at template-substitution time on the
synthesized `RawTLDef`'s `RawNodeMeta`, before the substituted body
re-enters the typer's per-declaration conversion path. This places the
merge inside `TemplateInstantiator` (introduced in PR-29.5) and means
that downstream code — `BaboonTranslator.convert*`, validators, and
all backends — sees the merged doc as if the user had hand-written
the materialised concrete type with that doc directly.

---

## 7. Per-backend emission idioms (locked)

This is the locked taxonomy of per-backend emission shapes for doc
comments. Future backends MUST pick from this taxonomy unless the
spec is amended. Each subsection states the emission shape and shows a
short example for the type-level case; field-level and method-level
emission follows the same shape with the symbol changed.

### 7.1 Scala

Javadoc-style `/** … */` with `*` line prefix, immediately before the
symbol.

```scala
/**
 * A page of integers.
 */
final case class IntPage(items: List[Int], total: Int)
```

### 7.2 Java

Javadoc `/** … */` immediately before the symbol. Body text is HTML-
escaped: `<` → `&lt;`, `>` → `&gt;`, `&` → `&amp;` to preserve the
text rendered through `javadoc`. Emission shape otherwise identical to
§7.1.

### 7.3 Kotlin (incl. KMP)

KDoc `/** … */` immediately before the symbol; both the JVM and
Kotlin/KMP translators emit the same form. Shape identical to §7.1.

### 7.4 TypeScript

Javadoc `/** … */` immediately before the symbol. Shape identical to
§7.1.

### 7.5 C#

XML doc comments via `///`. The first paragraph of the cleaned doc is
emitted inside `<summary>…</summary>`; subsequent paragraphs are
emitted inside `<remarks>…</remarks>`. Body text is XML-escaped:
`<` → `&lt;`, `>` → `&gt;`, `&` → `&amp;`, `"` → `&quot;`.

```csharp
/// <summary>A page of integers.</summary>
/// <remarks>
/// Optional second paragraph appears here.
/// </remarks>
public sealed class IntPage { /* … */ }
```

### 7.6 Python

Class-level and method-level doc comments are emitted as PEP 257
docstrings — a triple-quoted string literal as the first statement of
the class or method body.

**Field docs are NOT emitted as per-field statements.** Per the Q2
lock, field docs are folded into the class-level docstring as a
Sphinx/Google-style `Attributes:` section keyed by field name. ADT-arm
DTOs follow the same shape: each arm class folds its own arm-field
docs into its own arm-class docstring.

```python
class IntPage:
    """Paged integers.

    Attributes:
        items: the elements on this page
        total: total elements across all pages
    """
```

Continuation lines for multi-paragraph field docs indent by 8 spaces
(4 for the `Attributes:` block + 4 for continuation), per Sphinx and
Google docstring conventions. Triple-quote sequences `"""` inside the
doc body are escaped to `\"\"\"` to keep the docstring well-formed.

### 7.7 Rust

Rust uses `///` outer-line doc comments before the item. Rust
distinguishes outer-line `///` from module-inner `//!`; the `//!`
module-inner form **is not used in generated output** because it would
mis-attribute the doc to the enclosing module rather than to the
following item.

A baboon postfix `//!` on a field is therefore mapped to a `///` line
**before** the field in Rust output:

```rust
/// A page of integers.
pub struct IntPage {
    /// the elements on this page
    pub items: Vec<i32>,
    /// never reused after deletion
    pub total: u32,
}
```

### 7.8 Dart

Dart `///` doc comments immediately before the symbol. One `///` line
per cleaned doc-body line.

### 7.9 Swift

Swift `///` doc comments (apple-doc default) immediately before the
symbol. Same shape as §7.8.

### 7.10 GraphQL SDL

GraphQL block-string descriptions (`"""…"""`) immediately before the
type, field, or argument. Embedded `"""` sequences inside the doc body
are escaped to `\"\"\"` per the GraphQL specification.

```graphql
"""A page of integers."""
type IntPage {
  """the elements on this page"""
  items: [Int!]!
  """total elements across all pages"""
  total: Int!
}
```

### 7.11 OpenAPI 3.1

The `description` JSON Schema key on the component, field, or
parameter. The cleaned doc body is emitted as a plain JSON string
(JSON-escaped); no Markdown normalisation is applied by the compiler
(see §9). Type-level docs land on the component schema's
`description`; field-level docs land on each property's
`description`.

---

## 8. Diagnostics introduced

M30 introduces a single new parser diagnostic. No new `TyperIssue`,
`VerificationIssue`, `EvolutionIssue`, `IOIssue`, `TranslationIssue`,
or `RuntimeCodecIssue` cases are introduced.

### 8.1 `ParserIssue.StackedDocComments(pos)` (PR-30.2)

Two `/** … */` prefix doc blocks back-to-back with no intervening
declaration. The diagnostic cites the position of the **second**
block. The recommended message form: "Stacked prefix doc comments at
`pos` — merge into a single `/** … */` block per declaration."

The case-class shape mirrors the convention already established in
`baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/ParserIssue.scala`:

```scala
case class StackedDocComments(pos: InputPointer) extends ParserIssue
```

A corresponding `IssuePrinter[StackedDocComments]` instance lives
alongside the existing `parserFailedPrinter` and
`includeNotFoundPrinter`.

### 8.2 Audit obligation: parser-issue exhaustive matches

`ParserIssue` is a sealed trait. Three files in the compiler contain
exhaustive `match` expressions over `ParserIssue` cases (verified by
inspection of the current tree), with a total of four match sites:

- `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/DiagnosticsProvider.scala`
  (the `convertToDiagnostic` body, `BaboonIssue.Parser(pi)` arm);
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/state/WorkspaceState.scala`
  (the `extractInputPointer` body, L81–87);
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/state/WorkspaceState.scala`
  (the `formatIssue` body, L207–211);
- `baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala`
  (the `extractIssuePointer` body, JS-side error-formatting path).

PR-30.2 must add the `StackedDocComments` arm to every exhaustive
`ParserIssue` match in each of the three files. Bundle all arms in a
single edit per file even when a file carries multiple matches (e.g.
`WorkspaceState.scala` contains two such matches). This mirrors the
M29 3-site exhaustive-match discipline documented in `CLAUDE.md`
("PR-29.7 / PR-29.4 / PR-29.5 canonical examples").
`mdl :build :test` runs `sbt +compile`, which cross-builds JVM and
Scala.js with `-Wconf` settings that promote inexhaustive-match
warnings to errors; a missed JS-side update is caught by that path.
The single new case (`StackedDocComments`) maps to four arms total
across three files.

---

## 9. Out of scope

The following items are deliberately **NOT** delivered in M30 and will
require separate milestones / decisions:

1. **Tree-sitter editor-grammar updates** for doc-comment recognition.
   The grammar changes live in a 3-level submodule chain
   (`editors/baboon-zed` → `grammars/baboon`) that requires user-
   authorised pointer bumps across separate git repositories. Deferred
   per the `[PR-29.8-D01]` precedent.
2. **Preserving non-doc comments** (`// …` line comments and
   `/* … */` block comments without the second `*`). Non-doc comments
   are ignored by the parser and do not appear in the typed model.
3. **Per-enum-value docs.** Only the `enum` type as a whole accepts a
   prefix doc in M30; per-value docs are deferred.
4. **Sub-statement / mid-expression doc placement.** Docs are
   anchored at a small set of grammar positions (§3); doc placement
   inside a type expression (`lst[/** elem */ T]`) or between an
   identifier and its type (`x: /** wat */ i32`) is not in scope.
5. **Markdown / RST / structured-text normalisation** of doc body
   content beyond the cleanup rule of §5. The cleaned doc body is
   plain text; backends emit it as-is (with per-language escaping per
   §7).
6. **Wire-format changes.** Docs do not enter the JSON wire form, the
   UEBA wire form, or the schema-digest computation
   (`ShallowSchemaId` / `DeepSchemaId`). Two `.baboon` files that
   differ only in their doc comments produce byte-identical wire
   output and byte-identical digests; this is verified as a cross-
   cutting concern in PR-30.3 (see plan §5.4).

---

## 10. Implementation pointers

PR-30.2 (parser), PR-30.3 (typer + domain model), PR-30.4 .. PR-30.13
(per-backend emitters, one PR per language), PR-30.14 (LSP hover),
and PR-30.15 (cross-language smoke fixture `m30-ok` + close-out)
implement this specification. The per-PR breakdown lives in
`docs/drafts/20260504-1213-m30-docstrings-plan.md` §4.

Critical files for the implementer (paths relative to repository
root):

- parser: `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/{defns/{DefMeta,DefDto}.scala, model/RawNodeMeta.scala, model/issues/ParserIssue.scala}`
- typer / domain: `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/{BaboonTranslator,TemplateInstantiator}.scala`, `typer/model/{Typedef,DomainMember}.scala`
- per-backend translators: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/<lang>/{*DefnTranslator,*TreeTools}.scala`
- LSP / JS exhaustive matches (§8.2 audit): `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/DiagnosticsProvider.scala`, `lsp/state/WorkspaceState.scala`, `baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala`
- LSP hover (PR-30.14): `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/HoverProvider.scala`

---

## 11. PR-origin diagnostic table

| Diagnostic                         | Section ref | Origin PR  |
|------------------------------------|-------------|------------|
| `ParserIssue.StackedDocComments`   | §4, §8.1    | PR-30.2    |

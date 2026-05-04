# Clarifications: M30 docstring/comment preservation

**Context:** Planning M30 (docstring preservation across parser → typer →
all 11 backends). Plan doc: `docs/drafts/20260504-1213-m30-docstrings-plan.md`.
Four design decisions are surface-syntax / semantic choices that need
your sign-off before PR-30.1 (spec) freezes them.

**How to answer:** Write your response on the `Answer:` line under each
question. Leave a question blank if you want to skip it. Reference by
ID (`Q1`…`Q4`) in chat if convenient.

---

## Q1: Template doc propagation rule

When `type Y = X[Foo]` materialises a concrete DTO from `data X[T] { … }`,
which docs end up on the synthesized DTO and on its fields?

**Context:** This is the M29 monomorphisation interaction. The synthesized
DTO has *one* type-level doc slot and *one* doc slot per field. The
template carries docs on its type declaration (`/** … */ data X[T] { … }`)
and on its fields (`/** … */ value: T`). The alias also carries docs
(`/** … */ type Y = X[Foo]`). They have to be merged or one of them has
to win.

**Suggestions:**

- **A (recommended)** — alias-overrides-template-type for the *type-
  level* doc; template field docs propagate verbatim to the materialised
  DTO; if the alias has no docs, the template's type-level doc is the
  fallback. Rationale: aliases are the user's named handle; if the user
  documents the alias they intend that doc to describe the resulting
  concrete type. Field docs belong to the template body and are not
  duplicated at the alias call-site, so they propagate as-is.
- **B** — concatenate alias doc + template-type doc on the synthesized
  type; template field docs propagate verbatim. More information, but
  awkward to read and produces double-headers in generated source.
- **C** — alias docs are the *only* doc on the synthesized type
  (template-type docs are dropped unconditionally even when the alias
  has none); template field docs propagate verbatim. Simpler, but
  templates effectively cannot self-document.
- **D** — template-type docs always win; alias docs are dropped.
  Keeps the template as the single source of truth, but contradicts
  M29's locked decision #4 (alias id is canonical) — the alias is the
  monomorphisation identifier, so it's natural to also own the doc.

Answer:

---

## Q2: Python field-doc convention

Python has no first-class field-docstring syntax. How should the Python
backend render field docs?

**Context:** Class-level and method-level docstrings are unambiguous —
PEP 257 standard `"""…"""` as the first statement. The question is field
docs only. The choice affects downstream Python tooling: IDE hover, Sphinx
`autodoc`, `pydantic`'s `Field(..., description=…)`, dataclass field
metadata, etc.

**Suggestions:**

- **A (recommended)** — PEP-257 attribute docstring: emit a separate
  `"""…"""` string-literal statement immediately following the
  assignment / annotation. Picked up by Sphinx `autodoc` and by most
  modern IDEs (PyCharm, pyright). Idiomatic but slightly unusual.
- **B** — `# field: docstring` line comment immediately above the
  field. Universal but invisible to autodoc / hover.
- **C** — embed the doc into a `Field(..., description="…")` call (or
  `dataclass.field(metadata={"description": "…"})`). Couples docs to a
  specific runtime library; the current Python codegen uses pydantic
  for codecs but the data-class shape is plain — a runtime-coupling
  change is broader than a comment-emission change.
- **D** — emit both A and B (belt-and-braces). Verbose; risks the two
  drifting if any later edit touches one.

Answer:

---

## Q3: Stacked prefix-doc accumulation

If the user writes two `/** … */` blocks back-to-back with no
intervening declaration, what does the parser do?

```
/** First paragraph */
/** Second paragraph */
data Foo { … }
```

**Context:** Anchored-rule design (plan §2.1) lets us either accumulate
both into a list or take only the last one. The choice affects parser
state and how the typed `Docs.cleaned` is composed.

**Suggestions:**

- **A (recommended)** — accumulate as an ordered `List[RawDocComment]`;
  `Docs.cleaned` joins paragraphs with a blank line. Mirrors most IDE
  behaviour; lets users compose docs incrementally.
- **B** — last-block-wins (single `Option[RawDocComment]`). Simpler,
  but silent data loss if the user actually meant both.
- **C** — first-block-wins. No clear rationale; unusual.
- **D** — emit a parser warning and reject. Strictest; pushes users
  to merge into a single block but breaks ergonomics for tools that
  insert auto-generated headers.

Answer:

---

## Q4: Cleanup-time policy (raw vs. cleaned)

When does the parser/typer strip the `/**` / `*/` delimiters and the
leading `*` indentation from doc bodies?

**Context:** Two reasonable points: (a) parse-time keeps raw text in
`RawDocComment.raw` and clean once at typer-stage into
`DocComment.cleaned`; backends consume `cleaned`. (b) Always-raw —
each backend cleans on its own.

**Suggestions:**

- **A (recommended)** — clean once at typer stage; `DocComment` carries
  both `raw` and `cleaned`; backends consume `cleaned` and may further
  escape per language (XML escape for C#, `"""` escape for Python /
  GraphQL, HTML escape for Java). Single canonical cleanup; per-language
  escaping stays small.
- **B** — keep raw at all stages; each backend cleans inline.
  Maximum flexibility per backend (e.g. a backend could preserve `*`
  prefixes verbatim in its own style), but duplicates cleanup logic 11×
  and risks subtle per-language drift.
- **C** — clean at parse time and discard raw. Smaller AST but loses
  fidelity for any future feature (e.g. a hypothetical "format-
  preserving rewrite" tool).

Answer:

---

## Q4-elab: what does "cleanup" actually do?

You asked for elaboration. Q4 is really two questions tangled together:
*who owns the cleanup logic* (Q4a) and *what does the canonical cleaned
form look like* (Q4b). Splitting them.

### Q4a: ownership of cleanup logic

**Recommended: A from the original Q4** — one cleanup function in the
typer (`DocFormat.clean(raw: String): String`), `DocComment(raw,
cleaned)` carries both, backends consume `cleaned` and apply per-
language escaping (XML escape for C#, `"""` escape for Python /
GraphQL, HTML escape for Java). Reasoning: 11 backends × 11 cleanup
implementations is a guaranteed drift surface; one function tested
once is the simpler shape.

**Alternative B** — store raw only, each backend cleans inline.
Picks up flexibility (a backend could choose to preserve `*` prefixes
verbatim) at the cost of duplicating one ~30-line function 11×.

**Alternative C** — clean at parse time, discard raw. Smaller AST,
but no recovery if any future tool needs the original spelling.

### Q4b: canonical cleaned form

Given input

```
/**
 *  First paragraph.
 *  Continued.
 *
 *  Second paragraph.
 */
```

what does `cleaned` contain?

- **Option 1 (recommended)** — strip `/**` and `*/`, strip the common
  leading-whitespace + `*` prefix on each interior line, normalise
  trailing whitespace, collapse leading/trailing blank lines, preserve
  internal blank lines as paragraph separators. Result for the example:
  ```
  First paragraph.
  Continued.
  
  Second paragraph.
  ```
  This is what Scaladoc / Javadoc / KDoc tools see. Backends that need
  Javadoc form re-wrap with `*` line prefix at emission time;
  backends that need raw paragraphs (Python, GraphQL, OpenAPI) consume
  this directly.

- **Option 2** — strip only `/**` and `*/`, leave interior `*` prefixes
  intact. Simpler cleanup but every non-Javadoc backend (Python,
  GraphQL, OpenAPI, Rust `///`, Dart `///`, Swift `///`) has to
  re-strip on emission — defeating Q4a.

- **Option 3** — strip everything, collapse all whitespace to single
  spaces. Loses paragraph structure; bad for any backend that respects
  paragraph breaks.

For postfix `//! …` field comments: cleanup just strips the `//!`
marker and a single optional leading space. Result is the trimmed
inline text (no multi-paragraph case for postfix).

### Q4c: what about empty / whitespace-only doc bodies?

- `/** */` and `/**\n*/` and `/**\n* \n*/` — should these (a) be
  rejected as a parser error (degenerate doc), (b) be silently
  dropped (no `Docs` attached), or (c) be preserved as an empty doc?
  Recommended **(b) silently drop** — degenerate but harmless; matches
  Scaladoc/Javadoc tooling behaviour.

Answer Q4a: A (recommended) — typer-stage cleanup; `DocComment(raw, cleaned)`; backends consume `cleaned` + apply per-language escaping.

Answer Q4b: Option 1 (recommended) — strip `/**` / `*/` and common leading `*` prefix; preserve paragraph breaks via blank lines; collapse leading/trailing blank lines; trim trailing whitespace per line.

Answer Q4c: (b) silently drop empty / whitespace-only doc bodies.

---


---

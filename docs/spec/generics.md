# Generics — User-Defined Templates (M29 / BAB-A04)

**Status:** authoritative specification for user-defined generic templates in
the Baboon DML. Implementation is delivered across PR-29.2 .. PR-29.11 of
milestone M29; the per-PR breakdown lives in
`docs/drafts/20260503-2210-m29-generics-plan.md` and `tasks.md`
("Milestone M29 — PR breakdown"). The six locked design decisions and the
nine-case negative-test matrix referenced throughout this document are
user-blessed (2026-05-03) and reproduced verbatim from `tasks.md`.

This document is the single source of truth. When the compiler and this
document disagree, the document wins and the compiler is wrong.

---

## 1. Overview

A **template** is a `data`, `adt`, `contract`, or `service` declaration that
takes one or more type parameters in square brackets after its name:
`data Page[T] { ... }`, `adt Result[T, E] { ... }`, `contract Acked[T] { ... }`,
`service Crud[T] { ... }`. Templates are not types; they are recipes for
producing types.

A template is **instantiated** by referencing it on the right-hand side of a
type alias: `type IntPage = Page[i32]`. Instantiation is the **only** way to
produce a usable type from a template — instantiations in any other position
(field types, ADT inheritance arms, nested in alias RHSes, …) are rejected at
compile time.

Every reachable instantiation is **monomorphised** at compile time into an
ordinary concrete type whose identity is the alias's name. After
monomorphisation the typed model contains no templates and no parameterised
references — only ordinary `data` / `adt` / `contract` / `service`
declarations indistinguishable from hand-written equivalents. Codegen,
JSON wire form, UEBA wire form, evolution, and conversion all operate on
the monomorphised concrete types and have no awareness that templates ever
existed.

Users gain the ability to write a single generic shape (such as a paged
collection or a request envelope) once, and instantiate it for as many
element types as the model needs, without copy-pasting the structure.

---

## 2. Surface syntax

### 2.1 Template declarations

A template adds a `[T1, T2, ...]` clause between the type's name and its
body. The four declaration kinds support templating uniformly (locked
decision #1).

#### `data` template

```text
data Page[T] {
  items: lst[T]
  total: u32
}
```

#### `adt` template

```text
adt Result[T, E] {
  data Ok  { value: T }
  data Err { error: E }
}
```

#### `contract` template

```text
contract Acked[T] {
  payload: T
  ack:     bit
}
```

#### `service` template

```text
service Crud[T] {
  def create (in: T): T
  def read   (in: uid): T
  def delete (in: uid): bit
}
```

#### Arity greater than one

The bracketed clause may carry any positive number of type parameters
separated by commas. Example with two parameters:

```text
data Envelope[T, E] {
  payload: opt[T]
  error:   opt[E]
}
```

### 2.2 Type-parameter naming

A type parameter is a bare identifier. The grammar is the same as for any
other identifier in the language: a letter or `_` followed by any number of
letters, digits, or `_`. Convention is to use a short uppercase identifier
(`T`, `E`, `K`, `V`), but the parser does not enforce a casing rule; the
compiler treats type-parameter names as opaque labels scoped to the
template's body.

### 2.3 Type-parameter scoping

A type parameter `T` declared on a template `X[T]` is in scope **only inside
the body of `X`**. It may appear in any position where an ordinary type
reference may appear inside that body — field types, collection element
types (`lst[T]`, `set[T]`, `opt[T]`, `map[str, T]`), the underlying type
of an `any[T]` builtin (which is itself substituted at monomorphisation),
nested DTO/ADT member field types within the template's body, and method
argument / return / error positions for service templates.

A type parameter may not appear:

- in any sibling or unrelated declaration's body (not in scope);
- in another template's body, even if both templates use the same letter
  (each template has its own independent type-parameter scope);
- as the head of a constructor (`T[Foo]`) — type parameters are not
  themselves templates;
- inside the bracketed clause of a *nested* template declaration, because
  M29 does not permit nesting one template declaration inside another at
  any structural position.

Within a template body, a type-param name shadows any same-named top-level
type. Outside the template body, the top-level type is visible normally.

### 2.4 Instantiation (alias-only)

A template is instantiated **only** as the right-hand side of a type alias
(locked decision #3):

```text
type IntPage = Page[i32]
type StrPage = Page[str]
type EnvelopeStrInt = Envelope[str, i32]
```

The alias's name becomes the identity of the materialised concrete type
(see §3). The type arguments inside the brackets must themselves be
ordinary type references (primitives, user types, type aliases that
resolve transitively to ordinary types) — they may not themselves be
template instantiations (see §2.5, forbidden case 2).

### 2.5 Forbidden positions

The following constructions are rejected at compile time. Each item maps
1:1 to an entry in the locked negative-test matrix (`tasks.md`,
"Milestone M29 — PR breakdown", "Negative test matrix (locked)"). The
exact diagnostic strings are finalised in PR-29.7; the names below
identify the matrix item the spec rejects.

#### 2.5.1 Instantiation in field position (matrix #1)

A template instantiation may not appear as a field type:

```text
data Holder {
  page: Page[i32]   // REJECTED — instantiation outside an alias RHS
}
```

The legal alternative is to introduce an intermediate alias:

```text
type IntPage = Page[i32]

data Holder {
  page: IntPage
}
```

#### 2.5.2 Nested instantiation in alias RHS (matrix #2)

A type argument inside an instantiation may not itself be a template
instantiation:

```text
type Bad = Page[Page[i32]]   // REJECTED — nested instantiation
```

The legal alternative is to introduce an intermediate alias for the inner
type:

```text
type IntPage   = Page[i32]
type PageOfPages = Page[IntPage]
```

Note that wrapping a type parameter in a builtin collection (`lst[T]`,
`opt[T]`, `map[str, T]`, `set[T]`, `any[T]`) is **not** considered nested
instantiation — builtins are not templates. `type Bad = Page[lst[i32]]`
is legal.

#### 2.5.3 Arity mismatch (matrix #3)

The number of type arguments at an instantiation site must equal the
number of type parameters declared on the template:

```text
data X[T] { a: lst[T] }
type XInt = X[i32, str]   // REJECTED — arity mismatch (expected 1, got 2)
```

#### 2.5.4 Duplicate type-parameter name (matrix #4)

A template may not declare two type parameters with the same name:

```text
data X[T, T] { a: lst[T] }   // REJECTED — duplicate type parameter `T`
```

#### 2.5.5 Self-reference in a template body (matrix #5)

A template may not reference itself in its own body. See §4 for the full
treatment; the direct field-position case is unconditionally rejected:

```text
data X[T] {
  rec: X[T]   // REJECTED — template self-reference
}
```

#### 2.5.6 Type-parameter placeholder leak (matrix #6)

A bare identifier in a type position outside any template is resolved as
an ordinary type reference. A name that the user intended as a "free
type parameter" is therefore treated as a missing type:

```text
data Y {
  f: T   // REJECTED — `T` is not declared as a type
}
```

The diagnostic name for this case (whether the existing `NameNotFound` or
a more specific `OrphanTypeParam`) is finalised in PR-29.7.

#### 2.5.7 Template referenced without instantiation (matrix #7)

An alias whose RHS is a bare reference to a template (no bracket clause)
is rejected:

```text
data X[T] { f: T }
type Y = X      // REJECTED — template `X` referenced without instantiation
```

#### 2.5.8 Instantiating a non-generic (matrix #8)

A constructor expression `H[arg]` whose head `H` resolves to a non-template
type (a primitive, an ordinary `data`, an ordinary `adt`, an ordinary type
alias, etc.) is rejected:

```text
type Y = i32[X]   // REJECTED — `i32` is not a template
```

The existing builtin collections (`lst`, `set`, `opt`, `map`, `any`) are
not templates either, but they accept their arguments through the
established collection / `any` syntax; they are unaffected by template
instantiation rules.

#### 2.5.9 Cycle through aliases (matrix #9)

A graph of templates and instantiations whose monomorphised result would
contain a cycle of fields between concrete types is rejected:

```text
data X[T] { f: T }
type A = X[B]
type B = X[A]   // REJECTED — cycle through alias instantiations
```

The diagnostic name for this case (whether `TemplateInstantiationCycle`
or the existing `CircularInheritance`) is finalised in PR-29.6 / PR-29.7.

### 2.6 Interaction with ADT inheritance

ADT inheritance arms (`+`, `-`, `^`) operate on non-templated type
references today and are **not** widened in M29 to accept template
instantiations. The following is therefore not legal in M29:

```text
adt Mix {
  + Page[i32]   // REJECTED — instantiation in ADT inheritance arm
}
```

A template ADT (`adt X[T] { ... }`) may itself use the inheritance arms in
its own body, provided the arms reference non-templated type names; the
substitution machinery that materialises an instantiation preserves the
arms unchanged. The restriction is specifically on *instantiations* in
arm position, not on arms appearing inside template bodies.

The wider question of generics-in-arms is out of scope for M29; see §6.

---

## 3. Semantics — monomorphisation

### 3.1 Statement of the rule

Every reachable instantiation `type Y = X[T1, ..., Tn]` is materialised at
compile time into an ordinary concrete declaration whose **identity is the
alias's name `Y`**, not a synthetic name like `X<T1>` or `X_T1_..._Tn`
(locked decision #4). The materialised declaration's body is the template
`X`'s body with every occurrence of each type-parameter placeholder
replaced by the corresponding argument.

### 3.2 Worked example

Given:

```text
data Page[T] {
  items:  lst[T]
  cursor: opt[str]
}

type IntPage = Page[i32]
```

the typed model after compilation contains exactly one user type for these
declarations, equivalent to the hand-written form:

```text
data IntPage {
  items:  lst[i32]
  cursor: opt[str]
}
```

The template `Page` itself does not appear in the typed model, in any
generated source file, in the JSON wire form, in the UEBA wire form, in
any conversion code, or in any evolution diff.

### 3.3 Codegen is unchanged

Every backend (Scala, C#, Python, Rust, TypeScript, Kotlin, Java, Dart,
Swift, GraphQL SDL, OpenAPI 3.1) emits the materialised concrete type
keyed by its alias identity. No backend emits a parameterised type, a
language-level generic, or any reference to the template name. C# does
not emit `Page<int>`; it emits `IntPage`. The same holds for
Java/Kotlin/TypeScript/Rust/Swift native generics — none of them are
produced by M29.

This is the meaning of locked decision #4: monomorphisation identifier =
alias id; no synthetic identifier is exposed.

### 3.4 Distinct aliases produce distinct types

Two aliases that instantiate the same template with the same arguments
produce **two separate** concrete types, each keyed by its own alias name:

```text
data Page[T] {
  items: lst[T]
  total: u32
}

type IntPage     = Page[i32]
type AlsoIntPage = Page[i32]
```

`IntPage` and `AlsoIntPage` are distinct types in the typed model, the
generated code, the JSON wire form, the UEBA wire form, and evolution.
There is no de-duplication step. Users who want a single concrete type
should write a single alias.

### 3.5 Pipeline placement

For implementation context only: monomorphisation runs in the typer
between ADT-inheritance expansion and the second scope-build pass. By
the time the rest of the typer, the validator, and any backend sees the
model, templates have been removed from the member list and
instantiations have been replaced with concrete declarations. See
`docs/drafts/20260503-2210-m29-generics-plan.md` §3.7 for the pipeline
diagram.

---

## 4. Self-reference and DAG-only structure

The matrix-#5 worked example is at §2.5.5; this section discusses the
choice of strict vs permissive interpretation.

### 4.1 Locked rule

Locked decision #2: self-reference is forbidden; only DAG-shaped graphs
of templates and instantiations are valid.

### 4.2 Two interpretations

The phrase "self-reference" admits two readings, distinguished here for
clarity:

- **Direct field-position recursion.** A template body that names itself
  (parameterised or not) directly in a field type position:

  ```text
  data X[T] {
    rec: X[T]   // direct self-reference
  }
  ```

  This form is **forbidden unconditionally**. Materialising it would
  produce `data Y { rec: Y }` for any `type Y = X[...]`, which is direct
  recursion the type system cannot terminate.

- **Container-mediated recursion.** A template body that names itself
  through a builtin collection wrapper:

  ```text
  data Tree[T] {
    children: lst[Tree[T]]   // container-mediated self-reference
  }
  ```

  Materialising this form would produce
  `data IntTree { children: lst[IntTree] }`, which is a valid recursive
  shape that the existing non-template validator
  (`BaboonValidator.checkLoops`) already accepts for hand-written
  recursive types.

### 4.3 Default for M29: strict (Option A)

This spec defaults to the strict reading: **container-mediated recursion
within a template body is also forbidden in M29.** The rationale is the
literal wording of locked decision #2 ("only DAG-shaped"), which by the
strictest reading rejects any cycle in the template-and-instantiation
graph regardless of whether a builtin collection mediates it.

Users who need a recursive container structure can hand-write a
non-template recursive type (the existing `checkLoops` permits this for
non-template types — see `baboon-compiler/src/test/resources/baboon/pkg0/pkg03.baboon`
lines 53-61, where `RecTest1` self-references directly via an ADT branch
and `RecTest2` self-references through `opt[RecTest2]`) and reference that
from any template that needs it.

> **DESIGN NOTE:** this strict reading is the default chosen by the M29
> plan; revisit if a use case demands container-mediated recursion within
> a template body. The permissive reading (Option B) would limit the
> self-reference detector to direct field-position recursion only and
> defer container-mediated cycles to the existing `checkLoops` pass after
> monomorphisation. Switching readings is a single-site change in the
> validator plus a corresponding update to this section; the choice does
> not affect any wire-format invariant.

---

## 5. Codec annotations on aliases

M29 extends `RawAlias` to carry an optional `derived` set parsed in the
same `: name[…], …` form used for DTO/ADT declarations. Pre-M29 aliases
never carried derivation; PR-29.2/PR-29.3 introduce both the parser change
and the typer-side propagation.

### 5.1 Locked rule

Locked decision #6: `: derived[…]` is written **only on the alias**, not on
the template body. The annotation propagates to the materialised concrete
type, so the resulting type carries the same codec derivation it would
carry if the user had hand-written it with the annotation directly.

Each alias's `derived` set propagates independently to its own
materialised concrete type. Two aliases instantiating the same template
with different `derived` sets produce two distinct concrete types each
with its own derivation.

### 5.2 Worked example

```text
data Page[T] {
  items:  lst[T]
  cursor: opt[str]
}

type IntPage = Page[i32] : derived[json], derived[ueba]
```

After monomorphisation `IntPage` is equivalent to the hand-written form

```text
data IntPage : derived[json], derived[ueba] {
  items:  lst[i32]
  cursor: opt[str]
}
```

and JSON + UEBA codecs are emitted for it by every backend that supports
codec generation.

### 5.3 Annotation on a template body is rejected

A `: derived[…]` clause on a template body itself is a spec error. The
template never appears in the typed model and therefore has no opportunity
to carry a codec annotation directly; only the materialised instances
participate in codec generation, and decision #6 reserves the annotation
to the alias as the canonical site:

```text
data Page[T] : derived[json] {   // REJECTED — derivation must live on the alias
  items:  lst[T]
  cursor: opt[str]
}
```

The exact diagnostic name (`TemplateCarriesDerivation` or equivalent) is
finalised in PR-29.7.

### 5.4 `root` keyword applies only to aliases

Like `: derived[…]`, the `root` keyword applies only to aliases of
templates, not to template declarations themselves. A `root` annotation on
a template body is a spec error and the compiler will reject it.

---

## 6. Out of scope (this milestone)

The following features are deliberately **NOT** delivered in M29 and will
require separate milestones / decisions:

1. **Higher-kinded templates** (`X[F[_]]`). Substitution requires every
   type parameter to be replaced by a concrete type reference; type
   parameters that themselves take type parameters are a separate
   feature.
2. **Variance annotations** (`X[+T]`, `X[-T]`). Subtype variance is
   irrelevant to the wire format because monomorphisation produces
   concrete types.
3. **Where-clauses and type-class bounds** (`X[T : SomeContract]`).
   Constraint-bounded templates require a separate constraint-checking
   pass.
4. **Defaulted type parameters** (`X[T = i32]`). Sugar that does not
   affect the materialised wire form.
5. **Field-position instantiation** (`field: Foo[Bar]`). Forbidden by
   locked decision #3 and matrix #1.
6. **Nested instantiation in alias RHS** (`type Y = Foo[Bar[i32]]`).
   Forbidden by locked decision #3 and matrix #2.
7. **Templated identifiers** (`id Foo[T] { ... }`). Identifier fields are
   subject to a tightly controlled type set (M18) and templating opens
   wire-form questions independent of M29's monomorphisation question.
   The parser rejects `[…]` on `id` declarations.
8. **Templates on ADT inheritance arms** (`+ Foo[T]`, `- Foo[T]`,
   `^ Foo[T]`). M29 leaves `+` / `-` / `^` operating on non-templated
   names only (see §2.6).
9. **Per-language reified generics in emitted source.** No backend emits
   a generic type; every backend emits the monomorphised concrete type.
   This is the meaning of locked decision #4.
10. **Evolution of templates as templates.** Locked decision #5: only the
    materialised instances participate in `BaboonEvolution`. The
    evolution diff between two model versions is computed against each
    materialised concrete type independently, exactly as it is today for
    hand-written types. A template's body change affects every alias
    that instantiates it; the diff is observed at the alias-keyed
    concrete types, not at the template.

---

## 7. Worked end-to-end example

This section walks through a small but complete example demonstrating
two distinct alias instantiations of the same template, the resulting
materialised types, and one cross-version evolution that affects only a
materialised instance.

### 7.1 Version 1.0.0

```text
model my.example.pages

version "1.0.0"

data Page[T] {
  items:  lst[T]
  total:  u32
}

root type IntPage = Page[i32] : derived[json], derived[ueba]
root type StrPage = Page[str] : derived[json], derived[ueba]
```

After monomorphisation the typed model contains two user types, each
equivalent to a hand-written form:

```text
root data IntPage : derived[json], derived[ueba] {
  items:  lst[i32]
  total:  u32
}

root data StrPage : derived[json], derived[ueba] {
  items:  lst[str]
  total:  u32
}
```

The template `Page` does not appear. Each backend emits two distinct
concrete types named `IntPage` and `StrPage`. JSON wire form,
UEBA wire form, evolution baseline, and any conversions are all keyed by
these two names.

### 7.2 Version 1.0.1 — affecting one materialised instance only

Suppose `IntPage` needs an additional field but `StrPage` does not. The
change is expressed at the alias level by introducing an intermediate
hand-written declaration alongside the template, *not* by mutating the
template's body (which would affect every instance):

```text
model my.example.pages

version "1.0.1"

data Page[T] {
  items:  lst[T]
  total:  u32
}

// Hand-written replacement for IntPage that adds the extra field.
root data IntPage : derived[json], derived[ueba] {
  items:  lst[i32]
  total:  u32
  cursor: opt[str]
}

// StrPage continues to be derived from the template.
root type StrPage = Page[str] : derived[json], derived[ueba]
```

Evolution is computed on the materialised concrete types: the diff
between 1.0.0's `IntPage` and 1.0.1's `IntPage` is one added optional
field, exactly as it would be for any hand-written type. The diff
between 1.0.0's `StrPage` and 1.0.1's `StrPage` is empty.

The template `Page` does not participate in the evolution diff in either
direction. This is the practical meaning of locked decision #5.

---

## 8. Implementation reference

The implementation of templates is tracked in milestone M29, PRs
PR-29.2 .. PR-29.11, per
`docs/drafts/20260503-2210-m29-generics-plan.md`. See also `tasks.md`,
section "Milestone M29 — PR breakdown", for the per-PR landing slots and
their dependency order. The exact diagnostic strings emitted for each
forbidden position in §2.5 are finalised in PR-29.7; the cycle
diagnostic in §2.5.9 is finalised in PR-29.6; the codec-on-template
diagnostic in §5.3 is finalised in PR-29.7.

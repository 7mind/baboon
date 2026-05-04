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

A template may not reference itself in its own body. This is a special case
of matrix #1 (field-position template instantiation forbidden by locked
decision #3): the inner `X[T]` is a template instantiation in field position.
§4 restates this rule and shows why container-mediated forms are also rejected
on the same grounds (see §4.3).

```text
data X[T] {
  rec: X[T]   // REJECTED — template instantiation in field position (matrix #1)
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

This case fires the existing `NameNotFound` diagnostic; no new typer issue is introduced
(locked in PR-29.7 per CLAUDE.md §4 simplicity — `Type not found: T` is accurate and clear
when `T` is referenced outside any template body).

#### 2.5.7 Template referenced without instantiation (matrix #7)

An alias whose RHS is a bare reference to a template (no bracket clause)
is rejected:

```text
data X[T] { f: T }
type Y = X      // REJECTED — template `X` referenced without instantiation
```

This case fires the `TemplateNotInstantiated` diagnostic (introduced in
PR-29.7): `Alias 'Y' references template 'X' without type arguments — use 'X[…]'`.

#### 2.5.8 Instantiating a non-generic (matrix #8)

A constructor expression `H[arg]` whose head `H` resolves to a non-template
type (a primitive, an ordinary `data`, an ordinary `adt`, an ordinary type
alias, etc.) is rejected:

```text
type Y = i32[X]   // REJECTED — `i32` is not a template
```

This case fires the `NotATemplate` diagnostic (introduced in PR-29.7):
`'i32' is not a template or builtin collection — cannot be used as a
generic constructor head in alias 'Y'`.

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

The cycle is caught by `BaboonValidator.checkLoops` as
`VerificationIssue.ReferentialCyclesFound`, post-typer, post-monomorphisation.
M29 reuses this existing validator-side diagnostic rather than introducing a
dedicated `TemplateInstantiationCycle` typer-side diagnostic (CLAUDE.md §4
simplicity); after monomorphisation the cycle takes the same form as any
non-template referential cycle (`data A { f: B }; data B { f: A }`).

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

### 4.1 The governing rule

Locked decision #3 forbids template instantiation in any position other
than the right-hand side of a type alias (matrix #1). Self-reference in a
template body is therefore not a separate rule: it is a special case of
matrix #1. Whenever a template names itself inside its own body, that
occurrence is a template instantiation in field position and is rejected
on exactly those grounds.

Locked decision #2 ('Self-reference is forbidden. Only DAG-shaped
template-and-instantiation graphs are valid.') is enforced through this same
mechanism in the template-and-instantiation graph; locked decision #3
('alias-only instantiation') is the operational lever that makes the
prohibition mechanical at parse/type time.

### 4.2 Direct field-position case (matrix #5)

```text
data X[T] {
  rec: X[T]   // REJECTED — template instantiation in field position (matrix #1)
}
```

The inner `X[T]` is a template instantiation expression in field position. Decision #3
rejects it before the question of recursion is even reached. If the
instantiation were somehow materialised it would produce `data Y { rec: Y }`
for `type Y = X[...]`, which is direct self-referential recursion; but the
rejection happens earlier, at the instantiation-position check.

### 4.3 Container-mediated form is also rejected by matrix #1

```text
data Tree[T] {
  children: lst[Tree[T]]   // REJECTED — template instantiation in field position (matrix #1)
}
```

The inner `Tree[T]` is equally a template instantiation expression in field
position — the surrounding `lst[…]` is a builtin collection, not an alias
RHS, so it provides no exemption from decision #3. This form is forbidden on the same
grounds as the direct case.

If this template were materialised it would produce
`data IntTree { children: lst[IntTree] }`, a shape that
`BaboonValidator.checkLoops` would accept for a hand-written non-template
type. That acceptance does not retroactively permit the in-body instantiation
that would have been required to derive it.

### 4.4 Hand-written recursive types remain valid

Users who need a recursive container structure write the type directly
without a template:

```text
root adt RecTest1 {
  data Branch1 { value: RecTest1 }
  data Branch2 { value: i32 }
}

root adt RecTest2 {
  data Branch1 { value: opt[RecTest2] }
}
```

`BaboonValidator.checkLoops` (L69-87) and the `terminatesLoop` rule (L89-111)
in `baboon-compiler/src/main/scala/io/septimalmind/baboon/validator/BaboonValidator.scala`
accept recursion when at least one termination path exists. Two fixtures
demonstrate this: `RecTest1` (`pkg03.baboon:53-56`) uses **ADT-branch
alternative termination** — `Branch1`'s direct self-reference is acceptable
because `Branch2 { value: i32 }` terminates the ADT (the `terminatesLoop`
ADT arm uses `exists`). `RecTest2` (`pkg03.baboon:59-61`) uses
**option-mediated termination** — `opt[RecTest2]` terminates because `opt`
is a builtin collection (the `_: DomainMember.Builtin` arm in
`terminatesLoop`). A non-template recursive type can then be referenced from
any template that needs it.

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

The diagnostic name is `TemplateBodyCarriesDerived` (introduced in
PR-29.7): `Template 'X' carries ':derived[…]' — write the annotation on
the alias instead`.

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
5. **Templated identifiers** (`id Foo[T] { ... }`). Identifier fields are
   subject to a tightly controlled type set (M18) and templating opens
   wire-form questions independent of M29's monomorphisation question.
   The parser rejects `[…]` on `id` declarations.
6. **Templates on ADT inheritance arms** (`+ Foo[T]`, `- Foo[T]`,
   `^ Foo[T]`). M29 leaves `+` / `-` / `^` operating on non-templated
   names only (see §2.6).
7. **Per-language reified generics in emitted source.** No backend emits
   a generic type; every backend emits the monomorphised concrete type.
   This is the meaning of locked decision #4.
8. **Evolution of templates as templates.** Locked decision #5: only the
    materialised instances participate in `BaboonEvolution`. The
    evolution diff between two model versions is computed against each
    materialised concrete type independently, exactly as it is today for
    hand-written types. A template's body change affects every alias
    that instantiates it; the diff is observed at the alias-keyed
    concrete types, not at the template.
9. **Alias-rewrite of in-body template instantiations.** The typer does
    not structurally pattern-match a substituted template body against
    existing top-level aliases in order to rewrite an in-body instantiation
    to a reference to one. Concretely: even when a top-level alias
    `type K = Tree[i32]` exists, a template body
    `data Tree[T] { children: lst[Tree[T]] }` is rejected by matrix #1 —
    the typer does not recognise that `lst[Tree[T]]` would substitute
    to `lst[Tree[i32]]` and would match `lst[K]`. The acceptable form for
    recursive container structures remains a hand-written non-template
    recursive type (see §4.4). The reasons this path is excluded: the
    match is order-dependent (works only when the matching alias already
    exists at substitution time), it imposes a structural-equality scan
    over the substituted body on every instantiation, and it would make
    acceptance of a template body contingent on which other aliases happen
    to be present in the same model version — a fragile and non-local
    constraint.
10. **Tree-sitter editor grammar for template syntax.** M29 does not ship
    updated `grammar.js` / parser artefacts for the Baboon tree-sitter
    grammar. The grammar changes live in a 3-level submodule chain
    (`editors/baboon-zed` → `grammars/baboon`) that requires
    user-authorised pointer bumps across separate git repositories.
    Deferred to a dedicated submodule-coordination PR (`[PR-29.8-D01]`).
11. **Cross-namespace template instantiation (alias-RHS, same package).**
    Cross-namespace alias-RHS template instantiation within the same
    package is supported in M29 (`[PR-29.15]`). `type Y = foo.X[i32]`
    (where `X` is declared in namespace `foo` of the same package)
    resolves correctly: the resolver honours the prefix and looks up the
    template under `Owner.Ns([foo])`. Namespaces that contained only
    template declarations are dropped from the scope tree after template
    extraction so that an otherwise-empty namespace does not cause a
    scope-build failure. The detection-side matchers for prefixed
    templates in **forbidden positions** (bare alias-RHS without args —
    matrix #7; nested template-instantiation in alias-RHS args — matrix
    #2; in-body field-position instantiation — matrix #1) were also
    extended in PR-29.15 to honour namespace-qualified references, closing
    `[PR-29.5-D04]`, `[PR-29.7-D07]`, and `[PR-29.8-D06]`.
    What remains **not** supported is **cross-package** instantiation:
    the registry key includes `Pkg`, and `resolveTemplateKey` uses the
    alias's current package. Lifting this would require extending the
    registry and resolver to span packages and a scope-walker change to
    disambiguate package vs namespace prefixes; deferred to a future
    milestone.
12. **Cross-language end-to-end acceptance for templated services.** M29
    verifies service-template monomorphisation at the typer unit-test
    level and emits valid service code for all 9 backends. It does NOT
    exercise a templated service through the full `:test-service-acceptance`
    cross-language wire-round-trip harness. That would require extending
    `test/services/petstore.baboon` and the 9 per-language service
    harnesses (`[PR-29.10-D07]`). Deferred to a follow-up milestone.

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

### 7.3 The `m29-ok` fixture — full shape reference

The canonical fixture at
`baboon-compiler/src/test/resources/baboon/m29-ok/m29.baboon` defines
three template declarations (one `data`, one `adt`, one `service`) and five
alias instantiations in the same namespace `my.ok.m29`:

```text
model my.ok.m29

version "1.0.0"

data Item {
  name:  str
  price: f64
}

data Page[T] {
  items: lst[T]
  total: u32
}

adt Envelope[T, E] {
  data Ok  { value: T }
  data Err { error: E }
}

service Crud[K, V] {
  def get (K): V
  def put (V): K
}

root type IntPage        = Page[i32]          : derived[json], derived[ueba]
root type StrPage        = Page[str]          : derived[json], derived[ueba]
root type ItemPage       = Page[Item]         : derived[json], derived[ueba]
root type IntStrEnvelope = Envelope[i32, str] : derived[json], derived[ueba]
root type IntStrCrud     = Crud[i32, str]
```

After monomorphisation the typed model contains exactly the following
user-defined types (templates `Page`, `Envelope`, `Crud` are absent):

```text
// From Page[i32]:
root data IntPage : derived[json], derived[ueba] {
  items: lst[i32]
  total: u32
}

// From Page[str]:
root data StrPage : derived[json], derived[ueba] {
  items: lst[str]
  total: u32
}

// From Page[Item] — user-type argument, resolved to the non-template DTO:
root data ItemPage : derived[json], derived[ueba] {
  items: lst[Item]
  total: u32
}

// From Envelope[i32, str] — two-parameter ADT template:
root adt IntStrEnvelope : derived[json], derived[ueba] {
  data Ok  { value: i32 }
  data Err { error: str }
}

// From Crud[i32, str] — service template:
root service IntStrCrud {
  def get (i32): str
  def put (str): i32
}
```

All 9 backends emit concrete types keyed by `IntPage`, `StrPage`,
`ItemPage`, `IntStrEnvelope`, and `IntStrCrud` — no backend emits
`Page<int>`, `Envelope<int, string>`, `Crud<int, string>`, or any
parameterised form (locked decision #4). JSON and UEBA codecs are
generated for the four `root type` aliases that carry
`: derived[json], derived[ueba]`. No codec is generated for `IntStrCrud`
(service templates do not carry derivation annotations).

---

## 8. Implementation reference

The implementation of templates is tracked in milestone M29, PRs
PR-29.2 .. PR-29.11, per
`docs/drafts/20260503-2210-m29-generics-plan.md`. See also `tasks.md`,
section "Milestone M29 — PR breakdown", for the per-PR landing slots and
their dependency order. Diagnostic names for the forbidden positions in
§2.5 and §5.3 are all finalised: `TemplateNotInstantiated` (§2.5.7,
PR-29.7), `NotATemplate` (§2.5.8, PR-29.7), `TemplateBodyCarriesDerived`
(§5.3, PR-29.7), `VerificationIssue.ReferentialCyclesFound` (§2.5.9,
PR-29.6, existing validator diagnostic reused).

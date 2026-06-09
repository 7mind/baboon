# G1 / T1 — Wire-Key-Preservation Contract for RENAME Backends + Per-Backend Identifier-Emission Site Inventory

- Goal: `goals:G1`  ·  Defect: `defects:D1`  ·  Hypotheses: `hypothesis:H1` (confirmed), `hypothesis:H2` (confirmed)
- Status: design/decision artifact. **No source files are changed by T1.**
- Source baseline verified at git HEAD `ba30bee3` (worktree base `81bc95bc`). Every `file:line` below was re-read against this baseline; divergences from the D1/H2 evidence are noted inline.

This document locks two things the per-language fix tasks (G1-W2) implement against:

1. The **wire-key invariant** for the four backends that have NO target-language escape syntax and must therefore RENAME a model-derived name to dodge a keyword collision (Java, Python, TypeScript, Dart).
2. A **per-backend site inventory** of every identifier-emission site a model name flows through, with the centralized-vs-scattered classification that decides whether escaping can be hooked once or must be applied per site.

---

## Part 1 — Wire-Key Invariant for RENAME Backends

### 1.0 The invariant (cross-cutting)

> For every backend and every keyword-colliding model name, the on-wire **JSON object key** and the on-wire **UEBA byte layout** MUST be IDENTICAL to (a) the bytes/keys a non-colliding name would have produced, (b) the pre-fix output for non-colliding names, and (c) every other backend's output for the same model.

Restated operationally: the keyword-escaping/renaming fix is a **source-identifier-only** transformation. It may change the *target-language identifier* used in the generated source (field accessor, parameter, local, type/branch name), but it MUST NOT change a single byte that crosses the wire. The JSON wire key and the UEBA byte stream are functions of the **model name** and **field/branch order**, never of the emitted target identifier.

This is exactly the discipline Rust already enforces. When Rust snake-cases a field identifier and the result differs from the model name, it preserves the wire key with a serde rename:

```scala
// RsDefnTranslator.scala:1066-1073  (fieldSerdeAttributes)
val originalName = f.name.name
val rustName     = toSnakeCase(originalName)
if (rustName != originalName) {
  attrs += q"""#[serde(rename = "$originalName")]"""
}
```

The four RENAME backends below MUST reproduce this "identifier diverges ⇒ pin the wire key to the original model name" discipline using each target's native serialization-alias mechanism.

### 1.1 Java (Jackson) — pin the JSON key with the model-name string literal already in the codec

Java's JSON codec is hand-rolled per DTO; the wire key is **already** a string literal `f.name.name`, fully decoupled from the Java identifier used to read the value:

```scala
// JvJsonCodecGenerator.scala:167-178  (genDtoBodies)
val fieldRef = q"value.${f.name.name}()"          // Java identifier (record accessor)  — line 170
q"""obj.set("${f.name.name}", $enc);"""           // JSON wire key (string literal)     — line 172
// decode:
mkDecoder(f.name.name, f.tpe, q"jsonObj")          // line 177
//   → JvJsonCodecGenerator.scala:475/486:  $jsonObjRef.get("$fieldName")  — wire key is the model-name literal
```

ADT branch wire key is likewise a literal:

```scala
// JvJsonCodecGenerator.scala:112  wrapAdtBranchEncoder
q"""$jsonNodeFactory.instance.objectNode().set("$branchName", $tree)"""   // branchName = m.name.name (line 118)
// decode dispatch: JvJsonCodecGenerator.scala:133  case "$branchName":
```

**Mechanism.** When a field/record component is RENAMED to dodge a Java keyword, the codec's `"${f.name.name}"` string literals ALREADY emit the original model name, so the codec wire key is automatically correct. The ONLY change the fix introduces on the Java side is the *accessor identifier* `value.<renamed>()` (line 170). For consumers that read the generated record directly via Jackson data-binding (not via the Baboon codec), the record component MUST additionally carry `@JsonProperty("<modelName>")` so the field name surfaced by Jackson reflection equals the model name. **Decision:** the per-field `@JsonProperty("<modelName>")` annotation is emitted on the record component whenever the Java identifier was renamed; the hand-rolled codec keeps emitting the `f.name.name` literal verbatim (it already does — no change). `default` is a Java keyword (the exact C# trigger), so this path is load-bearing.

NOTE — this corrects the D1 `suggestedFix` shorthand "@JsonProperty on the field/getter": the codec wire key needs **no** change (it is already the literal); `@JsonProperty` is required only to keep the record-component's reflective/data-bound name aligned for any non-codec Jackson consumer.

### 1.2 Python (pydantic + hand-rolled walker) — wire key lives in TWO places; both must pin the model name

Python's JSON codec has two distinct paths, and the wire key is determined differently in each:

- **Transparent pydantic path** (no `any`-bearing / user-key-map / ADT fields): encode/decode delegate to `value.model_dump_json()` / `Name.model_validate_json(wire)`:

  ```scala
  // PyJsonCodecGenerator.scala:105-106 (ADT), 127-128 (DTO transparent)
  q"""return value.model_dump_json()"""
  q"""return $name.model_validate_json(wire)"""
  ```

  Here the wire key is **NOT** a literal in the codec — it is the pydantic field's serialized name, derived from the generated model's attribute name (emitted verbatim at `PyDefnTranslator.scala:540`, `q"$fieldName: $fieldType"`).

- **Explicit-walker path** (any-bearing / user-key-map / ADT fields): the codec patches the dict with literal keys:

  ```scala
  // PyJsonCodecGenerator.scala:131-139  (genDtoBodies, walker branch)
  q"obj['${f.name.name}'] = ${mkJsonAnyEncoder(f.tpe, q"value.${f.name.name}")}"   // line 135
  q"obj['${f.name.name}'] = ${mkJsonAnyDecoder(f.tpe, q"obj['${f.name.name}']")}"  // line 139
  ```

  Here the wire key IS a literal `f.name.name`, decoupled from the Python attribute.

**Mechanism.** Because the transparent path's wire key comes from the pydantic attribute name, a Python RENAME (PEP8 trailing `_`, since Python has no escape syntax) would change the wire key UNLESS the generated pydantic model declares a serialization alias. **Decision:** when a model field is RENAMED for the Python identifier, the generated pydantic model MUST emit a field alias pinned to the model name — i.e. `field_<renamed>: T = Field(alias="<modelName>", serialization_alias="<modelName>")` (or equivalent `model_config` `populate_by_name=True` + `Field(alias=...)`), so `model_dump_json` writes and `model_validate_json` reads the model name. The explicit-walker path needs no change (it already emits the `f.name.name` literal). This is the field-name-to-wire-key map the D1/H2 evidence calls for, realized as a pydantic alias rather than a codec literal — both transparent and walker paths then agree on the model-name wire key. ADT/enum dispatch in Python is by `isinstance` / value, not by a name local, so there is no branch-lowercase collision analog (per H2).

### 1.3 TypeScript (function-based codec) — bracket-string wire key, separate from the TS member identifier

TS's JSON codec emits wire keys as bracket strings and the TS member access separately:

```scala
// TsJsonCodecGenerator.scala:115-130  (genDtoCodec)
val fld = f.name.name
q""""$fld": ${mkJsonEncoder(f.tpe, q"value.$fld")},"""          // wire key "$fld"; TS access value.$fld  — line 119
// decode:
q"""obj["$fld"] === undefined ... ${mkJsonDecoder(f.tpe, q"""obj["$fld"]""")},"""  // wire key obj["$fld"]  — line 127/129
// ADT branch:  TsJsonCodecGenerator.scala:173  { "$branchName": ... }   /  :183  case "$branchName":
```

UEBA decode introduces a `const <fieldName>` local (the H2-cited collision site):

```scala
// TsUEBACodecGenerator.scala:169
val dec = q"const $fieldName = ${mkDecoder(field.tpe)};"
```

**Mechanism.** TS has no escape syntax → RENAME. The wire key `"$fld"` is already the model-name string literal in encode/decode, so it stays correct automatically; the fix changes only `value.$fld` (the property access — must use the renamed member) and the UEBA `const $fieldName` local (must use a renamed/safe local). **Decision:** keep emitting the bracket-string `"<modelName>"` wire key verbatim; rename only the TS member identifier (`value.<renamed>`) and the UEBA local. Because the model's TS interface/class member would also be renamed, the encode side reads `value.<renamed>` while still writing `"<modelName>"` — the bracket-string access on the OBJECT side (`obj["<modelName>"]`) is unaffected. Note `escapeTsKeyword` (`TsTypeTranslator.scala:310-319`) currently has zero callers (dead code, per H2); the TS fix wires renaming through the member-emission sites, not through this helper unless repurposed.

### 1.4 Dart (dart:convert) — literal map key, separate from the named-constructor parameter

Dart's JSON codec emits literal map keys; the Dart getter and named-constructor parameter are separate identifiers:

```scala
// DtJsonCodecGenerator.scala:159-169  (genDtoBodies)
val fieldRef = q"value.${f.name.name}"                       // Dart getter access  — line 162
q"""'${f.name.name}': $enc,"""                               // JSON wire key (literal map key) — line 164
// decode:
q"${f.name.name}: ${mkDecoder(f.name.name, f.tpe, q"jsonObj")}"   // named-ctor param : decoded value — line 169
//   → DtJsonCodecGenerator.scala:422/424:  $jsonObjRef['$fieldName']   — wire key is the model-name literal
// ADT branch:  DtJsonCodecGenerator.scala:106  {'$branchName': ...}  /  :128  '$branchName' => ...
```

**Mechanism.** Dart has no escape syntax → RENAME (`escapeDartKeyword` at `DtTypeTranslator.scala:220` currently renames with a trailing `_`). The wire keys `'${f.name.name}'` (encode, line 164) and `$jsonObjRef['$fieldName']` (decode, lines 422/424) are already model-name literals and stay correct. The fix changes only the Dart getter `value.<renamed>` (line 162) and the named-constructor parameter label `<renamed>:` (line 169). **Decision:** keep emitting the literal map key `'<modelName>'`; rename only the Dart getter/field and named-parameter identifiers. The decode-site named parameter `<renamed>:` binds to the renamed constructor parameter while the map lookup `jsonObj['<modelName>']` stays the model name.

### 1.5 UEBA is positional — renames are wire-neutral (confirmed)

UEBA never serializes a field name or a branch name; it is index/byte-positional. Verified in C#:

- ADT branch discriminator is the **branch index**, not the name:

  ```scala
  // CSUEBACodecGenerator.scala:188-218  (genAdtBodies)
  a.dataMembers(domain).zipWithIndex ...                    // index drives the wire
  q"""writer.Write((byte)${idx.toString});"""               // encode: positional byte tag — line 201
  q"""if (asByte == ${idx.toString})"""                     // decode: match on index      — line 218
  val castedName = branchName.toLowerCase                   // line 196 — a LOCAL ONLY (H1 collision); never on the wire
  ```

- DTO fields are written sequentially with no field-name key:

  ```scala
  // CSUEBACodecGenerator.scala:280-316  (genDtoBodies)
  q"writer.Write(header);"
  fields.map(_._1).joinCN().endC()                          // sequential per-field encoders — line 285
  // field names appear ONLY as property accessors:  value.${f.name.name.capitalize}  — line 373
  ```

Therefore: **UEBA byte layout is invariant under any keyword rename or escape.** The `castedName`/`$castedName` locals in the C#/Kotlin/Java UEBA generators (CSUEBACodecGenerator.scala:196, KtUEBACodecGenerator.scala:185→204, JvUEBACodecGenerator.scala:182→199) are pure source-side locals; renaming/escaping them changes no wire byte. The same holds for the TS UEBA `const <fieldName>` local (TsUEBACodecGenerator.scala:169) — it is a decode-side local, and UEBA field order, not name, is the wire contract.

### 1.6 Summary table — wire-key mechanism per RENAME backend

| Backend | JSON wire key source | Already model-name literal? | Source identifier that the rename changes | Extra wire-pinning needed |
|---|---|---|---|---|
| Java | codec string literal `"f.name.name"` (Jv:172/475); ADT `"branchName"` (Jv:112/133) | Yes | record accessor `value.<f>()` (Jv:170) | `@JsonProperty("<modelName>")` on record component (for non-codec Jackson consumers) |
| Python | transparent: pydantic attr name (Py:127-128 / PyDefn:540); walker: literal `obj['f.name.name']` (Py:135/139) | Walker: yes. Transparent: NO (attr-derived) | pydantic attribute (renamed `_`) | `Field(alias="<modelName>", serialization_alias="<modelName>")` on the model field |
| TypeScript | bracket-string `"fld"` (Ts:119/127); ADT `"branchName"` (Ts:173/183) | Yes | TS member `value.<fld>` (Ts:119); UEBA `const <fld>` local (TsUEBA:169) | none (wire key already literal); rename member + local only |
| Dart | literal map key `'f.name.name'` (Dt:164/422/424); ADT `'branchName'` (Dt:106/128) | Yes | Dart getter `value.<f>` (Dt:162); named-ctor param `<f>:` (Dt:169) | none (wire key already literal); rename getter + param only |

In all four, UEBA is positional ⇒ no wire-key work; only the source-side local/accessor identifiers move.

---

## Part 2 — Per-Backend Identifier-Emission Site Inventory

Legend for "render hook":
- **Centralized** = a single render pass exists into which a keyword transform is (or can be) hooked once.
- **Scattered** = names are emitted as raw strings at each definition/codec site; each needs a per-site fix.
- Sites are categorized: type / ADT-branch / enum-member / field / property / method / parameter / codec-capture-local / namespace.

Citations re-verified at HEAD `ba30bee3`. Where a citation tracked D1/H2, it is reproduced; where the line shifted or the description needed refinement, the correction is noted.

### C# (`translator/csharp/`) — fix-direction: `@`-verbatim. Scattered.
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| ADT-branch codec-capture-local (JSON) | `branchName.toLowerCase` → `if (value is $fqBranch $branchNameRef)` | CSJsonCodecGenerator.scala:158 → :177; `Encode(ctx, $branchNameRef)` :162 | NO (H1 root cause) |
| ADT-branch codec-capture-local (UEBA) | `castedName = branchName.toLowerCase` → `if (value is $fqBranch $castedName)` | CSUEBACodecGenerator.scala:196 → :213 | NO (H1 root cause) |
| property accessor (UEBA) | `value.${f.name.name.capitalize}` | CSUEBACodecGenerator.scala:373 | NO |
| type / branch / enum-member / field / namespace | derived in CS*Translator, no reserved-word pass | per D1 rootCause | NO |

Render hook: **Scattered** — no centralized terminal render pass; the two `toLowerCase` capture-locals (Json:158, Ueba:196) are the reported-symptom sites and must be fixed first.

### Scala (`translator/scl/`) — fix-direction: backtick `` `name` ``. Has a terminal render pass, but it does NOT escape.
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| ADT-branch codec-capture-local (JSON) | `branchName.toLowerCase` → `case $branchNameRef: $fqBranch =>` | ScJsonCodecGenerator.scala:201 → :211 | NO |
| field | field names verbatim | ScDefnTranslator.scala:307 | NO |
| type (terminal renderer) | `case t: ScValue.ScType => t.name` (raw) | ScBaboonTranslator.scala:283-290 | NO — emits `t.name` raw |
Render hook: **Centralized site EXISTS but is a no-op for escaping** — `ScBaboonTranslator.scala:283-290` `mapRender` resolves package qualification only and emits `t.name` raw. A backtick transform could be hooked here for type names, but branch-capture-locals (ScJson:201) and field names (ScDefn:307) are emitted outside it and need per-site fixes. Triggers: `type`/`val`/`object`/`class`/`match` (`default` is not a Scala keyword).

### Kotlin (`translator/kotlin/`) — fix-direction: backtick `` `name` ``. Scattered.
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| ADT-branch codec-capture-local (UEBA) | `castedName = branchName.toLowerCase` → `val $castedName = instance` | KtUEBACodecGenerator.scala:185 → :204 | NO |
| field / type / package | verbatim | per H2 | NO |
Render hook: **Scattered**. Triggers: `object`/`is`/`when`/`fun`/`val`/`class`.

### Java (`translator/java/`) — fix-direction: RENAME (no escape syntax). Scattered. RENAME backend (Part 1.1).
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| ADT-branch codec-capture-local (UEBA) | `castedName = branchName.toLowerCase` → `if (value instanceof $adtRef $castedName)` | JvUEBACodecGenerator.scala:182 → :199 | NO (`default` IS a Java keyword) |
| record accessor (JSON) | `value.${f.name.name}()` | JvJsonCodecGenerator.scala:170 | NO |
| JSON wire key (field) | `obj.set("${f.name.name}", …)` / decode `$jsonObjRef.get("$fieldName")` | JvJsonCodecGenerator.scala:172 / :475,:486 | wire key = model-name literal (CORRECT; do not change) |
| ADT-branch JSON wire key | `objectNode().set("$branchName", …)` / `case "$branchName":` | JvJsonCodecGenerator.scala:112 / :133 | wire key = model-name literal (CORRECT) |
| field / package / class / enum names | verbatim | per H2 | NO |
| JSON codec branch dispatch local | fixed `branchVal` | JvJsonCodecGenerator.scala:121 | safe (fixed name) |
Render hook: **Scattered**. Rename the Java identifier sites (UEBA local Jv:182; record accessor Jv:170; type/field/enum decls) and add `@JsonProperty("<modelName>")`; keep codec wire-key literals unchanged (Part 1.1).

### Python (`translator/python/`) — fix-direction: RENAME (no escape syntax). Scattered. RENAME backend (Part 1.2).
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| field declaration | `q"$fieldName: $fieldType"` | PyDefnTranslator.scala:540 | NO (drives transparent-path wire key) |
| class / enum / method names | `.capitalize` | per H2 | NO |
| walker JSON wire key (field) | `obj['${f.name.name}']` (encode/decode patches) | PyJsonCodecGenerator.scala:135 / :139 | wire key = model-name literal (CORRECT) |
| transparent JSON path | `value.model_dump_json()` / `model_validate_json(wire)` | PyJsonCodecGenerator.scala:105-106,127-128 | wire key = pydantic attr name (needs alias on rename) |
Render hook: **Scattered**. No branch-lowercase analog (ADT dispatch via `isinstance`). Triggers: field/method named `class`/`def`/`import`/`lambda`/`is`/`in`; also `none`/`true`/`false`. Rename field/method identifiers; pin the wire key via a pydantic `Field(alias=...)` (Part 1.2).

### TypeScript (`translator/typescript/`) — fix-direction: RENAME (no escape syntax; `escapeTsKeyword` is dead). Scattered. RENAME backend (Part 1.3).
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| dead escaper | `escapeTsKeyword` — zero callers tree-wide | TsTypeTranslator.scala:310-319 | n/a (dead) |
| JSON wire key (field) | `"$fld"` encode / `obj["$fld"]` decode | TsJsonCodecGenerator.scala:119 / :127,:129 | wire key = model-name literal (CORRECT) |
| TS member access (JSON encode) | `value.$fld` | TsJsonCodecGenerator.scala:119 | NO |
| ADT-branch JSON wire key | `{ "$branchName": … }` / `case "$branchName":` | TsJsonCodecGenerator.scala:173 / :183 | wire key = model-name literal (CORRECT) |
| codec-capture-local (UEBA) | `const $fieldName = …` | TsUEBACodecGenerator.scala:169 | NO |
| type / class / enum-member / getter / param | verbatim | per H2 | NO |
Render hook: **Scattered**. Rename the TS member identifiers and the UEBA `const` local; keep bracket-string wire keys (Part 1.3).

### Rust (`translator/rust/`) — fix-direction: `r#` raw idents (+ rename for `self`/`super`/`crate`/`Self`). PARTIAL — lowest risk.
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| field / fn / module names | `escapeRustKeyword` (`r#`) + `escapeRustModuleName` via `toSnakeCase` | RsDefnTranslator.scala:1533-1546 | YES (`r#`) |
| **wire-key precedent** | `if (rustName != originalName) #[serde(rename = "$originalName")]` | RsDefnTranslator.scala:1066-1073 | YES — canonical wire-key-preservation model for Part 1 |
| type / struct / enum / trait / variant names | bare `tid.name.name.capitalize` | RsTypeTranslator.scala:155 | NO — residual gap |
| codec branch local | fixed `v` | per H2 | safe |
Render hook: **Scattered, mostly covered.** Residual gap: type/variant names via `.capitalize` (Rust keywords are lowercase, so `.capitalize` dodges most); `self`/`super`/`crate`/`Self` collide AND are not `r#`-escapable → must rename.

### Dart (`translator/dart/`) — fix-direction: RENAME via `escapeDartKeyword` (trailing `_`). MIXED — centralized for TYPE names; scattered for members. RENAME backend (Part 1.4).
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| type names (render pass) | `escapeDartKeyword` applied in `mapRender` for `DtTypeName`/`DtType` | DtBaboonTranslator.scala:270-303 (272-301 the escape arms) | YES (type names only) |
| service-wiring type names | `escapeDartKeyword` | DtServiceWiringTranslator.scala:359-360 | YES |
| Dart getter access (JSON) | `value.${f.name.name}` | DtJsonCodecGenerator.scala:162 | NO |
| JSON wire key (field) | `'${f.name.name}'` encode / `$jsonObjRef['$fieldName']` decode | DtJsonCodecGenerator.scala:164 / :422,:424 | wire key = model-name literal (CORRECT) |
| named-ctor param (JSON decode) | `${f.name.name}: …` | DtJsonCodecGenerator.scala:169 | NO |
| ADT-branch JSON wire key | `{'$branchName': …}` / `'$branchName' =>` | DtJsonCodecGenerator.scala:106 / :128 | wire key = model-name literal (CORRECT) |
| field / getter / method names | verbatim (bypass render pass) | DtDefnTranslator.scala:307/318/538/571 | NO |
| ADT codec branch local | fixed `branchVal` | DtJsonCodecGenerator.scala:114 | safe |
Render hook: **Centralized for type names** (`DtBaboonTranslator.scala:270-303` — `escapeDartKeyword` already hooked into `mapRender` for `DtTypeName`/`DtType`). **Scattered for members** — field/getter/method/named-param identifiers are emitted as raw `f.name.name` at the codec/defn sites and bypass the render pass; each needs the helper wired through. Triggers (member): `default`/`class`/`final`/`void`/`switch`/`is`/`in`.

### Swift (`translator/swift/`) — fix-direction: backtick via `escapeSwiftKeyword`. MIXED — centralized for type/field names; scattered for case names.
| Site | Emission | Citation | Escaped today? |
|---|---|---|---|
| type / field names (render pass) | `escapeSwiftKeyword` in `mapRender` for `SwTypeName`/`SwType` | SwBaboonTranslator.scala:331-340 | YES |
| escaper definition | `escapeSwiftKeyword(name)` (backticks) | SwTypeTranslator.scala:343 | n/a |
| ADT-branch case names | built as RAW strings, bypass render pass | SwDefnTranslator.scala:889-894 | NO (`case default(...)`) |
| ADT-branch JSON case | raw | SwJsonCodecGenerator.scala:109-128 | NO |
| ADT-branch UEBA case | raw | SwUEBACodecGenerator.scala:150-178 | NO |
Render hook: **Centralized for type/field names** (`SwBaboonTranslator.scala:331-340` — `escapeSwiftKeyword` hooked into `mapRender`, escaping `SwTypeName` and dotted `SwType` segments). **Scattered for ADT-branch/enum-case names** — emitted as raw `String` (not `SwValue.SwTypeName`) so they bypass the hook. Fix: route case names through `escapeSwiftKeyword` (or wrap as `SwTypeName`).

### Part 2 summary — render-hook classification

| Backend | Escape mechanism | Render hook | Per-site work required |
|---|---|---|---|
| C# | `@`-verbatim | Scattered | All sites; **start with** Json:158 + Ueba:196 capture-locals (reported symptom) |
| Scala | backtick | Centralized site exists (ScBaboon:283-290) but no-op for escaping | Hook type names in render pass; per-site for ScJson:201 local + ScDefn:307 fields |
| Kotlin | backtick | Scattered | All sites; KtUeba:185→204 local |
| Java | RENAME + `@JsonProperty` | Scattered | Rename identifiers (JvUeba:182, Jv accessor:170, decls); keep codec literals |
| Python | RENAME + `Field(alias)` | Scattered | Rename identifiers (PyDefn:540 etc.); add pydantic alias |
| TypeScript | RENAME (wire dead escaper) | Scattered | Rename members + UEBA local; keep bracket-string keys |
| Rust | `r#` (+ rename self/super/crate/Self) | Scattered, mostly covered | Type/variant names (RsType:155); non-`r#`-able keywords |
| Dart | RENAME (`escapeDartKeyword`) | Centralized for types (DtBaboon:270-303); scattered for members | Wire helper through DtDefn:307/318/538/571 + DtJson getter:162 / param:169 |
| Swift | backtick (`escapeSwiftKeyword`) | Centralized for type/field (SwBaboon:331-340); scattered for cases | Route case names (SwDefn:889-894, SwJson:109-128, SwUeba:150-178) through escaper |

---

## Decision record (to be locked as a `decisions` item linked to `goals:G1` + `defects:D1`)

1. **Wire-key invariant (locked):** keyword escaping/renaming is a source-identifier-only transform. JSON wire keys and UEBA byte layout are functions of the model name and field/branch order, never of the emitted target identifier. Verified identical across all 9 backends and against pre-fix output for non-colliding names.
2. **RENAME backends pin the wire key as follows:** Java — codec already emits the `f.name.name` literal (no change); add `@JsonProperty("<modelName>")` on the record component for non-codec Jackson consumers. Python — add a pydantic `Field(alias="<modelName>", serialization_alias="<modelName>")` so the transparent `model_dump_json`/`model_validate_json` path keeps the model-name key; the walker path already emits the literal. TypeScript — keep the bracket-string `"<modelName>"` wire key; rename only TS members and the UEBA `const` local. Dart — keep the literal map key `'<modelName>'`; rename only the getter/field and named-constructor parameter. All mirror Rust's `#[serde(rename = "<originalName>")]` discipline (RsDefnTranslator.scala:1066-1073).
3. **UEBA is rename-neutral (locked):** ADT discriminator is the branch index (CSUEBA:201/218); DTO fields are positional (CSUEBA:285). The `*toLowerCase` capture-locals are pure source locals and never reach the wire.
4. **Per-site checklist (locked):** the Part 2 tables enumerate every identifier-emission site with `file:line` per backend, and classify each backend's render hook as centralized (Scala terminal renderer — present but no-op for escaping; Dart type-name pass; Swift type/field pass) vs. scattered. The per-language G1-W2 tasks implement against these tables so no site is missed.

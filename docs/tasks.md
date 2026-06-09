---
ledger: tasks
counters:
  milestone: 0
  item: 14
archives: []
---

# tasks

## M3

### T1 — planned

- createdAt: 2026-06-09T20:47:06.854Z
- updatedAt: 2026-06-09T20:47:06.854Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Lock wire-key-preservation contract for RENAME backends and define the per-backend identifier-emission site inventory
- description: |
    DESIGN/DECISION task (no code yet). Produce a written contract, recorded as a `decisions` ledger item linked to goals:G1+defects:D1, that the per-language fix tasks (G1-W2) implement against. Two parts:
    
    1) WIRE-KEY INVARIANT for RENAME backends (Java, Python, TypeScript, Dart): when a model-derived name is RENAMED to dodge a target keyword (no escape syntax available), the emitted JSON/UEBA wire key MUST remain the ORIGINAL model name. Mirror Rust's existing `#[serde(rename = "<modelName>")]` discipline (RsDefnTranslator.scala). Specify the concrete annotation/mechanism per RENAME backend: Java=Jackson `@JsonProperty("<modelName>")` on the field/getter; Python=codec emits the original key string (field-name-to-wire-key map, since codecs are hand-rolled — NOT attribute name); TypeScript=bracket-string property access `obj["<modelName>"]` in the function-based codec; Dart=explicit literal key in the dart:convert map (`json['<modelName>']`), not the renamed getter. UEBA is positional/index-based so the rename is wire-neutral there — confirm and note this. The invariant: for every backend and every keyword-colliding name, the on-wire JSON key and UEBA byte layout are IDENTICAL to the pre-fix output for non-colliding names and to every OTHER backend.
    
    2) SITE INVENTORY: enumerate, per backend, every identifier-emission site a model name flows through (type/ADT-branch/enum-member/field/property/method/parameter/codec-capture-local/namespace), citing file:line from the D1/H2 evidence + grounding. This becomes the per-language task checklist so no site is missed. Confirm whether each backend has a single centralized render pass (Swift/Dart do; Scala terminal renderer ScBaboonTranslator:283-290) into which escaping can be hooked once, vs. scattered raw emissions needing per-site fixes.
- acceptance: "A `locked` decisions item linked goals:G1+defects:D1 exists containing (a) the wire-key invariant with the concrete annotation mechanism named for each of Java/Python/TS/Dart and an explicit statement that UEBA layout is unaffected by renames, and (b) a per-backend table of identifier-emission sites with file:line citations covering all 9 backends. No source files changed by this task."
- suggestedModel: frontier
- ledgerRefs: ["goals:G1","defects:D1"]

### T2 — planned

- createdAt: 2026-06-09T20:47:26.002Z
- updatedAt: 2026-06-09T20:47:26.002Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Add shared reserved-word test model(s) to the test resource model-dir
- description: |
    Add a new keyword-collision test model under the shared model-dir `baboon-compiler/src/test/resources/baboon/<subdir>/` (e.g. `reserved-words-ok/reserved.baboon`), modeled after the existing minimal subdir pattern (e.g. recursive-ok/recursive.baboon — isolated, root-reachable so it is included in output). The model must exercise the reserved-word collision class across identifier KINDS so every per-language fix site is hit by at least one backend:
    - ADT with a branch named for the reported symptom: `adt AvatarItem { data Default {} data BuiltIn { id: str } }` (C#/Java `default`; Swift `default` case) plus branches named for other-language keywords (`Type`, `Object`, `Class`, `When`, `Match`, `Is`).
    - A root DTO reachable from a @root with FIELDS named after keywords spanning languages: `default`, `class`, `final`, `void`, `is`, `in`, `none`, `true`, `false`, `type`, `val`, `object`, `def`, `import`, `switch`, `self`, `super`, `crate`.
    - An enum whose MEMBERS are keywords (Swift enum-case + general enum-member sites).
    - A type whose NAME maps to a Rust keyword post-capitalize is impossible (Rust kws are lowercase) — instead include a type/variant collision relevant to Rust's residual gap if expressible, else document why type-name collision is not reachable for Rust and rely on field/module coverage.
    Keep it ONE cohesive model so a single `--generate-{json,ueba}-codecs-by-default=true` generation pass surfaces codec-capture-variable collisions too. Do NOT yet wire it into mdl actions (that is G1-W3); this task only adds the .baboon source and confirms it PARSES + TYPECHECKS (compiler accepts the model; codegen may still emit non-compiling target code until W2 lands — that is expected).
- acceptance: New `.baboon` model file(s) committed under baboon-compiler/src/test/resources/baboon/<subdir>/; running the baboon compiler on that model-dir succeeds (parse+typecheck pass, exit 0) and the model contains branches, fields, and enum members named after reserved words covering C#/Java `default`, Scala/Kotlin keywords, Python `none/true/false`, and Rust `self/super/crate`. Generated target code is NOT required to compile yet.
- suggestedModel: standard
- ledgerRefs: ["goals:G1","defects:D1"]

## M4

### T3 — planned

- createdAt: 2026-06-09T20:47:38.592Z
- updatedAt: 2026-06-09T20:53:05.917Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "C# (minimal symptom fix): @-escape the two ADT branch codec capture variables"
- description: "Resolve the REPRODUCED reported symptom (D1/H1). At CSJsonCodecGenerator.scala:158 (`val branchNameRef = q\"${branchName.toLowerCase}\"`, used as the pattern-capture var at :177 `if (value is $fqBranch $branchNameRef)` and :162 `Encode(ctx, $branchNameRef)`) and CSUEBACodecGenerator.scala:196 (`val castedName = branchName.toLowerCase`, used at :199), apply a C# verbatim-identifier escape so a lowercased branch name that is a C# keyword (e.g. `default`) is emitted as `@default`. Add a `escapeCsKeyword`/`@`-prefix helper (or reuse an existing C# identifier helper if one exists) covering the full C# keyword set, and apply it to the capture variable. Confirm BOTH the non-wrapped and `wrappedAdtBranchCodecs` arms (:164/:198) are correct — the wrapped arm uses `wire`/cName and may not need the capture var, but verify. This is the minimal blast-radius fix; the general C# identifier pass is T4. dependsOn T1."
- acceptance: "Generating C# for the reserved-word test model (T2) with `--generate-{json,ueba}-codecs-by-default=true` produces `if (value is AvatarItem.Default @default)` (or equivalent escaped capture) instead of the bare `default`; the cs-stub project containing the generated code compiles with `dotnet build` (no CS1026); pre-existing non-colliding branches still compile. JSON/UEBA wire output for the branch is unchanged (capture var is local, not on-wire)."
- suggestedModel: frontier
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T4 — planned

- createdAt: 2026-06-09T20:47:48.860Z
- updatedAt: 2026-06-09T20:47:48.860Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "C# (general pass): @-escape all model-derived identifiers across the C# translator"
- description: "Complete fix for the C# backend beyond the two capture sites (T3). Using the C# `@`-verbatim helper from T3, route EVERY model-derived C# identifier emission through it: type names, ADT-branch nested type names, enum member names, DTO field/property names, method names, constructor parameters, codec locals, and namespace segments — per the site inventory from T1. C# is case-sensitive so PascalCase type names rarely collide, but lowercased/camelCased members and params can; the `@` prefix is wire-neutral for C# (Newtonsoft serializes the property name without `@`, but verify JSON property names use the model name via attribute or are otherwise wire-stable). Where C# emits a JSON property name derived from a renamed/escaped identifier, ensure the on-wire key remains the original model name. dependsOn T3 (reuses its helper)."
- acceptance: Generating C# for the reserved-word test model (T2) with codecs-by-default produces a cs-stub that compiles with `dotnet build` for ALL identifier kinds (keyword-named fields, params, enum members, methods), not just the branch capture var; cross-language JSON round-trip (conv-test-cs) preserves wire keys equal to the model names; existing cs regular/wrapped tests still pass.
- suggestedModel: frontier
- dependsOn: ["T3"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T5 — planned

- createdAt: 2026-06-09T20:47:59.203Z
- updatedAt: 2026-06-09T20:53:07.282Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Scala: backtick-escape all model-derived identifiers (no escaping exists today)"
- description: "Scala has NO reserved-word escaping anywhere (H2). Add an `escapeScKeyword` helper (backtick-quote `` `name` `` for the Scala keyword set — `type`/`val`/`object`/`class`/`match`/`def`/`trait`/`implicit`/etc.; note `default` is NOT a Scala keyword) and route all model-derived identifier emissions through it: the terminal renderer (ScBaboonTranslator:283-290 emits names raw), DTO field names (ScDefnTranslator:307), and the JSON codec branch capture var (ScJsonCodecGenerator:201 `branchNameRef = branchName.toLowerCase` → :211 `case $branchNameRef: $fqBranch =>`). Backtick-quoting is wire-neutral (the backticks are not part of the identifier text). Prefer hooking the centralized terminal renderer once if all names flow through it; add per-site fixes for the codec capture var. dependsOn T1."
- acceptance: Generating Scala for the reserved-word test model (T2) with codecs-by-default produces an sc-stub that compiles with `sbt +compile` (JVM+JS) including fields/branches named `type`/`val`/`object`/`match`; existing sc regular/wrapped tests pass; JSON/UEBA wire keys equal the model names (backtick is source-only).
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T6 — planned

- createdAt: 2026-06-09T20:48:08.169Z
- updatedAt: 2026-06-09T20:53:08.698Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Kotlin: backtick-escape all model-derived identifiers (no escaping exists today)"
- description: "Kotlin has NO reserved-word escaping anywhere (H2). Add an `escapeKtKeyword` helper (backtick-quote `` `name` `` for the Kotlin hard-keyword set — `object`/`is`/`when`/`fun`/`val`/`class`/`in`/`when`/etc.) and route all model-derived identifier emissions through it: type/field/package names (verbatim today) and the UEBA codec branch capture var (KtUEBACodecGenerator:185 `castedName = branchName.toLowerCase` → :204 `val $castedName = instance`). Backtick is wire-neutral. Note the Kotlin JSON codec uses Jackson — if a field is backtick-escaped its on-wire key is unaffected (Kotlin backtick identifiers serialize under the identifier text minus backticks); confirm the wire key equals the model name and add `@JsonProperty` only if Jackson would otherwise diverge. Cover both kt-stub and kt-stub-kmp. dependsOn T1."
- acceptance: Generating Kotlin for the reserved-word test model (T2) with codecs-by-default produces kt-stub AND kt-stub-kmp projects that compile (gradle) including fields/branches named `object`/`is`/`when`/`fun`/`val`; existing kt regular/wrapped (+kmp) tests pass; JSON/UEBA wire keys equal the model names.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T7 — planned

- createdAt: 2026-06-09T20:48:17.897Z
- updatedAt: 2026-06-09T20:53:10.148Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Swift: route ADT-branch and enum-case names through the existing escapeSwiftKeyword (backticks)"
- description: "Swift PARTIALLY escapes (H2): escapeSwiftKeyword (backticks) is applied via the centralized render pass (SwBaboonTranslator:331-340) for type/field names, but ADT-branch case names + enum case names are built as RAW strings and bypass it. Fix the gap sites: SwDefnTranslator.scala:889-894 (`caseName = memberName.head.toLower + tail`, emitted as `case $caseName(...)`) and the codec sites SwJsonCodecGenerator:109-128 + SwUEBACodecGenerator:150-178 — route `caseName` through escapeSwiftKeyword so branch `Default` emits `case `default`(...)` not `case default(...)`. Confirm the JSON codec's on-wire case key (the discriminator string) remains the UNescaped model name (backticks are source-only; the wire string is a literal, verify it is not derived from the escaped identifier). Reuse the existing helper — no new keyword list. dependsOn T1."
- acceptance: Generating Swift for the reserved-word test model (T2) with codecs-by-default produces an sw-stub that compiles with `swift build` including a branch/enum-case named `Default` (emitted as backtick-escaped case); existing sw regular/wrapped tests pass; the JSON discriminator wire string for the branch equals the model name `Default` (unescaped), UEBA index unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T8 — planned

- createdAt: 2026-06-09T20:48:29.604Z
- updatedAt: 2026-06-09T20:53:12.000Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Rust: close the residual type/variant-name gap and handle non-r#-escapable keywords (self/super/crate/Self)"
- description: "Rust PARTIALLY escapes (H2, lowest risk): escapeRustKeyword (`r#`) + escapeRustModuleName are applied to field/fn/module names (RsDefnTranslator:1533-1546). Residual gap: type/struct/enum/trait + VARIANT names are emitted as bare `tid.name.name.capitalize` (RsTypeTranslator:155) with no escape. Rust keywords are lowercase so `.capitalize` dodges most, but: (a) route type/variant name emission through escapeRustKeyword for completeness; (b) handle the keywords that `r#` CANNOT escape — `self`/`super`/`crate`/`Self` — via RENAME (escapeRustModuleName already does `_`-suffix / `in`→`input`; extend an analogous rename for these in type/field positions), since `r#self` is itself illegal. For any RENAME, preserve the wire key with the existing `#[serde(rename = \"<modelName>\")]` discipline (this is the template the other RENAME backends mirror per T1). dependsOn T1."
- acceptance: "Generating Rust for the reserved-word test model (T2) with codecs-by-default produces an rs-stub that compiles with `cargo build` including fields named `self`/`super`/`crate` (renamed with `#[serde(rename)]`) and any keyword-named type/variant; existing rs regular/wrapped tests pass; serde JSON round-trip uses the original model names as keys; UEBA unchanged."
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T9 — planned

- createdAt: 2026-06-09T20:48:39.748Z
- updatedAt: 2026-06-09T20:53:13.397Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Java: RENAME keyword-colliding identifiers and preserve wire keys via @JsonProperty"
- description: "Java has NO escape syntax (H2) — must RENAME. `default` is a Java keyword (same trigger as C#). Add an `escapeJvKeyword` helper that renames (e.g. trailing `_`) any model-derived identifier equal to a Java keyword, and apply it to: field names, getter/method names, class/enum names, package segments (verbatim today), and the UEBA codec branch capture var (JvUEBACodecGenerator:182 `castedName = branchName.toLowerCase` → :199 `if (value instanceof $adtRef $castedName)`). The JSON codec uses a fixed `branchVal` (safe). CRITICAL wire-key preservation (per T1 contract): when a FIELD is renamed, annotate it with Jackson `@JsonProperty(\"<originalModelName>\")` so the on-wire JSON key is unchanged. UEBA is positional — rename is wire-neutral there. dependsOn T1."
- acceptance: Generating Java for the reserved-word test model (T2) with codecs-by-default produces a jv-stub that compiles (`mvn`/javac) including fields named `default`/`class`/`final`/`void` (renamed) and a branch capture for `Default`; existing jv regular/wrapped tests pass; Jackson JSON serialization emits the ORIGINAL model names as keys (verified by conv-test cross-language round-trip); UEBA byte layout unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T10 — planned

- createdAt: 2026-06-09T20:48:49.079Z
- updatedAt: 2026-06-09T20:53:14.842Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Python: RENAME keyword-colliding identifiers and preserve wire keys in the hand-rolled codec"
- description: "Python has NO escape syntax (H2) — must RENAME (PEP8 trailing `_`). Add an `escapePyKeyword` helper covering Python keywords + soft keywords (`class`/`def`/`import`/`lambda`/`is`/`in`/`for`/etc.) AND the literals `none`/`true`/`false` (which would shadow `None`/`True`/`False`). Apply to field names (PyDefnTranslator:540 `q\"$fieldName: $fieldType\"` verbatim today), method names, class/enum names (`.capitalize` today). No branch-lowercase analog (ADT dispatch via isinstance). CRITICAL wire-key preservation (per T1): Python codecs are HAND-ROLLED — the codec must emit the ORIGINAL model name as the JSON dict key while the Python attribute is the renamed identifier (maintain a field-name→wire-key mapping in the generated encode/decode). UEBA is positional. dependsOn T1."
- acceptance: Generating Python for the reserved-word test model (T2) with codecs-by-default produces a py-stub that imports and passes its checks (py-stub test action) including fields/attrs named `class`/`def`/`import`/`is` (renamed) and `none`/`true`/`false`; the JSON codec emits the ORIGINAL model names as dict keys (cross-language conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T11 — planned

- createdAt: 2026-06-09T20:49:07.245Z
- updatedAt: 2026-06-09T20:53:16.792Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "TypeScript: wire up the dead escapeTsKeyword and preserve wire keys via bracket-string access"
- description: "TypeScript has a COMPLETE escaper that is DEAD: escapeTsKeyword (TsTypeTranslator:310-319, rename trailing `_`) has ZERO callers tree-wide (H2). Wire it through every model-derived identifier emission: type/class names, interface field/getter names, params, enum members, and the UEBA codec `const <field>` local (TsUEBACodecGenerator:169). ADT branch dispatch is by instanceof/index (no lowercase capture). CRITICAL wire-key preservation (per T1): when a field/property is renamed, the function-based JSON codec must read/write the ORIGINAL model name via bracket-string property access (`obj[\"<modelName>\"]`) so the on-wire JSON key is unchanged — the renamed identifier is used only for the TS object property, not the wire key. (Alternatively keep the object property as the model name via bracket-string and only rename where a bare-identifier position requires it.) UEBA is positional. dependsOn T1."
- acceptance: Generating TypeScript for the reserved-word test model (T2) with codecs-by-default produces a ts-stub that compiles with `tsc` (no errors) including fields/types/enum-members named `default`/`class`/`type`/`is`/`function`; existing ts regular/wrapped tests pass; JSON codec emits/reads the ORIGINAL model names as keys (conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T12 — planned

- createdAt: 2026-06-09T20:49:17.577Z
- updatedAt: 2026-06-09T20:53:18.107Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Dart: wire existing escapeDartKeyword through member emission and preserve wire keys via explicit map keys"
- description: "Dart PARTIALLY escapes (H2): escapeDartKeyword (rename `_`) is applied to TYPE names via the DtBaboonTranslator render pass (:272-301) + DtServiceWiringTranslator, but field/getter/method names BYPASS it — DtDefnTranslator:307/318/538/571 + DtJsonCodecGenerator:164/169 emit `f.name.name` verbatim. Wire the existing helper through these member-emission sites (field decls, constructor params `this.<f>`, getters, methods). Dart keyword triggers for members: `default`/`class`/`final`/`void`/`switch`/`is`/`in`. ADT codec uses a fixed `branchVal` (safe). CRITICAL wire-key preservation (per T1): the dart:convert JSON codec must use the ORIGINAL model name as the explicit map key literal (`json['<modelName>']` / `map['<modelName>'] = ...`), not the renamed getter, so the on-wire key is unchanged. UEBA is positional. dependsOn T1."
- acceptance: Generating Dart for the reserved-word test model (T2) with codecs-by-default produces a dt-stub that compiles/analyzes clean (`dart analyze`/test action) including fields/getters named `default`/`class`/`final`/`void`/`is`/`in` (renamed); existing dt regular/wrapped tests pass; the JSON codec uses ORIGINAL model names as map keys (conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]

## M5

### T13 — planned

- createdAt: 2026-06-09T20:49:32.170Z
- updatedAt: 2026-06-09T20:53:19.546Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Wire reserved-word coverage into the mdl test matrix (generation + per-language compile lanes)
- description: "Ensure the reserved-word test model (T2) is GENERATED and COMPILED by the existing matrix so the per-language fixes are guarded against regression. Two options to evaluate and pick the lowest-friction (.mdl/defs/tests.md): (a) since T2's model lives in the shared model-dir `baboon-compiler/src/test/resources/baboon/`, the existing `test-gen-regular-adt` / `test-gen-wrapped-adt` actions already scan it and generate it into every per-language stub — verify the keyword-named types appear in generated output and the existing `test-<lang>-{regular,wrapped}` compile lanes pick them up automatically (preferred: zero new actions, mirrors recursive-ok). (b) If codecs-by-default is required to surface the codec-capture collisions and the regular/wrapped lanes do NOT pass `--generate-{json,ueba}-codecs-by-default=true`, add a dedicated `test-gen-reserved` action + per-language `test-<lang>-reserved` lanes (mirroring the MCP-overlay pattern) pointing at the model with codecs forced on. Add the new lanes (if any) to `mdl :test` and `mdl :ci`. Also add a JVM codegen-shape unit test if the shape is assertable (hidden .jvm/src/test, per MEMORY). dependsOn all per-language fixes T3-T12."
- acceptance: "The reserved-word model is generated into all 9 language stubs by `mdl :test` and each per-language compile lane includes it; if dedicated lanes were added they appear in `.mdl/defs/tests.md` and in the `:test`/`:ci` target lists; running the relevant `test-gen-*` action shows keyword-named branches/fields/enum-members in generated output for every backend."
- suggestedModel: standard
- dependsOn: ["T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12"]
- ledgerRefs: ["goals:G1","defects:D1"]

### T14 — planned

- createdAt: 2026-06-09T20:49:45.172Z
- updatedAt: 2026-06-09T20:49:45.172Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Full cross-language CI verification: green matrix + wire-key cross-language round-trip"
- description: "Final acceptance gate for G1. Run the complete CI path `mdl :ci` (which runs `sbt +compile` for JVM+JS, the full per-language test matrix, smoke, acceptance, and service-acceptance) and confirm GREEN with the reserved-word coverage (T13) included. Specifically verify two properties: (1) COMPILES — generated keyword-named code compiles/typechecks in all 9 targets (the per-language lanes from T13). (2) WIRE-COMPATIBLE — the cross-language serialization acceptance matrix (`test-acceptance`, conv-test-{cs,sc,py,rs,ts,kt,jv,dt,sw}, 9×9 JSON+UEBA) passes for a payload whose model has keyword-named fields, proving every backend reads/writes the SAME on-wire key (the original model name) despite per-language source-identifier escaping/renaming. Watch the documented failure modes: `sbt clean` before compile if any runtime resource changed; the 3-site exhaustive-match update if any TyperIssue case was added; local Kotlin OOM (`mdl --seq`). Mark defect D1 resolution-ready (do NOT close the goal — that is user-driven). dependsOn T13."
- acceptance: "`mdl :ci` completes GREEN with reserved-word coverage active; the conv-test cross-language acceptance matrix passes for a keyword-named-field payload (every language↔language JSON and UEBA round-trip succeeds with identical wire keys = original model names); no regression in any pre-existing lane. The original D1 repro (`adt AvatarItem { data Default {} ... }` + Holder root, codecs-by-default) now produces compiling C# (`@default`) and compiling output in all other backends."
- suggestedModel: frontier
- dependsOn: ["T13"]
- ledgerRefs: ["goals:G1","defects:D1"]

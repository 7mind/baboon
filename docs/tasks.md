---
ledger: tasks
counters:
  milestone: 0
  item: 28
archives: []
---

# tasks

## M3

### T1 — done

- createdAt: 2026-06-09T20:47:06.854Z
- updatedAt: 2026-06-09T21:12:31.077Z
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
- resultCommit: aa324e53
- completion: "Committed design doc docs/drafts/20260609-g1-wire-key-contract-and-site-inventory.md: wire-key invariant for the 4 RENAME backends + 9-backend identifier-emission site inventory."
- sessionLogs: ["docs/logs/20260609-210918-aa2c6fb6e62d45c77.md","docs/logs/20260609-210918-afce4af53bd336852.md"]

### T2 — done

- createdAt: 2026-06-09T20:47:26.002Z
- updatedAt: 2026-06-09T21:19:51.421Z
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
- resultCommit: b3f48fb6
- completion: Added baboon-compiler/src/test/resources/baboon/reserved-words-ok/reserved.baboon (enum KindEnum, adt AvatarItem w/ 8 keyword branches incl. Default, root Holder w/ 19 keyword fields incl. true/false/self/super/crate); parse+typecheck verified exit 0.
- sessionLogs: ["docs/logs/20260609-210918-a90169a141d378671.md","docs/logs/20260609-211657-a4134e0e0efb254d0.md","docs/logs/20260609-210918-abc7909e0fe01a7d1.md","docs/logs/20260609-211657-a6ee12345aa4afd7b.md"]

## M4

### T3 — done

- createdAt: 2026-06-09T20:47:38.592Z
- updatedAt: 2026-06-09T21:42:34.613Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "C# (minimal symptom fix): @-escape the two ADT branch codec capture variables"
- description: "Resolve the REPRODUCED reported symptom (D1/H1). At CSJsonCodecGenerator.scala:158 (`val branchNameRef = q\"${branchName.toLowerCase}\"`, used as the pattern-capture var at :177 `if (value is $fqBranch $branchNameRef)` and :162 `Encode(ctx, $branchNameRef)`) and CSUEBACodecGenerator.scala:196 (`val castedName = branchName.toLowerCase`, used at :199), apply a C# verbatim-identifier escape so a lowercased branch name that is a C# keyword (e.g. `default`) is emitted as `@default`. Add a `escapeCsKeyword`/`@`-prefix helper (or reuse an existing C# identifier helper if one exists) covering the full C# keyword set, and apply it to the capture variable. Confirm BOTH the non-wrapped and `wrappedAdtBranchCodecs` arms (:164/:198) are correct — the wrapped arm uses `wire`/cName and may not need the capture var, but verify. This is the minimal blast-radius fix; the general C# identifier pass is T4. dependsOn T1."
- acceptance: "Generating C# for the reserved-word test model (T2) with `--generate-{json,ueba}-codecs-by-default=true` produces `if (value is AvatarItem.Default @default)` (or equivalent escaped capture) instead of the bare `default`; the cs-stub project containing the generated code compiles with `dotnet build` (no CS1026); pre-existing non-colliding branches still compile. JSON/UEBA wire output for the branch is unchanged (capture var is local, not on-wire)."
- suggestedModel: frontier
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 75191d49
- completion: Added CSTypes.escapeCsKeyword (@-verbatim, full 77-keyword C# set), applied to ADT-branch capture var in CSJsonCodecGenerator + CSUEBACodecGenerator. Branch `Default` now emits `@default` (was CS1026). dotnet build clean + negative control confirmed. Resolves the reported D1 symptom.
- sessionLogs: ["docs/logs/20260609-210918-a70e8f3c745ee46fe.md","docs/logs/20260609-214140-af3a9491b3c67864c.md"]

### T4 — done

- createdAt: 2026-06-09T20:47:48.860Z
- updatedAt: 2026-06-10T00:11:26.039Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "C# (general pass): @-escape all model-derived identifiers across the C# translator"
- description: "Complete fix for the C# backend beyond the two capture sites (T3). Using the C# `@`-verbatim helper from T3, route EVERY model-derived C# identifier emission through it: type names, ADT-branch nested type names, enum member names, DTO field/property names, method names, constructor parameters, codec locals, and namespace segments — per the site inventory from T1. C# is case-sensitive so PascalCase type names rarely collide, but lowercased/camelCased members and params can; the `@` prefix is wire-neutral for C# (Newtonsoft serializes the property name without `@`, but verify JSON property names use the model name via attribute or are otherwise wire-stable). Where C# emits a JSON property name derived from a renamed/escaped identifier, ensure the on-wire key remains the original model name. dependsOn T3 (reuses its helper)."
- acceptance: Generating C# for the reserved-word test model (T2) with codecs-by-default produces a cs-stub that compiles with `dotnet build` for ALL identifier kinds (keyword-named fields, params, enum members, methods), not just the branch capture var; cross-language JSON round-trip (conv-test-cs) preserves wire keys equal to the model names; existing cs regular/wrapped tests still pass.
- suggestedModel: frontier
- dependsOn: ["T3"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 357102b6
- completion: "C# general keyword-escaping pass: all model-derived C# identifier sites routed through escapeCsKeyword (8 files); wire keys preserved; byte-identical. C# keyword scope COMPLETE. (dotnet-build-green for reserved-words-ok blocked by separate D7 System.Type shadowing.)"
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### T5 — done

- createdAt: 2026-06-09T20:47:59.203Z
- updatedAt: 2026-06-09T22:08:17.649Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Scala: backtick-escape all model-derived identifiers (no escaping exists today)"
- description: "Scala has NO reserved-word escaping anywhere (H2). Add an `escapeScKeyword` helper (backtick-quote `` `name` `` for the Scala keyword set — `type`/`val`/`object`/`class`/`match`/`def`/`trait`/`implicit`/etc.; note `default` is NOT a Scala keyword) and route all model-derived identifier emissions through it: the terminal renderer (ScBaboonTranslator:283-290 emits names raw), DTO field names (ScDefnTranslator:307), and the JSON codec branch capture var (ScJsonCodecGenerator:201 `branchNameRef = branchName.toLowerCase` → :211 `case $branchNameRef: $fqBranch =>`). Backtick-quoting is wire-neutral (the backticks are not part of the identifier text). Prefer hooking the centralized terminal renderer once if all names flow through it; add per-site fixes for the codec capture var. dependsOn T1."
- acceptance: Generating Scala for the reserved-word test model (T2) with codecs-by-default produces an sc-stub that compiles with `sbt +compile` (JVM+JS) including fields/branches named `type`/`val`/`object`/`match`; existing sc regular/wrapped tests pass; JSON/UEBA wire keys equal the model names (backtick is source-only).
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 36a32e52
- completion: ScTypes.escapeScKeyword (backtick) routed through case-class params, codec accessors/binders/ctor-args, ADT branch capture vars, conversion vars. Wire-neutral. sbt baboonJVM/test 602/602. (Class-shadowing → D3, orthogonal.)
- sessionLogs: ["docs/logs/20260609-220620-adcb227c4d5dd8da1.md","docs/logs/20260609-220620-a45bf9d85e30b139e.md"]

### T6 — done

- createdAt: 2026-06-09T20:48:08.169Z
- updatedAt: 2026-06-09T22:08:20.939Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Kotlin: backtick-escape all model-derived identifiers (no escaping exists today)"
- description: "Kotlin has NO reserved-word escaping anywhere (H2). Add an `escapeKtKeyword` helper (backtick-quote `` `name` `` for the Kotlin hard-keyword set — `object`/`is`/`when`/`fun`/`val`/`class`/`in`/`when`/etc.) and route all model-derived identifier emissions through it: type/field/package names (verbatim today) and the UEBA codec branch capture var (KtUEBACodecGenerator:185 `castedName = branchName.toLowerCase` → :204 `val $castedName = instance`). Backtick is wire-neutral. Note the Kotlin JSON codec uses Jackson — if a field is backtick-escaped its on-wire key is unaffected (Kotlin backtick identifiers serialize under the identifier text minus backticks); confirm the wire key equals the model name and add `@JsonProperty` only if Jackson would otherwise diverge. Cover both kt-stub and kt-stub-kmp. dependsOn T1."
- acceptance: Generating Kotlin for the reserved-word test model (T2) with codecs-by-default produces kt-stub AND kt-stub-kmp projects that compile (gradle) including fields/branches named `object`/`is`/`when`/`fun`/`val`; existing kt regular/wrapped (+kmp) tests pass; JSON/UEBA wire keys equal the model names.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: a24eeff7
- completion: KtTypeTranslator.escapeKtKeyword (backtick, 28 hard keywords) routed through field params, codec accessors/ctor-args, UEBA ADT branch capture, conversion vars; covers kt-stub + kmp. Jackson wire keys unescaped. sbt baboonJVM/test 602/602. (Class-shadowing → D3; service-wiring asymmetry → D4.)
- sessionLogs: ["docs/logs/20260609-220620-a3e0a7c5d864fab08.md","docs/logs/20260609-220620-aef8c4956d229a5b8.md"]

### T7 — done

- createdAt: 2026-06-09T20:48:17.897Z
- updatedAt: 2026-06-10T00:11:28.865Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Swift: route ADT-branch and enum-case names through the existing escapeSwiftKeyword (backticks)"
- description: "Swift PARTIALLY escapes (H2): escapeSwiftKeyword (backticks) is applied via the centralized render pass (SwBaboonTranslator:331-340) for type/field names, but ADT-branch case names + enum case names are built as RAW strings and bypass it. Fix the gap sites: SwDefnTranslator.scala:889-894 (`caseName = memberName.head.toLower + tail`, emitted as `case $caseName(...)`) and the codec sites SwJsonCodecGenerator:109-128 + SwUEBACodecGenerator:150-178 — route `caseName` through escapeSwiftKeyword so branch `Default` emits `case `default`(...)` not `case default(...)`. Confirm the JSON codec's on-wire case key (the discriminator string) remains the UNescaped model name (backticks are source-only; the wire string is a literal, verify it is not derived from the escaped identifier). Reuse the existing helper — no new keyword list. dependsOn T1."
- acceptance: Generating Swift for the reserved-word test model (T2) with codecs-by-default produces an sw-stub that compiles with `swift build` including a branch/enum-case named `Default` (emitted as backtick-escaped case); existing sw regular/wrapped tests pass; the JSON discriminator wire string for the branch equals the model name `Default` (unescaped), UEBA index unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 427dfa42
- completion: Swift ADT-branch + enum-case names escaped via escapeSwiftKeyword; JSON wire strings + UEBA ordinals preserved; +2 supporting fixes (Type metatype dot-shorthand, _self init param). swift build of reserved-words-ok green.
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### T8 — done

- createdAt: 2026-06-09T20:48:29.604Z
- updatedAt: 2026-06-10T00:11:22.740Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Rust: close the residual type/variant-name gap and handle non-r#-escapable keywords (self/super/crate/Self)"
- description: "Rust PARTIALLY escapes (H2, lowest risk): escapeRustKeyword (`r#`) + escapeRustModuleName are applied to field/fn/module names (RsDefnTranslator:1533-1546). Residual gap: type/struct/enum/trait + VARIANT names are emitted as bare `tid.name.name.capitalize` (RsTypeTranslator:155) with no escape. Rust keywords are lowercase so `.capitalize` dodges most, but: (a) route type/variant name emission through escapeRustKeyword for completeness; (b) handle the keywords that `r#` CANNOT escape — `self`/`super`/`crate`/`Self` — via RENAME (escapeRustModuleName already does `_`-suffix / `in`→`input`; extend an analogous rename for these in type/field positions), since `r#self` is itself illegal. For any RENAME, preserve the wire key with the existing `#[serde(rename = \"<modelName>\")]` discipline (this is the template the other RENAME backends mirror per T1). dependsOn T1."
- acceptance: "Generating Rust for the reserved-word test model (T2) with codecs-by-default produces an rs-stub that compiles with `cargo build` including fields named `self`/`super`/`crate` (renamed with `#[serde(rename)]`) and any keyword-named type/variant; existing rs regular/wrapped tests pass; serde JSON round-trip uses the original model names as keys; UEBA unchanged."
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: aa034c78
- completion: "Rust: self/super/crate renamed (_-suffix) + #[serde(rename)]; reserved-inactive keywords added; escapeRustTypeName on type/variant names. cargo build green; serde round-trip confirmed; byte-identical existing fixtures."
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### T9 — done

- createdAt: 2026-06-09T20:48:39.748Z
- updatedAt: 2026-06-09T21:43:00.301Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Java: RENAME keyword-colliding identifiers and preserve wire keys via @JsonProperty"
- description: "Java has NO escape syntax (H2) — must RENAME. `default` is a Java keyword (same trigger as C#). Add an `escapeJvKeyword` helper that renames (e.g. trailing `_`) any model-derived identifier equal to a Java keyword, and apply it to: field names, getter/method names, class/enum names, package segments (verbatim today), and the UEBA codec branch capture var (JvUEBACodecGenerator:182 `castedName = branchName.toLowerCase` → :199 `if (value instanceof $adtRef $castedName)`). The JSON codec uses a fixed `branchVal` (safe). CRITICAL wire-key preservation (per T1 contract): when a FIELD is renamed, annotate it with Jackson `@JsonProperty(\"<originalModelName>\")` so the on-wire JSON key is unchanged. UEBA is positional — rename is wire-neutral there. dependsOn T1."
- acceptance: Generating Java for the reserved-word test model (T2) with codecs-by-default produces a jv-stub that compiles (`mvn`/javac) including fields named `default`/`class`/`final`/`void` (renamed) and a branch capture for `Default`; existing jv regular/wrapped tests pass; Jackson JSON serialization emits the ORIGINAL model names as keys (verified by conv-test cross-language round-trip); UEBA byte layout unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 137bf305
- completion: "Added JvTypeTranslator.escapeJvKeyword (full JLS set), applied to DTO components, method names, identifier accessor, JSON+UEBA field accessors, UEBA ADT branch capture. JSON wire-key literals + positional UEBA preserved (original model names). Verified javac clean + sbt baboonJVM/test 605 green. NOTE: worker's stale-base emission test + model variant NOT merged (kept T2's canonical model); matrix coverage via T13. Out-of-scope defect D2 filed (renderOwner ADT package segment)."
- sessionLogs: ["docs/logs/20260609-210918-a55935b64519dbee2.md","docs/logs/20260609-214140-a2cea0e21e7bdf37d.md"]

### T10 — done

- createdAt: 2026-06-09T20:48:49.079Z
- updatedAt: 2026-06-10T10:31:46.424Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Python: RENAME keyword-colliding identifiers and preserve wire keys in the hand-rolled codec"
- description: "Python has NO escape syntax (H2) — must RENAME (PEP8 trailing `_`). Add an `escapePyKeyword` helper covering Python keywords + soft keywords (`class`/`def`/`import`/`lambda`/`is`/`in`/`for`/etc.) AND the literals `none`/`true`/`false` (which would shadow `None`/`True`/`False`). Apply to field names (PyDefnTranslator:540 `q\"$fieldName: $fieldType\"` verbatim today), method names, class/enum names (`.capitalize` today). No branch-lowercase analog (ADT dispatch via isinstance). CRITICAL wire-key preservation (per T1): Python codecs are HAND-ROLLED — the codec must emit the ORIGINAL model name as the JSON dict key while the Python attribute is the renamed identifier (maintain a field-name→wire-key mapping in the generated encode/decode). UEBA is positional. dependsOn T1."
- acceptance: Generating Python for the reserved-word test model (T2) with codecs-by-default produces a py-stub that imports and passes its checks (py-stub test action) including fields/attrs named `class`/`def`/`import`/`is` (renamed) and `none`/`true`/`false`; the JSON codec emits the ORIGINAL model names as dict keys (cross-language conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: bba2574c
- completion: Python keyword RENAME (PEP8 _) via PyKeywords.escapePyKeyword; wire keys preserved (pydantic alias); enum True_/False_. Verified JSON/UEBA round-trip preserves original keys; 602 green.
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

### T11 — done

- createdAt: 2026-06-09T20:49:07.245Z
- updatedAt: 2026-06-10T10:31:48.774Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "TypeScript: wire up the dead escapeTsKeyword and preserve wire keys via bracket-string access"
- description: "TypeScript has a COMPLETE escaper that is DEAD: escapeTsKeyword (TsTypeTranslator:310-319, rename trailing `_`) has ZERO callers tree-wide (H2). Wire it through every model-derived identifier emission: type/class names, interface field/getter names, params, enum members, and the UEBA codec `const <field>` local (TsUEBACodecGenerator:169). ADT branch dispatch is by instanceof/index (no lowercase capture). CRITICAL wire-key preservation (per T1): when a field/property is renamed, the function-based JSON codec must read/write the ORIGINAL model name via bracket-string property access (`obj[\"<modelName>\"]`) so the on-wire JSON key is unchanged — the renamed identifier is used only for the TS object property, not the wire key. (Alternatively keep the object property as the model name via bracket-string and only rename where a bare-identifier position requires it.) UEBA is positional. dependsOn T1."
- acceptance: Generating TypeScript for the reserved-word test model (T2) with codecs-by-default produces a ts-stub that compiles with `tsc` (no errors) including fields/types/enum-members named `default`/`class`/`type`/`is`/`function`; existing ts regular/wrapped tests pass; JSON codec emits/reads the ORIGINAL model names as keys (conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: fa834e94
- completion: "TypeScript: wired dead escapeTsKeyword at binding positions; wire keys original; tsc green. Surfaced low D9 (type/enum-name escape, latent)."
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

### T12 — done

- createdAt: 2026-06-09T20:49:17.577Z
- updatedAt: 2026-06-10T10:31:51.605Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Dart: wire existing escapeDartKeyword through member emission and preserve wire keys via explicit map keys"
- description: "Dart PARTIALLY escapes (H2): escapeDartKeyword (rename `_`) is applied to TYPE names via the DtBaboonTranslator render pass (:272-301) + DtServiceWiringTranslator, but field/getter/method names BYPASS it — DtDefnTranslator:307/318/538/571 + DtJsonCodecGenerator:164/169 emit `f.name.name` verbatim. Wire the existing helper through these member-emission sites (field decls, constructor params `this.<f>`, getters, methods). Dart keyword triggers for members: `default`/`class`/`final`/`void`/`switch`/`is`/`in`. ADT codec uses a fixed `branchVal` (safe). CRITICAL wire-key preservation (per T1): the dart:convert JSON codec must use the ORIGINAL model name as the explicit map key literal (`json['<modelName>']` / `map['<modelName>'] = ...`), not the renamed getter, so the on-wire key is unchanged. UEBA is positional. dependsOn T1."
- acceptance: Generating Dart for the reserved-word test model (T2) with codecs-by-default produces a dt-stub that compiles/analyzes clean (`dart analyze`/test action) including fields/getters named `default`/`class`/`final`/`void`/`is`/`in` (renamed); existing dt regular/wrapped tests pass; the JSON codec uses ORIGINAL model names as map keys (conv-test round-trip passes); UEBA unchanged.
- suggestedModel: standard
- dependsOn: ["T1","T2"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: c958f18c
- completion: "Dart: wired escapeDartKeyword through member emission; wire keys original; +UEBA castedName escape +Object/Type stdlib-shadow rename. dart analyze clean; non-keyword byte-identical."
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

## M5

### T13 — done

- createdAt: 2026-06-09T20:49:32.170Z
- updatedAt: 2026-06-10T10:54:03.657Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Wire reserved-word coverage into the mdl test matrix (generation + per-language compile lanes)
- description: "Ensure the reserved-word test model (T2) is GENERATED and COMPILED by the existing matrix so the per-language fixes are guarded against regression. Two options to evaluate and pick the lowest-friction (.mdl/defs/tests.md): (a) since T2's model lives in the shared model-dir `baboon-compiler/src/test/resources/baboon/`, the existing `test-gen-regular-adt` / `test-gen-wrapped-adt` actions already scan it and generate it into every per-language stub — verify the keyword-named types appear in generated output and the existing `test-<lang>-{regular,wrapped}` compile lanes pick them up automatically (preferred: zero new actions, mirrors recursive-ok). (b) If codecs-by-default is required to surface the codec-capture collisions and the regular/wrapped lanes do NOT pass `--generate-{json,ueba}-codecs-by-default=true`, add a dedicated `test-gen-reserved` action + per-language `test-<lang>-reserved` lanes (mirroring the MCP-overlay pattern) pointing at the model with codecs forced on. Add the new lanes (if any) to `mdl :test` and `mdl :ci`. Also add a JVM codegen-shape unit test if the shape is assertable (hidden .jvm/src/test, per MEMORY). dependsOn all per-language fixes T3-T12."
- acceptance: "The reserved-word model is generated into all 9 language stubs by `mdl :test` and each per-language compile lane includes it; if dedicated lanes were added they appear in `.mdl/defs/tests.md` and in the `:test`/`:ci` target lists; running the relevant `test-gen-*` action shows keyword-named branches/fields/enum-members in generated output for every backend."
- suggestedModel: standard
- dependsOn: ["T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12"]
- ledgerRefs: ["goals:G1","defects:D1"]
- resultCommit: 9478a082
- completion: Reserved-word matrix coverage wired via option (a) (existing regular/wrapped lanes scan shared dir with codecs-on, all 9 backends); documented + ReservedWordsScalaEmissionTest added (green).
- sessionLogs: ["docs/logs/20260610-105325-T23-T24-T13-batch.md"]

### T14 — wip

- createdAt: 2026-06-09T20:49:45.172Z
- updatedAt: 2026-06-10T10:55:07.520Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Full cross-language CI verification: green matrix + wire-key cross-language round-trip"
- description: "Final acceptance gate for G1. Run the complete CI path `mdl :ci` (which runs `sbt +compile` for JVM+JS, the full per-language test matrix, smoke, acceptance, and service-acceptance) and confirm GREEN with the reserved-word coverage (T13) included. Specifically verify two properties: (1) COMPILES — generated keyword-named code compiles/typechecks in all 9 targets (the per-language lanes from T13). (2) WIRE-COMPATIBLE — the cross-language serialization acceptance matrix (`test-acceptance`, conv-test-{cs,sc,py,rs,ts,kt,jv,dt,sw}, 9×9 JSON+UEBA) passes for a payload whose model has keyword-named fields, proving every backend reads/writes the SAME on-wire key (the original model name) despite per-language source-identifier escaping/renaming. Watch the documented failure modes: `sbt clean` before compile if any runtime resource changed; the 3-site exhaustive-match update if any TyperIssue case was added; local Kotlin OOM (`mdl --seq`). Mark defect D1 resolution-ready (do NOT close the goal — that is user-driven). dependsOn T13."
- acceptance: "`mdl :ci` completes GREEN with reserved-word coverage active; the conv-test cross-language acceptance matrix passes for a keyword-named-field payload (every language↔language JSON and UEBA round-trip succeeds with identical wire keys = original model names); no regression in any pre-existing lane. The original D1 repro (`adt AvatarItem { data Default {} ... }` + Holder root, codecs-by-default) now produces compiling C# (`@default`) and compiling output in all other backends."
- suggestedModel: frontier
- dependsOn: ["T13"]
- ledgerRefs: ["goals:G1","defects:D1"]

## M7

### T15 — done

- createdAt: 2026-06-09T22:24:56.031Z
- updatedAt: 2026-06-09T22:46:23.193Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D3: fully-qualify the stdlib `Class` reference in baboonAdtType metadata across the 3 JVM-family domain-tree-tools"
- description: |
    Fix the confirmed Class-shadowing root cause. The per-ADT metadata field `baboonAdtType` references stdlib `java.lang.Class` by SHORT predef name; a model ADT branch/type named `Class` (present in reserved-words-ok) shadows it -> compile errors in generated Scala/Kotlin/Java.
    
    Fix at the predef definition where possible so the rendered reference is fully-qualified and non-shadowable:
    - Java: translator/java/JvTypes.scala:104 `javaClass = JvType(javaLangPkg, "Class", predef = true)`. Make it render fully-qualified `java.lang.Class` (drop predef short-name rendering for this ref, or override at the emission site translator/java/JvDomainTreeTools.scala:67 which emits `$javaClass<?> baboonAdtType`). Goal: emitted type is `java.lang.Class<?>`.
    - Kotlin: translator/kotlin/KtTypes.scala:63-67 `javaClass` — ONLY the JVM (non-multiplatform) arm at :66 `KtType(javaLangPkg, "Class", predef = true)` is defective; the multiplatform arm at :64 already uses FQ-able `kotlin.reflect.KClass`. Make the JVM arm render `java.lang.Class`. Emission site KtDomainTreeTools.scala:70 `val baboonAdtType: $javaClass<*>` should yield `java.lang.Class<*>`.
    - Scala: ScDomainTreeTools (emits `Class[?]`, per D3 rootCause). Resolve its exact path under translator/ (grep for ScDomainTreeTools / baboonAdtType), make the reference render `_root_.java.lang.Class[?]`.
    
    Distinct from D1: `Class` is not a keyword, so do NOT route through any escapeKeyword path. C# is unaffected (typeof->System.Type) and non-JVM backends have no such field — do not touch them. OPTIONAL broader audit (note, do not over-scope): JvTypes jvString:100/jvObject:101 and any `Type` predef are also short-named and could shadow under analogous model names; if cheap, make stdlib predefs render FQ at the metadata-emission site, otherwise leave as a follow-up note.
- acceptance: "Generated Scala, Kotlin, and Java for the reserved-words-ok model (baboon-compiler/src/test/resources/baboon/reserved-words-ok/reserved.baboon, which contains a `Class` ADT branch) each emit the metadata field as a fully-qualified `java.lang.Class` (`_root_.java.lang.Class[?]` in Scala) and compile without the ~32 shadowing errors previously seen in AvatarItem.kt. Confirmed by the dedicated verification task. C# output is byte-identical to pre-change baseline."
- suggestedModel: frontier
- ledgerRefs: ["goals:G2","defects:D3"]
- resultCommit: e4cc800b
- completion: FQ java.lang.Class in baboonAdtType across Jv/Kt/Sc domain-tree-tools (via .fullyQualified). D3 Class-shadowing fixed (Java 8→0 errors). Surfaced sibling D5 (Object-shadowing in equals).
- sessionLogs: ["docs/logs/20260609-224538-T15-worker-reviewer.md"]

### T16 — done

- createdAt: 2026-06-09T22:25:11.759Z
- updatedAt: 2026-06-09T22:46:25.554Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D2: escape the ADT-name package segment in Java JvTypeTranslator.renderOwner"
- description: |
    In translator/java/JvTypeTranslator.scala:193 the `Owner.Adt` arm `case Owner.Adt(id) => renderOwner(id.owner) :+ id.name.name` appends the ADT name UNESCAPED as a package-path segment, asymmetric with the `Owner.Ns` arm at :192 which routes through `JvTypeTranslator.escapeJvKeyword(s.name.toLowerCase)`. An ADT used as a package qualifier whose (lowercased) name is a Java keyword would emit an illegal package segment.
    
    Fix: route the `Owner.Adt` arm's `id.name.name` through `JvTypeTranslator.escapeJvKeyword` to match the `Owner.Ns` arm. Preserve existing casing behavior unless the Ns arm's lowercasing is also required for package segments — follow whatever the Ns arm does so the two arms are symmetric. Low severity, latent (ADT names are conventionally capitalized so no current fixture triggers it); do not add a new fixture. Independent of T15 (different file region) — may proceed in parallel.
- acceptance: "translator/java/JvTypeTranslator.scala renderOwner Owner.Adt arm routes the ADT name segment through escapeJvKeyword (symmetric with the Owner.Ns arm). Full build + Java test matrix (mdl :build :test, or at minimum the java regular/wrapped/manual lanes) stays GREEN with output byte-identical to baseline for all existing fixtures (no fixture exercises a keyword-named ADT-as-package-qualifier)."
- suggestedModel: standard
- ledgerRefs: ["goals:G2","defects:D2"]
- resultCommit: 7e36259a
- completion: escapeJvKeyword on renderOwner Owner.Adt arm (escape-only). Byte-identical existing fixtures; 602 green.
- sessionLogs: ["docs/logs/20260609-224538-T16-worker-reviewer.md"]

### T17 — done

- createdAt: 2026-06-09T22:25:18.796Z
- updatedAt: 2026-06-09T22:46:27.619Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D4: escape Kotlin service-method wiring call sites in KtServiceWiringTranslator"
- description: |
    In translator/kotlin/KtServiceWiringTranslator.scala the wiring CALL sites emit `impl.${m.name.name}(...)` with the RAW method name (confirmed at lines 447/448 and 489/490; per the defect also the symmetric sites 669/677/699/707/767/775). T6 already escaped the method DECLARATION (`fun ´when´(...)`), so a service method named after a Kotlin hard keyword would declare the escaped name but be CALLED as `.when()` -> compile error. Pre-T6 both sides were unescaped (consistent); T6 introduced the asymmetry.
    
    Fix: route each `m.name.name` at the `impl.<method>(...)` call sites (and the symmetric `"${m.name.name}" -> {` when-branch labels if they must match an escaped identifier — verify whether the string-literal method-name key needs escaping; the dispatch key is a String literal, NOT an identifier, so it likely does NOT, but the `impl.<method>` invocation receiver DOES) through `KtTypeTranslator.escapeKtKeyword`, matching the escaped declaration from T6. Audit ALL call-site occurrences in the file (json + ueba, errors + no-errors variants) so none are missed. Low severity, latent (no current fixture has a keyword-named service method); do not add a new fixture. Independent of T15 (different file) — may proceed in parallel.
- acceptance: "Every `impl.${m.name.name}(...)` invocation in translator/kotlin/KtServiceWiringTranslator.scala routes the method name through KtTypeTranslator.escapeKtKeyword (declaration and call site now symmetric). String-literal dispatch keys left as-is unless verified to require escaping. Full build + Kotlin test matrix incl. service-wiring lanes (kt regular/wrapped + service-acceptance) stays GREEN, output byte-identical to baseline for existing fixtures."
- suggestedModel: standard
- ledgerRefs: ["goals:G2","defects:D4"]
- resultCommit: e33a1b43
- completion: All 12 kt service-wiring impl call sites escaped via escapeKtKeyword; wire strings raw. Byte-identical; 602 green. Surfaced D6 (client-stub decls).
- sessionLogs: ["docs/logs/20260609-224538-T17-worker-reviewer.md"]

### T18 — planned

- createdAt: 2026-06-09T22:25:34.431Z
- updatedAt: 2026-06-09T22:25:34.431Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Verify: reserved-words-ok generates+compiles for Scala+Kotlin+Java (closes D3, unblocks G1 T14) and full JVM-family matrix is green"
- description: |
    Verification gate for goal G2. Confirms the D3 fix (T15) closes the Class-shadowing defect and unblocks G1's T14 green-matrix gate, and that the D2 (T16) and D4 (T17) fixes did not regress the JVM family.
    
    Steps:
    1. Generate code for the reserved-words-ok model (baboon-compiler/src/test/resources/baboon/reserved-words-ok/reserved.baboon — it contains a `Class` ADT branch) for the Scala, Kotlin, and Java backends.
    2. Confirm the emitted baboonAdtType metadata field is fully-qualified (`_root_.java.lang.Class[?]` Scala / `java.lang.Class<*>` Kotlin / `java.lang.Class<?>` Java) and that each compiles — specifically that the ~32 shadowing errors previously seen in AvatarItem.kt are gone.
    3. Run the JVM-family test lanes that cover these (e.g. mdl :build :test, or at minimum the scala/kotlin/java regular+wrapped+manual + kt service lanes) and confirm GREEN.
    
    Build discipline (CLAUDE.md): sbt-git (jgit) CANNOT build inside a linked git worktree (NoWorkTreeException) — if running from a worktree, clone the repo to a real dir (e.g. /tmp/baboon-ci-clone) and run the pipeline there. No `bun run check` in this project; build/test via mdl (use --simple-log when capturing output, --seq on <16GB RAM to avoid Kotlin daemon OOM). Run `sbt clean` only if baboon-runtime resources changed (they should not for this goal).
- acceptance: Generated Scala, Kotlin, and Java for reserved-words-ok COMPILE with zero errors (the prior ~32 AvatarItem.kt Class-shadowing errors absent), the metadata field renders fully-qualified java.lang.Class in all three, and the JVM-family test matrix (scala/kotlin/java regular+wrapped+manual, kt service lanes) is GREEN. This is the evidence that D3 is closed and G1's T14 green-matrix gate is unblocked.
- suggestedModel: standard
- dependsOn: ["T15","T16","T17"]
- ledgerRefs: ["goals:G2","defects:D3"]

## M9

### T19 — done

- createdAt: 2026-06-09T22:53:25.558Z
- updatedAt: 2026-06-10T00:11:17.091Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D5 (Java): fully-qualify JVM stdlib type refs at all Java emission sites so a model type/branch named Object/String/Class/Type cannot shadow them"
- description: |
    GENERAL fix for D5 on the Java backend (generalizes T15's narrow Class fix). The defect: generated Java references java.lang stdlib types by SHORT name; a model ADT branch/type of that name shadows the stdlib type → ~15 javac errors compiling AvatarItem.java for the reserved-words-ok model (which has empty-field branches `data Object {}`, `data Class {}`, `data Type {}`).
    
    Confirmed sites (grounded against source):
    - baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/java/JvDefnTranslator.scala:431 — `public boolean equals(Object other)` emits `Object` as a BARE STRING LITERAL inside the `emptyRecordMethods` q-template (only emitted for empty-field records, i.e. exactly the `data Object {}`/`data Class {}` branches). Also :425 `public String toString()` is a bare `String` literal in the same block. Neither passes through the FQ renderer. FIX: interpolate `${jvObject.fullyQualified}` and `${jvString.fullyQualified}` (JvType.fullyQualified flips fq=true → renderer emits java.lang.Object / java.lang.String).
    - baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/java/JvTypes.scala:100-101,104 — jvString/jvObject/javaClass predefs are short-named. AUDIT every emission site that interpolates these predefs (and any other bare `Object`/`String`/`Class`/`Type` string literals in q-templates across the java/ translator package, incl. JvCodec*, service-wiring, metadata sites beyond T15's). Render them FQ wherever a model type could shadow them. Prefer the minimal, mechanical change that makes generation correct: interpolate `${predef.fullyQualified}` at the shadowing-risk sites rather than globally flipping the predef default (a global default flip would change byte-output of the pre-MCP baseline for ALL models and break golden tests — scope the FQ to the shadowing-risk emission sites, confirm no unintended baseline diff).
    
    This resolves D5's Java portion and pre-empts the String/Type siblings. Verification (model compiles) is the dependent task T-verify under G3-W2; do NOT duplicate G2's T18 — G3's verify subsumes it for this defect class.
- acceptance: "1) No bare `Object`/`String`/`Class`/`Type` stdlib type literal remains at a Java emission site that an empty-field/branch model type could shadow (grep the java/ package; each shadowing-risk ref routed through `.fullyQualified`). 2) Generated AvatarItem.java for reserved-words-ok emits `java.lang.Object`/`java.lang.String` in equals/toString. 3) `mdl :build` (sbt +compile, JVM+JS) stays GREEN — the translator change compiles and the exhaustive-match/JS sites are unaffected. 4) No unintended diff in existing golden/round-trip baselines for models WITHOUT shadowing names (the FQ is scoped to shadowing-risk sites). Full compile-of-generated-Java verification is deferred to T-verify (G3-W2)."
- suggestedModel: frontier
- ledgerRefs: ["goals:G3","defects:D5"]
- resultCommit: "32e43749"
- completion: FQ java.lang.Object/String in Java record-body templates (equals/toString). reserved-words-ok AvatarItem compiles (15→0 javac errors). Surfaced low D8 (enum parse(String) shadow).
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### T20 — done

- createdAt: 2026-06-09T22:53:39.243Z
- updatedAt: 2026-06-09T23:55:35.612Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D5 (Scala + Kotlin analogues): fully-qualify any bare stdlib type refs in Scala/Kotlin emission so reserved-words-ok (Object/Class/Type branches) compiles"
- description: |
    Sibling of the Java D5 fix, on the Scala and Kotlin backends. D5's root-cause class — generated code references a stdlib type by SHORT name, shadowed by a model type/branch of that name — is NOT inherently Java-only. The reserved-words-ok model's `Object`/`Class`/`Type` ADT branches and `object:`/`class:`/`type:` keyword fields will surface any analogous bare-stdlib-ref sites in Scala/Kotlin codegen at the T-verify compile (G3-W2).
    
    Scope: AUDIT the scala/ and kotlin/ translator packages for bare stdlib type references at emission sites that a model type/branch could shadow — in particular equals/hashCode/toString overrides, metadata fields (Scala/Kotlin analogue of the baboonAdtType site T15 fixed for Java's Class), and any `Object`/`String`/`Class`/`Any`/`Nothing`/`KClass`/`java.lang.*` short-name literal in q-templates. For Scala, a branch named `Object`/`Type` etc. is a nested type that can shadow `scala.Any`/`Predef` aliases or `java.lang.*` refs in generated equals/canEqual/Product code; for Kotlin, `Any`/`Class`/`Object`-named branches vs `kotlin.Any`/`java.lang.Class` refs (e.g. in reflective metadata). Apply the same FQ remedy used on the Java side (route the ref through the type's fully-qualified form), scoped to shadowing-risk sites only — do NOT globally change predef defaults (would diff non-shadowing baselines).
    
    If the audit finds NO analogous bare-ref site in a backend (the model already compiles for it), record that finding in the task notes and make no change for that backend — do not invent fixes. The authoritative test is T-verify: generated Scala AND Kotlin for reserved-words-ok must compile.
- acceptance: "1) scala/ and kotlin/ translator packages audited for bare stdlib type refs shadowable by a model `Object`/`Class`/`Type`/`Any`-named branch/field; every such shadowing-risk ref routed through its fully-qualified form (or, if none found for a backend, that absence recorded as the finding). 2) `mdl :build` (sbt +compile JVM+JS) stays GREEN. 3) No unintended baseline diff for models without shadowing names. Compile-of-generated-Scala+Kotlin verification is T-verify (G3-W2)."
- suggestedModel: frontier
- ledgerRefs: ["goals:G3","defects:D5"]
- completion: "AUDIT finding (zero-change): both Scala and Kotlin backends ALREADY compile reserved-words-ok (Object/Class/Type branches) given the merged T15 (Class metadata FQ in Sc/KtDomainTreeTools) + T5/T6 (keyword escaping). No analogous bare-stdlib-ref shadowing site exists in scl/ or kotlin/ (Scala uses compiler-generated case-class equals/hashCode/toString; Kotlin's one manual equals override already uses `kotlin.Any?`). Verified empirically: sc-stub `sbt compile` GREEN (-Wconf:any:error), kt-stub `gradle compileKotlin` GREEN (allWarningsAsErrors); baboonJVM/test 602/0. No source change warranted (task explicitly forbids inventing fixes). D5's Scala/Kotlin portion confirmed already-resolved."
- sessionLogs: []

### T21 — done

- createdAt: 2026-06-09T22:53:52.020Z
- updatedAt: 2026-06-10T00:11:19.922Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D6 (Kotlin): escape client-stub method declaration names at KtServiceWiringTranslator:864/882 via escapeKtKeyword"
- description: |
    Confirmed root cause (H7): in baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtServiceWiringTranslator.scala the generated client-stub DECLARATIONS at line 864 (`suspend fun ${m.name.name}(...)`) and line 882 (`suspend fun ${m.name.name}Json(...)`) emit the model method name as a RAW Kotlin identifier, NOT routed through KtTypeTranslator.escapeKtKeyword (unlike the escaped interface declaration that T6 fixed). A model service method named after a Kotlin hard keyword (e.g. `object`, `when`, `is`, `in`, `fun` — all in ktHardKeywords at KtTypeTranslator.scala:16-21) would emit an unparseable client stub.
    
    FIX: wrap the declaration identifier at :864 and :882 (the `${m.name.name}` and `${m.name.name}Json` declaration tokens) through `KtTypeTranslator.escapeKtKeyword(m.name.name)`. The `Json` variant escapes the base name then appends `Json` (the suffixed identifier as a whole is not a keyword; escape the base method name, append the literal Json suffix — confirm the backtick placement yields a valid identifier, e.g. ``object``Json` is invalid Kotlin, so escape the WHOLE `${m.name.name}Json` token if its base is a keyword — implementer to verify the correct backtick scope against the Kotlin grammar). CRITICAL: do NOT touch the transport string args at :867 and :884 (`"${m.name.name}"`) — those are on-wire method names and must stay the raw model name (per H7 + the escapeKtKeyword docstring: wire-key string literals must NOT pass through escapeKtKeyword).
    
    Low severity, LATENT: no current fixture has a keyword-named service method, and the reserved-words-ok model defines no service — so this fix has no observable codegen diff for existing fixtures. Verification is 'escape applied + nothing regresses' (T-verify), not a new keyword-named-service compile.
- acceptance: "1) KtServiceWiringTranslator.scala:864/882 declaration identifiers routed through escapeKtKeyword (base method name escaped; `Json` variant produces a grammar-valid backtick-quoted identifier when the base is a keyword). 2) Transport string args at :867/:884 remain the raw model name (unchanged). 3) `mdl :build` GREEN. 4) Existing kt service-wiring fixtures (no keyword-named methods) emit byte-identical output — no regression (verify via the kt wiring round-trip lane in T-verify)."
- suggestedModel: standard
- ledgerRefs: ["goals:G3","defects:D6"]
- resultCommit: f701d174
- completion: "Kotlin client-stub :864/:882 declarations escaped via escapeKtKeyword; transport wire strings raw. 602 green; byte-identical."
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

## M10

### T22 — planned

- createdAt: 2026-06-09T22:54:14.772Z
- updatedAt: 2026-06-09T22:54:14.772Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Verify: generated Scala + Kotlin + Java for reserved-words-ok COMPILES green (the real gate; subsumes G2's T18 for the Object/Class shadowing class)"
- description: |
    The authoritative green gate for G3. After T19 (D5-Java), T20 (D5-Scala/Kotlin), T21 (D6-kt) land, generate code for the reserved-words-ok model (baboon-compiler/src/test/resources/baboon/reserved-words-ok/reserved.baboon — has `data Object {}`/`data Class {}`/`data Type {}` empty-field branches, `enum KindEnum { Type Object ... }`, and `root data Holder` with `object:`/`class:`/`type:` keyword fields) and compile the generated Scala, Kotlin, and Java with their real toolchains. The ~15 javac errors D5 documented (method-does-not-override, `Object cannot be converted to <Branch>` in AvatarItem.java) must be GONE, and Scala/Kotlin must compile too.
    
    Mechanics (repo realities): drive via mdl, NOT a bare sbt in a worktree — sbt-git (jgit) throws NoWorkTreeException inside a linked git worktree, so a worker editing in a worktree must clone the repo to a real dir (e.g. /tmp/baboon-ci-clone) and run the pipeline there. Use `mdl --simple-log` when capturing output. The reserved-words-ok model is in the shared model-dir scanned by `--model-dir ./baboon-compiler/src/test/resources/baboon/`, so the existing per-language test-gen + compile lanes (`test-gen-regular-adt` → `test-{scala,kotlin,java}-regular`, and the wrapped variants) already exercise it; confirm whether a dedicated compile assertion for reserved-words-ok exists or whether the regular-adt lanes already cover it — if the model is only generated but its output is not compiled by an existing lane, add/extend the assertion so the Object/Class shadowing is a STANDING regression gate (not a one-off manual check).
    
    CROSS-GOAL coordination: this verification IS the green gate G2's T18 wants for the Object/Class shadowing defect class. Do NOT run a duplicate verification under G2's T18 for this class — this G3 task subsumes it; note in the T18 thread (or via a decision/comment) that G3-W2 verify covers the Object/Class shadowing compile, leaving T18 to cover only any G2-specific scope outside D5/D6. Surface to the orchestrator that G1's T14 green-CI gate now unblocks once this passes.
- acceptance: "1) `mdl :build :test-gen-regular-adt :test-scala-regular :test-kotlin-regular :test-java-regular :test-gen-wrapped-adt :test-scala-wrapped :test-kotlin-wrapped :test-java-wrapped` runs GREEN with the reserved-words-ok model in scope (the model's generated Scala/Kotlin/Java compiles). 2) Specifically: generated AvatarItem.java (Object/Class/Type empty-field branches) compiles with zero javac errors — the ~15 errors D5 cited are gone. 3) The kt service-wiring lanes (`test-gen-kt-mcp`/wiring lanes) stay green — T21's D6 escape introduced no regression. 4) If reserved-words-ok output was previously generated-but-not-compiled by any lane, a compile assertion now covers it so the shadowing is a standing gate. 5) Run from a real clone (not a linked worktree) per sbt-git constraint. Record the exact mdl targets run + green result in the session log."
- suggestedModel: standard
- dependsOn: ["T19","T20","T21"]
- ledgerRefs: ["goals:G3","defects:D5","defects:D6"]

## M12

### T23 — done

- createdAt: 2026-06-10T08:42:52.819Z
- updatedAt: 2026-06-10T10:53:57.996Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D7: FQ csTpe at both BaboonAdtType() sites in CSDomainTreeTools + rebaseline ~27 C# golden fixtures"
- description: |
    Fix the C# System.Type stdlib-shadow (D7). In baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSDomainTreeTools.scala, change the BaboonAdtType() return type from the SHORT `$csTpe` to the fully-qualified `${csTpe.fullyQualified}` (→ System.Type) at BOTH emission sites: L57 (makeFullMeta arm: `public $csTpe BaboonAdtType() => typeof(...)`) and L97 (makeRefMeta arm: `public override $csTpe BaboonAdtType() => typeof(...)`). This mirrors the proven T15(Class)/T19(Object) `.fullyQualified` remedy; sibling `$csString` already renders FQ in adjacent arms, confirming the member exists on the CSValue ref.
    
    BLAST RADIUS — GOLDEN REBASELINE REQUIRED: this changes ~27 existing C# golden fixtures (every `Type` → `System.Type` in a BaboonAdtType signature). The change is NOT byte-identical to the pre-fix baseline. After the source edit, regenerate the affected C# golden outputs and commit the rebaselined fixtures. Identify the golden-fixture set via the existing cs test/gen actions (test-gen-regular-adt / test-gen-wrapped-adt produce the cs outputs; cs golden comparisons live in the cs stub/conv test projects). Diff must show ONLY `Type`→`System.Type` in BaboonAdtType() signatures — any other delta is a regression and must be investigated, not rebaselined.
    
    Repo reality: build via mdl; sbt-git cannot build inside a linked git worktree (NoWorkTreeException) so clone to /tmp for the build (e.g. git clone <repo> /tmp/baboon-d7 && build there).
- acceptance: "(1) CSDomainTreeTools.scala L57 and L97 both render `${csTpe.fullyQualified}` (no remaining bare `$csTpe` in a BaboonAdtType return position). (2) sbt +compile / mdl :build is green (Scala.js exhaustive-match unaffected — no new TyperIssue). (3) The C# golden-fixture rebaseline diff contains ONLY `Type`→`System.Type` substitutions in BaboonAdtType() signatures (~27 fixtures); no other generated-code delta. (4) The existing cs test matrix (test-gen-regular-adt/test-gen-wrapped-adt + test-cs-regular/test-cs-wrapped) is GREEN against the rebaselined fixtures. NOT byte-identical-to-old-baseline — rebaselined-and-green is the bar."
- suggestedModel: frontier
- ledgerRefs: ["goals:G4","defects:D7"]
- resultCommit: b127e202
- completion: "FQ System.Type at CSDomainTreeTools L57/L97 BaboonAdtType(). dotnet build of reserved-words-ok C#: 24 CS0508/CS0738 → 0. Source-only (no cs goldens)."
- sessionLogs: ["docs/logs/20260610-105325-T23-T24-T13-batch.md"]

### T24 — done

- createdAt: 2026-06-10T08:43:04.758Z
- updatedAt: 2026-06-10T10:54:00.827Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "D8+D9: defensive type/enum-name escaping — FQ Java enum parse(String) + audit Java in-type-body bare literals + escape TS type/class/enum names"
- description: |
    Consolidated defensive type/enum-name hardening for the stdlib/keyword-shadowing class (LATENT — not exercised by reserved-words-ok which uses PascalCase types; default disposition FIX). Two backends:
    
    D8 (Java): JvDefnTranslator.scala:494 emits enum `public static <Name> parse(String s)` with a bare `String`; route the param type through `${jvString.fullyQualified}` (mirror T19's record-body fix). Also audit other in-type-body bare `Object`/`String`/`Class` literals outside <T>_*Codec/conversion top-level classes and FQ them.
    
    D9 (TypeScript): TsDefnTranslator emits class/type/interface names, enum name, and enum-member identifiers directly from the model name without escapeTsKeyword (T11 wired only field binding positions). Route the class/type/enum name + lowercase-mode enum `ident` through escapeTsKeyword; keep wire/JSON keys on the ORIGINAL model name (as T11 does for fields).
    
    Both are identity for non-keyword/non-stdlib names → byte-identical for existing fixtures. Verify: generated Java + TS for a model with a lowercase-keyword type/enum name (or, lacking one in reserved-words-ok, confirm the escape is applied + existing fixtures byte-identical + sbt baboonJVM/test green). Independent of T23 (Java + TS vs C#).
- acceptance: Java enum parse(String) param FQ'd; TS class/type/enum names + enum-member idents routed through escapeTsKeyword with wire keys preserved; both byte-identical for existing (PascalCase) fixtures; sbt baboonJVM/test green; sbt +compile (JVM+JS) green.
- suggestedModel: standard
- ledgerRefs: ["goals:G4","defects:D8","defects:D9"]
- resultCommit: 100d59f8
- completion: "D8: Java enum parse param FQ'd to java.lang.String. D9: TS class/type/enum names + lowercase enum ident escaped via escapeTsKeyword (wire values preserved). +compile + 602 green; byte-identical. Surfaced low D10 (pre-existing TS lowercase enum mismatch)."
- sessionLogs: ["docs/logs/20260610-105325-T23-T24-T13-batch.md"]

### T25 — planned

- createdAt: 2026-06-10T08:43:18.706Z
- updatedAt: 2026-06-10T08:43:18.706Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Verify: dotnet build of reserved-words-ok C# succeeds (closes D7, unblocks G1's T14) + cs/jv matrices green"
- description: |
    End-to-end verification gate for G4 after T23 (D7) and T24 (D8) land. Build the compiler (clone to /tmp — sbt-git cannot build in a linked worktree), generate C# for the reserved-words-ok model (the model with the `data Type {}` ADT branch that triggered D7), and run `dotnet build` on the generated output. Pre-fix this produced 24 errors (16×CS0508 + 8×CS0738) in AvatarItem.cs from System.Type shadowing; post-T23 it must compile cleanly. This closes D7 and unblocks G1's T14 C# green gate.
    
    This task does NOT re-do the golden rebaseline (that is part of T23) — it is the integration check that the rebaselined generator actually produces compiling C# for the shadowing model, plus the regression sweep across cs and jv. D8 is latent in reserved-words-ok (no String-named type), so D8's contribution here is the Java no-regression sweep, not a new green observation.
    
    On success, the orchestrator/implementer should transition D7 and D8 to resolved (their fix tasks complete).
- acceptance: "(1) Generated C# for reserved-words-ok compiles: `dotnet build` of the generated output exits 0 with ZERO errors (specifically: none of the prior 24 CS0508/CS0738 errors in AvatarItem.cs). (2) Full cs test matrix green: test-gen-regular-adt + test-gen-wrapped-adt + test-cs-regular + test-cs-wrapped (against T23's rebaselined fixtures). (3) Java no-regression: test-java-regular + test-java-wrapped green (D8 latent — confirms no regression from T24). (4) mdl :build green (Scala.js cross-build unaffected). Build performed from a /tmp clone, not a linked worktree."
- suggestedModel: standard
- dependsOn: ["T23","T24"]
- ledgerRefs: ["goals:G4","defects:D7","defects:D8"]

## M14

### T26 — wip

- createdAt: 2026-06-10T11:07:09.403Z
- updatedAt: 2026-06-10T11:11:24.285Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Fix D12: make test-tree-sitter.sh parse capture non-fatal so the real-file loop reports every failure"
- description: "In test/editors/test-tree-sitter.sh, the real-file parse loop (line 46) does `output=$(cd \"$GRAMMAR_DIR\" && tree-sitter parse \"$f\" 2>&1)` under `set -euo pipefail` (line 2). `tree-sitter parse` exits non-zero on a parse ERROR, so the failing command-substitution assignment aborts the WHOLE script under `set -e` — BEFORE the `grep -q ERROR` check, the `FAIL:` print, and the `--- Summary ---`. The stage exits non-zero with no indication of which file failed. FIX (D12 suggestedFix): append `|| true` to the capture, i.e. `output=$(cd \"$GRAMMAR_DIR\" && tree-sitter parse \"$f\" 2>&1) || true`, so the loop visits every file, the existing `grep -q ERROR` branch counts each failure into `errors` and appends to `failed_files`, the Summary prints, and the script exits 1 at the end on error count (lines 62-69 already do this). Do NOT change the corpus-test invocation (line 34) or the exit-1-on-error logic. This is independent of D11 — fix it FIRST so the D11 grammar-fix verification reports failures properly. Edit on a fresh clone (sbt-git cannot run in a linked worktree per CLAUDE.md, though this script edit does not invoke sbt). tree-sitter is on PATH via nix."
- acceptance: "With the deliberately-broken fixture still failing (i.e. before/independent of the D11 fix), running `bash test/editors/test-tree-sitter.sh .` runs the full real-file loop to completion: it prints `FAIL: <rel-path>` for reserved-words-ok/reserved.baboon (and any other failing file), prints the `--- Summary ---` block listing failed files, and exits with code 1 (not aborting silently after the last `OK:` line). Diff is limited to the line-46 capture in test/editors/test-tree-sitter.sh."
- suggestedModel: fast
- ledgerRefs: ["defects:D12","goals:G5"]

### T27 — planned

- createdAt: 2026-06-10T11:07:30.838Z
- updatedAt: 2026-06-10T11:07:30.838Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Fix D11: make tree-sitter editor grammar accept keyword-named identifiers (or exclude reserved-words-ok fixture) so test-editors parses all real files"
- description: |
    editors/baboon-zed/grammars/baboon/grammar.js sets `word: $ => $.identifier` (line 9), enabling tree-sitter keyword extraction. Field/member/type-name positions use `field("name", $.identifier)` (e.g. field_def line 234-240, data/adt/enum/contract/foreign/service/type_alias name fields, enum_member, foreign_mapping target, type_params), but the grammar's literal string-token keywords (`is`, `in`, `def`, `type`, `import`, `with`, `was`, `data`, `struct`, `id`, `adt`, `enum`, `choice`, `contract`, `foreign`, `service`, `ns`, `root`, `domain`, `derived`, builtin types, etc.) win the keyword-extraction tiebreak over `identifier`, so a model field/member named after one fails to parse. reserved-words-ok/reserved.baboon hits this first at `is` (Holder, row 45) → `ERROR[45,4]`. The compiler's FastParse grammar accepts these (idt.symbol, no keyword exclusion), so the editor grammar is stricter than the compiler.
    
    The implementer EVALUATES feasibility and picks the LOWEST-RISK option that makes test-editors green AND keeps the corpus tests + ALL OTHER real files passing:
      (a) Introduce a name-position rule, e.g. `_name = choice($.identifier, <the keyword tokens that are legal identifiers in the compiler>)`, and use it at the field/member/type/enum-member name `field("name", ...)` sites; re-run `tree-sitter generate` and resolve any conflicts (precedence/conflicts[] declarations) so the corpus tests still pass; OR
      (b) relax/remove `word:` with explicit precedence; OR
      (c) if a robust grammar fix is disproportionate or destabilizes the corpus/other-file parses, EXCLUDE the deliberately-pathological reserved-words-ok codegen fixture from the test-editors real-file scan — in test/editors/test-tree-sitter.sh adjust the `find ... -name '*.baboon'` collection (lines 19-22) to prune reserved-words-ok (e.g. `-not -path '*/reserved-words-ok/*'`) WITH a documented rationale comment (editor grammar is best-effort highlighting; reserved-words-ok is a codegen torture-test, not an editing target — mirrors the D9 mcp-stub-ok isolation precedent in CLAUDE.md).
    
    Reproduce FIRST: with T26's harness fix in place, run `bash test/editors/test-tree-sitter.sh .` and confirm it reports `FAIL: .../reserved-words-ok/reserved.baboon` with `(ERROR [45,4])` for the expected reason (keyword-as-field-name), not an unrelated error. Then apply the chosen fix. If option (a)/(b): re-run `tree-sitter generate` then `tree-sitter test` (corpus) inside editors/baboon-zed/grammars/baboon, committing the regenerated src/parser.c if the repo tracks it. Work on a fresh clone to /tmp (sbt-git cannot run in a linked worktree per CLAUDE.md). tree-sitter is on PATH via nix.
- acceptance: "`bash test/editors/test-tree-sitter.sh .` exits 0: the `--- Corpus tests ---` stage (tree-sitter generate && tree-sitter test) passes, and the `--- Real file parse tests ---` Summary reports `<N>/<N> passed` with zero failed files — in particular reserved-words-ok/reserved.baboon no longer produces ERROR (option a/b) OR is no longer in the scanned set with a documented rationale comment (option c), AND every previously-passing real .baboon file still parses. If option (a)/(b), the regenerated parser is consistent (no uncommitted `tree-sitter generate` drift)."
- suggestedModel: frontier
- dependsOn: ["T26"]
- ledgerRefs: ["defects:D11","goals:G5"]

### T28 — planned

- createdAt: 2026-06-10T11:07:44.885Z
- updatedAt: 2026-06-10T11:07:44.885Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Verify test-editors green then full mdl :ci green (closes D11/D12 verification incl. the previously-aborted per-language matrix + conv-test)"
- description: "After T26 (harness) and T27 (grammar/exclusion) land, verify the whole effort end-to-end. Step 1: run the editor stage in isolation — `bash test/editors/test-tree-sitter.sh .` (equivalently `mdl :test-editors`) — and confirm it exits 0 (corpus tests pass + real-file Summary reports all files passed). Step 2: run the full `mdl :ci` and confirm it goes GREEN end-to-end: with test-editors no longer aborting (D11) and no longer masking (D12), the gate proceeds past the editor stage and the previously-blocked downstream stages (per-language test matrix + conv-test cross-language acceptance) now execute and pass. Run from a fresh clone to /tmp because sbt-git cannot build inside a linked git worktree (CLAUDE.md); on a memory-constrained machine use `mdl --seq :ci` to avoid the Kotlin daemon OOM under the parallel matrix (CLAUDE.md). Capture mdl output with `--simple-log` when redirecting (MEMORY.md). This task does NOT close goal G5 or any defect to a terminal/done state — goal closure is user-driven; report the green result for review."
- acceptance: "Two commands observed green from a fresh /tmp clone: (1) `bash test/editors/test-tree-sitter.sh .` exits 0 with the real-file Summary showing `<N>/<N> passed` and no failed files; (2) full `mdl :ci` completes with exit 0 — the editor stage passes AND the downstream per-language test matrix + conv-test stages (previously unreached because test-editors aborted) run and pass. Captured log evidence (e.g. the mdl --simple-log tail showing the final success and no RED stage) attached to the task on completion."
- suggestedModel: standard
- dependsOn: ["T27"]
- ledgerRefs: ["goals:G5"]

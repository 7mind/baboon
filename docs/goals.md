---
ledger: goals
counters:
  milestone: 0
  item: 3
archives: []
---

# goals

## M2

### G1 — planned

- createdAt: 2026-06-09T19:05:26.509Z
- updatedAt: 2026-06-09T20:55:56.945Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- title: Escape/normalize reserved-word identifier collisions across all 9 backends
- description: |
    DEFECT-SEEDED goal (from D1, root cause confirmed — skip clarifying per T35).
    
    GOAL: generated code must never emit a target-language reserved word as an unescaped identifier. Apply target-native escaping/normalization at every site where a model-derived name becomes a target identifier (type / ADT-branch / enum-member / field / property / method / parameter / codec-local / namespace), including the codec capture-variable sites that lowercase the branch name.
    
    CONFIRMED ROOT CAUSE (D1): Baboon code generators render model names into target identifiers with no reserved-word escaping pass. C# symptom REPRODUCED + compile-confirmed: CSJsonCodecGenerator.scala:158 / CSUEBACodecGenerator.scala:196 lowercase the ADT branch name into a pattern-capture variable, so branch `Default` emits `if (value is AvatarItem.Default default)` — `dotnet build` → CS1026; `@default` builds. (Not a type-name collision: C# is case-sensitive.)
    
    CROSS-LANGUAGE SCOPE (audited, citations validated):
    - NO escaping at all — C#, Scala (ScJsonCodecGenerator:201→211), Kotlin (KtUEBACodecGenerator:185→204), Java (JvUEBACodecGenerator:182→199; `default` is a Java keyword too), Python (PyDefnTranslator:540 field names; `none`/`true`/`false`).
    - escaper EXISTS but dead — TypeScript (escapeTsKeyword TsTypeTranslator:310-319, zero callers).
    - PARTIAL/MIXED — Rust (escapeRustKeyword `r#`+escapeRustModuleName applied to field/fn/module; residual type/variant gap RsTypeTranslator:155; lowest risk), Dart (escapeDartKeyword applied to TYPE names only via render pass; member names bypass, DtDefnTranslator:307/538/571), Swift (escapeSwiftKeyword applied for type/field; ADT-branch/enum case names bypass, SwDefnTranslator:889-894 / SwJsonCodecGenerator:109-128 / SwUEBACodecGenerator:150-178).
    
    SUGGESTED FIX (native mechanism per language): C#=`@`-verbatim; Scala/Kotlin/Swift=backticks (Swift: route case names through existing escapeSwiftKeyword); Rust=`r#` + rename for self/super/crate/Self + type/variant names; Java/Python/TypeScript/Dart=RENAME (no escape syntax) — TS wire up dead escapeTsKeyword, Dart wire existing helper through member emission. CRITICAL: when RENAMING changes an emitted identifier, preserve the JSON/UEBA wire key via an explicit rename annotation (mirroring Rust's `#[serde(rename)]`) so cross-language serialization stays compatible. Add per-language test models with keyword-named branches/fields/types and assert generated code compiles in each target; extend the existing per-language test matrix. Repro harness: `adt AvatarItem { data Default {} data BuiltIn { id: str } }` + `root data Holder { item: AvatarItem }`, `--generate-{json,ueba}-codecs-by-default=true`.
    
    See defect D1 (rootCause/suggestedFix) and hypotheses H1/H2 for full validated evidence.
- sourceRefs: ["defects:D1"]
- grounding: |
    Verified against source (Read; codegraph/Grep/Glob unavailable in runtime):
    - C# capture sites confirmed exactly: CSJsonCodecGenerator.scala:158 `val branchNameRef = q"${branchName.toLowerCase}"` used at :162/:177; CSUEBACodecGenerator.scala:196 `val castedName = branchName.toLowerCase` used at :199. No `@`-escaping in the C# identifier path. Note `wrappedAdtBranchCodecs` branch at :164/:198 takes a different path (`wire`/cName) — escaping the capture var must cover BOTH the wrapped and non-wrapped arms.
    - TS escapeTsKeyword (TsTypeTranslator.scala:310-319) confirmed present, rename mechanism = trailing `_`; zero callers (dead).
    - Swift enum-case site SwDefnTranslator.scala:889-894 builds `caseName = memberName.head.toLower + tail` RAW, bypassing escapeSwiftKeyword (which exists per H2 via render pass).
    - Rust RsTypeTranslator.scala:155 emits `RsType(fullCrate, tid.name.name.capitalize)` raw for type/variant names (residual gap); escapeRustKeyword(`r#`) + escapeRustModuleName confirmed at RsDefnTranslator.scala:1533-1546 (module form uses `_`/readable rename, `in`→`input`); self/super/crate/Self NOT in rustKeywords list and not r#-escapable → need rename.
    - Dart DtDefnTranslator.scala:307/318 emit `f.name.name` verbatim for field decls + ctor params (bypass escapeDartKeyword).
    - Rust `#[serde(rename = "<wireKey>")]` is the existing wire-key-preservation discipline; the RENAME backends (Java=@JsonProperty, Python=alias/metadata, TS=bracket-string property access, Dart=JsonKey/explicit map key) must mirror it so the on-wire key is the UNRENAMED model name.
    - Test matrix: shared model-dir `baboon-compiler/src/test/resources/baboon/<subdir>/` (e.g. recursive-ok/) generated into per-lang stubs by `test-gen-regular-adt`/`test-gen-wrapped-adt`, then compiled by `test-<lang>-{regular,wrapped}` mdl actions (.mdl/defs/tests.md). Per-lang keyword test models go here; codecs only emit under `--generate-{json,ueba}-codecs-by-default=true`.
    - Exhaustive-match discipline (CLAUDE.md): if any fix adds a TyperIssue case, update 3 sites (DiagnosticsProvider, WorkspaceState, BaboonJS). `sbt clean` required after editing baboon-runtime resources. CI runs `sbt +compile` (JVM+JS) via `mdl :build :test`.
- milestones: ["M3","M4","M5"]
- sessionLogs: ["docs/logs/20260609-205006-a3f6cdd284886c24d.md","docs/logs/20260609-205337-ac80569ba528cf99b.md","docs/logs/20260609-205547-a57ca9ba4b4b1caa3.md"]

## M6

### G2 — planned

- createdAt: 2026-06-09T22:20:22.405Z
- updatedAt: 2026-06-09T22:29:34.863Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- title: Fix codegen identifier follow-ups (D3 Class-shadowing; consolidates D2/D4)
- description: |
    DEFECT-SEEDED goal (root cause confirmed — skip clarifying per T35). Consolidates the generated-code identifier-correctness follow-up defects discovered during G1's implementation. These are DISTINCT from D1 (reserved-keyword escaping); they are codegen-correctness bugs surfaced by the reserved-words-ok test model.
    
    CONFIRMED ROOT CAUSE — D3 (medium, BLOCKS G1's T14): the per-ADT metadata field `baboonAdtType` references the JVM stdlib `java.lang.Class` by SHORT/predef name in all three JVM-family domain-tree-tools. `JvTypes.scala:104` `javaClass = JvType(javaLangPkg, "Class", predef = true)` (unqualified); emitted as `Class<?>` (JvDomainTreeTools:67), `Class<*>` (KtDomainTreeTools:70), `Class[?]` (ScDomainTreeTools:67). A model ADT branch/type named `Class` shadows `java.lang.Class` → compile errors. C# unaffected (typeof→System.Type); non-JVM backends have no such field.
    SUGGESTED FIX (D3): fully-qualify the stdlib Class reference in the metadata field — Scala `_root_.java.lang.Class[?]`, Java `java.lang.Class<?>`, Kotlin `java.lang.Class<*>` (or KClass per the multiplatform arm) — ideally at the `javaClass` predef definition. Then generated Scala/Kotlin/Java for the reserved-words-ok `Class` branch must compile. Broader: audit other predef short-name refs (`Type`,`String`) for the same shadowing hazard.
    
    TO BE CONSOLIDATED (pending their investigate passes this run):
    - D2 (low): Java `renderOwner` ADT-name package segment not keyword-escaped (JvTypeTranslator:166) — route through escapeJvKeyword.
    - D4 (low): Kotlin service-method wiring call sites `impl.<m>()` in KtServiceWiringTranslator (lines 669/677/699/707/767/775/798/806) not backtick-escaped, asymmetric with the now-escaped declaration — route through escapeKtKeyword.
    
    See defects D3 (rootCause/suggestedFix) + H3 (confirmed), and D2/D4.
- sourceRefs: ["defects:D3","defects:D2","defects:D4"]
- grounding: "Source-validated this pass (translator pkg is io/septimalmind/baboon/translator). D3 predef def confirmed at translator/java/JvTypes.scala:104 `javaClass = JvType(javaLangPkg, \"Class\", predef = true)` — short, shadowable; sibling jvString:100/jvObject:101 also short (broader-audit hazard). D3 emission sites confirmed: translator/java/JvDomainTreeTools.scala:67 `q\"public static final $javaClass<?> baboonAdtType\"`; translator/kotlin/KtDomainTreeTools.scala:70 `q\"val baboonAdtType: $javaClass<*>\"`. Kotlin javaClass predef at translator/kotlin/KtTypes.scala:63-67: multiplatform arm already FQ (kotlin.reflect.KClass), JVM arm :66 uses short `Class` predef = the defect; fix only the JVM arm. Scala domain-tree-tools (ScDomainTreeTools:67 `Class[?]`, per D3 rootCause validated) lives under a translator subpkg I could not glob this pass — implementer resolves exact path; fix = `_root_.java.lang.Class[?]`. D2 confirmed translator/java/JvTypeTranslator.scala:193 `case Owner.Adt(id) => renderOwner(id.owner) :+ id.name.name` (unescaped) vs Owner.Ns :192 `escapeJvKeyword(s.name.toLowerCase)`. D4 confirmed translator/kotlin/KtServiceWiringTranslator.scala raw `impl.${m.name.name}(...)` at 447/448/489/490 (+ 669/677/699/707/767/775 symmetric). Verification: generate+compile reserved-words-ok model (has a `Class` ADT branch) for Scala+Kotlin+Java; per CLAUDE.md sbt-git cannot build in a linked worktree so implement workers clone to /tmp; no `bun run check`, build via mdl/sbt."
- milestones: ["M7"]
- sessionLogs: ["docs/logs/20260609-222602-a88e96d286fbec03b.md","docs/logs/20260609-222834-a392629b29a540856.md"]

## M8

### G3 — planning

- createdAt: 2026-06-09T22:49:31.137Z
- updatedAt: 2026-06-09T22:49:31.137Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- title: Fix stdlib-type-shadowing (D5, general) + Kotlin client-stub escape (D6)
- description: |
    DEFECT-SEEDED goal (root causes confirmed — skip clarifying per T35). Second wave of codegen identifier-correctness follow-ups surfaced by the reserved-words-ok model (which has ADT branches `Object` and `Class`).
    
    CONFIRMED ROOT CAUSE — D5 (high, BLOCKS G2's T18 + G1's T14 Java compile): generated JVM code references stdlib types (`Object`, `String`, `Class`, `Type`) by SHORT name, shadowable by a model type/branch of that name. Concrete: JvDefnTranslator.scala:431 `public boolean equals(Object other)` (bare `Object`) → a `data Object {}` branch shadows java.lang.Object → ~15 javac errors. Also JvTypes:100-101 jvString/jvObject short-named predefs. T15 already FQ'd `Class` in baboonAdtType narrowly; D5 generalizes it. SUGGESTED FIX: render JVM stdlib type refs fully-qualified at all generated-code emission sites (fix bare `Object` in JvDefnTranslator equals/hashCode; make jvObject/jvString/javaClass predefs render FQ via .fullyQualified). Resolves D5, pre-empts String/Type siblings, generalizes T15. Verify generated Java for reserved-words-ok (Object+Class branches) compiles.
    
    CONFIRMED ROOT CAUSE — D6 (low): KtServiceWiringTranslator:864/882 client-stub declarations `suspend fun ${m.name.name}(...)`/`...Json(...)` emit raw method name (not escaped, unlike T6's interface declaration). SUGGESTED FIX: route through KtTypeTranslator.escapeKtKeyword; keep transport string args (867/884) raw.
    
    CROSS-GOAL: D5's fix unblocks G2's T18 (verification) and thus G1's T14 (green CI gate). See defects D5/D6 + hypotheses H6/H7.
- sourceRefs: ["defects:D5","defects:D6"]

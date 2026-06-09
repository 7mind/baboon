---
ledger: defects
counters:
  milestone: 0
  item: 6
archives: []
---

# defects

## M1

### D1 — root-caused

- createdAt: 2026-06-09T18:46:04.620Z
- updatedAt: 2026-06-09T19:04:33.755Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Generated identifiers collide with target-language reserved words (C# `Default` → invalid code); no escaping/normalization
- description: |
    we have an issue with C# keywords. This model:
    
      adt AvatarItem {
        data Default {}
        data BuiltIn { id: str }
      }
    
    leads to incorrect C# code — `Default` collides with the C# `default` keyword and the generated identifiers are not escaped/normalized. (1) C# keywords must be properly escaped/normalized per C# conventions in generated code. (2) Every other backend (Scala, Python, Rust, TypeScript, Kotlin, Java, Dart, Swift) must be audited for the same reserved-identifier collision class and given target-native transformations.
- severity: medium
- rootCause: |
    Baboon code generators render model-derived names into target-language identifiers WITHOUT a reserved-word escaping/normalization pass, so a model whose name maps to a target keyword emits non-compiling code.
    
    C# (reported symptom, REPRODUCED + compile-confirmed): the JSON and UEBA codec generators lowercase the ADT branch name into a pattern-match capture variable. CSJsonCodecGenerator.scala:158 `val branchNameRef = q"${branchName.toLowerCase}"` used at :177 `if (value is $fqBranch $branchNameRef)` / :162 `Encode(ctx, $branchNameRef)`; CSUEBACodecGenerator.scala:196 `val castedName = branchName.toLowerCase` used at :199. For branch `Default`, the variable is `default` — a reserved C# keyword. Reproduced: generated Avatar/AvatarItem.cs:437 `if (value is AvatarItem.Default default)`; `dotnet build` → `error CS1026: ) expected` at the `default` column; `@default` → Build succeeded. (Note: the C# symptom is NOT a type-name collision — C# is case-sensitive, so the record `Default` itself is legal; the fault is the lowercased codec capture variable. Codecs only emit for `derived[...]`-annotated types or under `--generate-{json,ueba}-codecs-by-default=true`.)
    
    CROSS-LANGUAGE (audited, citations validated against source): the same defect CLASS affects all 8 other backends.
    - NO escaping at all: Scala (ScJsonCodecGenerator:201→211; triggers `type`/`val`/`object`/`class`/`match`), Kotlin (KtUEBACodecGenerator:185→204; `object`/`is`/`when`/`fun`/`val`), Java (JvUEBACodecGenerator:182→199; `default` is also a Java keyword), Python (field/method names verbatim PyDefnTranslator:540; `none`/`true`/`false`→`None`/`True`/`False`).
    - Escaper exists but UNUSED/dead: TypeScript (escapeTsKeyword TsTypeTranslator:310-319 has zero callers tree-wide; defect via field/type/enum names + UEBA `const <field>` local).
    - Partial escaping with gaps (MIXED): Rust (escapeRustKeyword `r#` + escapeRustModuleName applied to field/fn/module names; residual gap on type/variant names via bare `.capitalize`, RsTypeTranslator:155 — `self`/`super`/`crate`/`Self` not even r#-escapable; lowest risk), Dart (escapeDartKeyword applied to TYPE names via DtBaboonTranslator render pass only; field/getter/method names bypass it, DtDefnTranslator:307/538/571), Swift (escapeSwiftKeyword applied via render pass for type/field names; ADT-branch + enum case names built as raw strings bypass it, SwDefnTranslator:889-894 / SwJsonCodecGenerator:109-128 / SwUEBACodecGenerator:150-178 → `case default(...)`).
    
    See hypotheses H1 (confirmed) and H2 (confirmed) for full validated evidence.
- suggestedFix: |
    Apply target-native reserved-word escaping/normalization at every site where a model-derived name becomes a target identifier (type/branch/enum-member/field/property/method/parameter/local/namespace), and — critically — at the codec capture-variable sites that lowercase the branch name. The native mechanism differs per language:
    - C#: `@`-verbatim-identifier prefix (`@default`). Fix the two branch-capture sites (CSJsonCodecGenerator:158, CSUEBACodecGenerator:196) first to resolve the reported symptom, then add a general C# identifier-escaping pass.
    - Scala / Kotlin / Swift: backtick-quoting (`` `name` ``). Swift already has escapeSwiftKeyword — route ADT-branch/enum case names through it (SwDefnTranslator:889-894 et al.).
    - Rust: raw identifiers `r#name` (already present); additionally handle type/variant names and the non-r#-able `self`/`super`/`crate`/`Self` via renaming.
    - Java / Python / TypeScript / Dart: NO escape syntax exists — must RENAME (e.g. trailing `_`, with a wire-format `@JsonProperty`/serde-rename/alias to preserve the on-wire key, mirroring Rust's existing `#[serde(rename)]` discipline). TypeScript: wire up the dead `escapeTsKeyword`. Dart: wire the existing escapeDartKeyword through field/getter/method emission.
    WHEN RENAMING (not escaping) changes an emitted identifier, the JSON/UEBA wire key MUST be preserved via an explicit rename annotation so cross-language serialization stays compatible. Add per-language test models with keyword-named branches/fields/types and assert the generated code compiles in each target (extend the existing per-language test matrix). Reproduction harness: model `adt AvatarItem { data Default {} data BuiltIn { id: str } }` + `root data Holder { item: AvatarItem }`, compiled with `--generate-{json,ueba}-codecs-by-default=true`.
- sessionLogs: ["docs/logs/20260609-190158-orchestrator-h1-repro.md","docs/logs/20260609-190158-a5a5a81722f58e956.md","docs/logs/20260609-190158-a6e4b7512b52af94b.md","docs/logs/20260609-190158-a123e05351e969bd7.md","docs/logs/20260609-190158-a339a8ad797fbee13.md","docs/logs/20260609-190158-a119c0b6658e6655f.md","docs/logs/20260609-190158-abf8f375663b3c77b.md","docs/logs/20260609-190158-a6a64824537135162.md","docs/logs/20260609-190158-a3ef4e52b3b442ee7.md"]

## M4

### D2 — resolved

- createdAt: 2026-06-09T21:42:07.419Z
- updatedAt: 2026-06-09T22:46:36.199Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Java: ADT-name package segment in renderOwner not keyword-escaped"
- description: "Filed by T9 reviewer (out-of-scope for T9's field/accessor/capture-var scope). In JvTypeTranslator.renderOwner (JvTypeTranslator.scala:166), the Owner.Adt arm emits `renderOwner(id.owner) :+ id.name.name` retaining the ADT name at original casing as a package-path segment, unlike the Owner.Ns arm which now lowercases-and-escapes. If a model declared an ADT whose name lowercased to a Java keyword AND used it as a package qualifier, the emitted package segment could be illegal. Latent edge; ADT names are conventionally capitalized so collision is unlikely. Default disposition: FIX (fold into the C#/Java general-pass scope or a follow-up)."
- severity: low
- suggestedFix: "Route the Owner.Adt arm's `id.name.name` through `JvTypeTranslator.escapeJvKeyword` (matching the Owner.Ns arm at :192), or document the capitalization invariant. Fix under goal G2."
- ledgerRefs: ["tasks:T9","goals:G1"]
- rootCause: "Validated against source: JvTypeTranslator.scala:193 `case Owner.Adt(id) => renderOwner(id.owner) :+ id.name.name` appends the ADT name UNESCAPED as a package-path segment, unlike the Owner.Ns arm (:192) which routes through `escapeJvKeyword(s.name.toLowerCase)`. An ADT used as a package qualifier whose (lowercased) name is a Java keyword would emit an illegal package segment. Low severity (ADT names conventionally capitalized). See H4 (confirmed). Consolidated into goal G2."
- sessionLogs: ["docs/logs/20260609-221857-orchestrator-d3-confirm.md"]
- dependsOn: ["T16"]
- fix: "T16 (merged 7e36259a): JvTypeTranslator.renderOwner Owner.Adt arm now routes the ADT-name package segment through escapeJvKeyword (escape-only, casing preserved). 602 green, byte-identical for existing fixtures."

### D3 — root-caused

- createdAt: 2026-06-09T22:02:49.218Z
- updatedAt: 2026-06-09T22:25:40.341Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: ADT branch/type named `Class` shadows stdlib `java.lang.Class` in generated metadata field (cross-language; distinct from D1 keyword-escaping)
- description: "Discovered independently by the T5 (Scala) and T6 (Kotlin) workers while compiling the reserved-words-ok model. The generated per-type metadata field `baboonAdtType: Class[?]` (Scala) / `Class<*>` (Kotlin), emitted by the domain-tree-tools, references the stdlib `java.lang.Class` by SHORT name. When an ADT has a branch named `Class` (or a type named `Class`), the nested `AvatarItem.Class` case class/object shadows `java.lang.Class`, so the metadata field's type resolves to the wrong `Class` => compile errors (~32 in AvatarItem.kt; analogous in Scala). This is NOT a reserved-keyword collision (`Class` is not a language keyword) and is NOT caused by/limited to the keyword-escaping fixes — it is a separate stdlib-type-name-shadowing defect. Likely affects every backend whose generated metadata/runtime interface references a stdlib type by short name (Scala, Kotlin, Java at least). BLOCKS G1's T14 green-matrix gate because the reserved-words-ok model includes a `Class` branch. Default disposition: FIX."
- severity: medium
- suggestedFix: "In the JVM-family domain-tree-tools, emit the stdlib `Class` reference in the `baboonAdtType` metadata field by a NON-SHADOWABLE form so a model type/branch named `Class` cannot capture it: (a) Scala (ScDomainTreeTools:67) — fully-qualify as `_root_.java.lang.Class[?]`; (b) Java (JvDomainTreeTools:67) — fully-qualify as `java.lang.Class<?>`; (c) Kotlin (KtDomainTreeTools:70) — fully-qualify as `java.lang.Class<*>` (or `kotlin.reflect.KClass` per the existing multiplatform branch — check the `multiplatform` arm at KtTypes javaClass:63). Implement by making the `javaClass` predef reference fully-qualified at its definition (JvTypes:104 etc.) OR overriding it at the metadata-field emission site. Add coverage: the reserved-words-ok model already includes a `Class` ADT branch — once fixed, generated Scala/Kotlin/Java for it must compile. Broader: audit other predef short-name references in generated code (`Type`, `String`, etc.) for the same model-name-shadowing hazard."
- ledgerRefs: ["tasks:T5","tasks:T6","goals:G1"]
- rootCause: "The generated per-ADT metadata field `baboonAdtType` references the JVM stdlib `java.lang.Class` by SHORT/predef name in all three JVM-family domain-tree-tools. Validated against source: `JvTypes.scala:104` defines `javaClass = JvType(javaLangPkg, \"Class\", predef = true)` (renders unqualified `Class`); it is emitted as `Class<?> baboonAdtType` (JvDomainTreeTools:67), `Class<*>` (KtDomainTreeTools:70), `Class[?]` (ScDomainTreeTools:67). When a model ADT has a branch (or a type) named `Class`, the generated nested type `<Adt>.Class` shadows `java.lang.Class` in scope, so the field's type resolves to the model type → compile errors (~32 in AvatarItem.kt; analogous in Scala/Java). C# is NOT affected (CSDomainTreeTools:57/97 uses `typeof(...)→System.Type`, no short `Class` binding); Rust/TypeScript/Dart/Swift/Python emit no such `Class`-typed metadata field. Distinct from D1: `Class` is not a language keyword, so keyword-escaping does not and must not touch it. See hypothesis H3 (confirmed)."
- sessionLogs: ["docs/logs/20260609-221857-orchestrator-d3-confirm.md"]
- dependsOn: ["T15","T18"]

### D4 — resolved

- createdAt: 2026-06-09T22:06:55.181Z
- updatedAt: 2026-06-09T22:46:38.651Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Kotlin: service-method wiring call sites not keyword-escaped (asymmetric with escaped declaration)"
- description: "Filed by T6 reviewer (out-of-scope for T6). T6 escaped the service-method DECLARATION (`fun ´when´(...)` in KtDefnTranslator ~:445) but the symmetric wiring CALL sites `impl.${m.name.name}(...)` in KtServiceWiringTranslator.scala (lines 669/677/699/707/767/775/798/806) still use raw `m.name.name`. A service method named after a Kotlin hard keyword would declare `´when´` but be called as `.when()` => compile error. Pre-T6 both sides were unescaped (consistent); T6 introduced the asymmetry. No current fixture exercises it; build stays green. Default disposition: FIX."
- severity: low
- suggestedFix: "Route each `impl.${m.name.name}(...)` invocation in KtServiceWiringTranslator through `KtTypeTranslator.escapeKtKeyword` so call sites match the escaped interface declaration. Fix under goal G2."
- ledgerRefs: ["tasks:T6","goals:G1"]
- rootCause: "Validated against source: KtServiceWiringTranslator.scala emits `impl.${m.name.name}(...)` with the RAW method name at lines 447/448/489/490/669/677/699/707/767/775 (and symmetric), while T6 escaped the method DECLARATION. For a service method named after a Kotlin hard keyword the escaped declaration would not match the unescaped call site → compile error. Low severity (no current fixture has a keyword-named service method). See H5 (confirmed). Consolidated into goal G2."
- sessionLogs: ["docs/logs/20260609-221857-orchestrator-d3-confirm.md"]
- dependsOn: ["T17"]
- fix: "T17 (merged e33a1b43): all 12 impl.<method>() service-wiring call-site receivers in KtServiceWiringTranslator now route through escapeKtKeyword (symmetric with T6's escaped declaration); wire-name string literals left raw. 602 green, byte-identical for existing fixtures."

## M7

### D5 — root-caused

- createdAt: 2026-06-09T22:46:48.909Z
- updatedAt: 2026-06-09T22:54:20.523Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Java `equals(Object)` (and bare java.lang.Object refs) shadowed by a model ADT branch/type named `Object` — sibling of D3
- description: "Discovered by the T15 worker + reviewer while compiling the reserved-words-ok model (which has a `data Object {}` ADT branch). SAME defect CLASS as D3 (stdlib type referenced by SHORT name, shadowed by a model type/branch of that name) but for `java.lang.Object` rather than `Class`, and at a DIFFERENT emission site: the generated Java `equals(Object other)` method signature (and likely other bare `Object` refs). A `data Object {}` branch produces a nested `Object` type that shadows `java.lang.Object` → ~15 residual javac errors (`method does not override`, `Object cannot be converted to <Branch>`) in AvatarItem.java AFTER T15 fixed the Class-shadowing. BLOCKS G2's T18 verification (and thus G1's T14 green gate) for the Java backend. Default disposition: FIX. Related: the T15 worker also flagged JvTypes jvString:100/jvObject:101 predefs are short-named (broader audit). Likely the general fix is to render stdlib predef type refs in generated code fully-qualified wherever a model type could shadow them."
- severity: high
- suggestedFix: "GENERAL fix (preferred): render JVM stdlib type references (Object, String, Class, Type) fully-qualified at all generated-code emission sites — fix the bare `Object` in JvDefnTranslator equals/hashCode (:431+), and make jvObject/jvString/javaClass predefs render FQ (mirroring T15's .fullyQualified for Class). This resolves D5 and pre-empts String/Type siblings. Generalizes T15's narrow Class fix. Verify generated Java for reserved-words-ok (which has Object+Class branches) compiles."
- ledgerRefs: ["tasks:T15","goals:G2","defects:D3"]
- rootCause: "Validated against source (H6 confirmed): JvDefnTranslator.scala:431 `public boolean equals(Object other)` emits the parameter type as a BARE `Object` literal; a model ADT branch/type named `Object` (nested type) shadows java.lang.Object → the override is mistyped → ~15 javac errors. Same defect CLASS as D3 but for `Object` and at the equals-emission site. Also JvTypes:100-101 jvString/jvObject predefs render short-named (same hazard for `String`). GENERAL class: JVM stdlib predef short-name refs in generated code are shadowable by a model type of that name. Blocks G2's T18 (Java compile) and G1's T14."
- sessionLogs: ["docs/logs/20260609-221857-orchestrator-d3-confirm.md"]
- dependsOn: ["T19","T20","T22"]

### D6 — root-caused

- createdAt: 2026-06-09T22:46:53.812Z
- updatedAt: 2026-06-09T22:54:21.472Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Kotlin client-stub method declarations not keyword-escaped (KtServiceWiringTranslator:864/882)"
- description: "Discovered by the T17 reviewer (out of D4's wiring-call-site scope). In KtServiceWiringTranslator.scala the generated client-stub DECLARATIONS at lines 864 (`suspend fun ${m.name.name}(...)`) and 882 (`suspend fun ${m.name.name}Json(...)`) emit the model method name as a raw Kotlin identifier without routing through escapeKtKeyword. A model service method named after a Kotlin hard keyword would emit an unparseable client stub. Distinct from the transport string args at 867/884 which must stay raw (wire names). Low severity, latent (no current fixture has a keyword-named service method). Default disposition: FIX."
- severity: low
- suggestedFix: "Route the declaration identifier at KtServiceWiringTranslator:864/882 (+ Json variant) through KtTypeTranslator.escapeKtKeyword; keep transport string args raw."
- ledgerRefs: ["tasks:T17","goals:G2"]
- rootCause: "Validated (H7 confirmed): KtServiceWiringTranslator.scala:864/882 client-stub declarations `suspend fun ${m.name.name}(...)`/`...Json(...)` emit the raw model method name, not routed through escapeKtKeyword (unlike T6's escaped interface declaration). A keyword-named service method emits an unparseable Kotlin client stub. Transport string args (867/884) correctly raw. Low severity, latent."
- sessionLogs: ["docs/logs/20260609-221857-orchestrator-d3-confirm.md"]
- dependsOn: ["T21","T22"]

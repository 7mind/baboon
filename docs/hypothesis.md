---
ledger: hypothesis
counters:
  milestone: 0
  item: 7
archives: []
---

# hypothesis

## M1

### H1 — confirmed

- createdAt: 2026-06-09T18:51:54.467Z
- updatedAt: 2026-06-09T19:03:33.484Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: C# codec generators lowercase ADT branch names into unescaped pattern-capture identifiers; a branch whose lowercased name is a C# keyword (Default→default) emits non-compiling C#
- description: "The reported symptom is NOT a type-name collision (C# is case-sensitive; `Default` the class name is legal). The actual mechanism: the C# JSON and UEBA codec generators take the ADT branch name and `.toLowerCase` it to form a pattern-match capture variable in `if (value is <Adt>.<Branch> <lower>) { ... Encode(ctx, <lower>); }`. For branch `Default`, `<lower>` = `default`, which is a reserved C# keyword and is illegal as an identifier without the `@` verbatim prefix. No reserved-word escaping exists anywhere in the C# identifier-emission path. What would make this true: locating the `.toLowerCase` branch-variable sites and confirming no `@`-escaping is applied; reproducing the generated C#."
- ledgerRefs: ["defects:D1"]
- evidence: ["[correct] CSJsonCodecGenerator.scala:158 `val branchNameRef = q\"${branchName.toLowerCase}\"` — used at :177 `if (value is $fqBranch $branchNameRef)` and :162 `...Encode(ctx, $branchNameRef)`. Re-read against source: the ADT branch name is lowercased into a C# pattern-capture variable. No reserved-word escaping in the C# identifier path.","[correct] CSUEBACodecGenerator.scala:196 `val castedName = branchName.toLowerCase` — used at :199 `if (value is $fqBranch $castedName)`. Re-read against source: same lowercase-into-pattern-variable mechanism in the UEBA codec.","[correct] REPRODUCED (orchestrator, executed): model `adt AvatarItem { data Default {} data BuiltIn { id: str } }` reachable via `root data Holder { item: AvatarItem }`, compiled `:cs --generate-json-codecs-by-default=true --generate-ueba-codecs-by-default=true`. Generated Avatar/AvatarItem.cs:437 `if (value is AvatarItem.Default default)` and :499-502 (UEBA). `default` is the C# keyword used as a pattern-capture identifier.","[correct] COMPILE CONFIRMED (dotnet net9.0): the snippet `if (value is AvatarItem.Default default)` fails with `error CS1026: ) expected` at the `default` column (sibling `builtin` branch compiles fine). Replacing with the C#-verbatim identifier `@default` → `Build succeeded`. Confirms both the non-compiling output and the native-fix direction (`@`-prefix).","[correct] NOTE on trigger conditions: per-type codecs are emitted only for `derived[json]`/`derived[ueba]`-annotated types unless `--generate-{json,ueba}-codecs-by-default=true` is passed; a bare unannotated ADT emits the (valid) type definition but no codec, so the collision only surfaces when codecs are generated for the ADT."]
- sessionLogs: ["docs/logs/20260609-190158-orchestrator-h1-repro.md"]

### H2 — confirmed

- createdAt: 2026-06-09T18:52:03.098Z
- updatedAt: 2026-06-09T19:04:01.973Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: The same defect class (model identifier rendered into a target identifier with no reserved-word escaping) affects the other 8 backends (Scala, Python, Rust, TypeScript, Kotlin, Java, Dart, Swift)
- description: "Generalization of H1. Each backend's translator derives target-language identifiers (type names, branch names, field accessors, codec locals/params, discriminator-derived variables) from model names. If none applies a reserved-word escaping/normalization pass native to its target, then any model whose name maps to a target keyword produces invalid code in that backend too. What would make this true: per-language evidence that (a) no reserved-word escaping exists in the identifier-emission path, and (b) at least one concrete collision site exists (e.g. a lowercased branch local, a camelCased field param, a type/namespace name) where a model name equal to a target keyword would be emitted verbatim."
- ledgerRefs: ["defects:D1"]
- evidence: ["[correct] SCALA — NO escaping anywhere. ScJsonCodecGenerator.scala:201 `branchNameRef = branchName.toLowerCase` → :211 `case $branchNameRef: $fqBranch =>` (validated against source); DTO field names verbatim (ScDefnTranslator:307); terminal renderer ScBaboonTranslator:283-290 emits names raw. Scala keywords exclude `default`, but `type`/`val`/`object`/`class`/`match` trigger it. Native fix: backtick `` `name` ``.","[correct] KOTLIN — NO escaping anywhere. KtUEBACodecGenerator.scala:185 `val castedName = branchName.toLowerCase` → :204 `val $castedName = instance` (validated); field/type/package names verbatim. Triggers: `object`/`is`/`when`/`fun`/`val`/`class`. Native fix: backtick `` `name` ``.","[correct] JAVA — NO escaping anywhere (Java has no escape syntax → must rename). JvUEBACodecGenerator.scala:182 `val castedName = branchName.toLowerCase` → :199 `if (value instanceof $adtRef $castedName)` (validated); `default` IS a Java keyword (exact C# trigger). Field/package/class/enum names verbatim. JSON codec uses fixed `branchVal` (safe).","[correct] PYTHON — NO escaping anywhere. Field names verbatim (PyDefnTranslator.scala:540 `q\"$fieldName: $fieldType\"`); class/enum/method names `.capitalize`. No branch-lowercase analog (ADT dispatch via isinstance). Triggers: field/method named `class`/`def`/`import`/`lambda`/`is`/`in`; also `none`/`true`/`false`→`None`/`True`/`False`. Native fix: RENAME (PEP8 trailing `_`), no escape syntax.","[correct] TYPESCRIPT — escaper `escapeTsKeyword` EXISTS (TsTypeTranslator.scala:310-319) but has ZERO callers tree-wide (validated by orchestrator `rg`: dead code). Defect fully applies via field getters/params, UEBA `const <field>` local (TsUEBACodecGenerator:169), enum members, type/class names. ADT branch dispatch by instanceof/index. Native fix: RENAME (no escape syntax; property access can use bracket-string).","[correct] RUST — PARTIAL: escapeRustKeyword(`r#`) + escapeRustModuleName EXIST and ARE applied to field/fn/module names via toSnakeCase (RsDefnTranslator.scala:1533-1546, validated). Residual narrow gap: type/struct/enum/trait + variant names emitted as bare `tid.name.name.capitalize` (RsTypeTranslator:155, validated) with no escape. Rust keywords are lowercase so `.capitalize` dodges most; `self`/`super`/`crate`→`Self`/... collide AND are not r#-escapable. Branch local uses fixed `v` (safe). Lowest-risk backend.","[correct] DART — MIXED: escapeDartKeyword (rename `_`) EXISTS and IS applied to TYPE names via the DtBaboonTranslator render pass (:272-301) + DtServiceWiringTranslator (:359-360) (validated by orchestrator `rg`). But field/getter/method names bypass it: DtDefnTranslator:307/318/538/571 + DtJsonCodecGenerator:164/169 emit `f.name.name` verbatim. Triggers (field/getter/method): `default`/`class`/`final`/`void`/`switch`/`is`/`in`. ADT codec uses fixed `branchVal` (safe). Fix: wire existing helper through member emission.","[correct] SWIFT — MIXED: escapeSwiftKeyword (backticks) EXISTS and IS applied via centralized render pass (SwBaboonTranslator.scala:331-340) + per-site for type/field names. But ADT branch case names + enum case names are built as RAW strings and bypass it: SwDefnTranslator:889-894 + SwJsonCodecGenerator:109-128 + SwUEBACodecGenerator:150-178 produce `case default(...)` for a branch named `Default`. Fix: route case names through escapeSwiftKeyword.","[correct] AGGREGATE: the defect CLASS (a model identifier rendered into a target keyword without escaping) is present in ALL 8 audited backends + C#. The literal 'no escaping anywhere' holds only for C#/Scala/Kotlin/Java/Python; Rust/TS/Dart/Swift have partial escaping with gaps. Native fix mechanism is per-language: C#=`@`-verbatim, Scala/Kotlin/Swift=backticks, Rust=`r#`(+rename for self/super/crate/Self), Java/Python/TypeScript/Dart=RENAME (no escape syntax)."]
- sessionLogs: ["docs/logs/20260609-190158-a5a5a81722f58e956.md","docs/logs/20260609-190158-a6e4b7512b52af94b.md","docs/logs/20260609-190158-a123e05351e969bd7.md","docs/logs/20260609-190158-a339a8ad797fbee13.md","docs/logs/20260609-190158-a119c0b6658e6655f.md","docs/logs/20260609-190158-abf8f375663b3c77b.md","docs/logs/20260609-190158-a6a64824537135162.md","docs/logs/20260609-190158-a3ef4e52b3b442ee7.md"]

## M4

### H3 — confirmed

- createdAt: 2026-06-09T22:19:18.483Z
- updatedAt: 2026-06-09T22:19:18.483Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: JVM-family domain-tree-tools emit the `baboonAdtType` metadata field using the predef/short `Class` (java.lang.Class) reference, which a model ADT branch/type named `Class` shadows
- description: Root cause for D3. The generated per-ADT metadata field `baboonAdtType` references the JVM stdlib `java.lang.Class` by SHORT name (predef) in Scala/Kotlin/Java domain-tree-tools. A model type/branch named `Class` produces a nested type that shadows it -> compile error. C# differs (typeof->System.Type); non-JVM backends have no such field.
- evidence: ["[correct] JvTypes.scala:104 `val javaClass: JvType = JvType(javaLangPkg, \"Class\", predef = true)` — read against source: the Java stdlib Class reference is `predef`, so it renders UNQUALIFIED as `Class` (relies on java.lang auto-import).","[correct] JvDomainTreeTools.scala:67 `q\"public static final $javaClass<?> baboonAdtType\"` — emits `Class<?> baboonAdtType` (short name).","[correct] KtDomainTreeTools.scala:70 `q\"val baboonAdtType: $javaClass<*>\"` — emits `Class<*>` (short name).","[correct] ScDomainTreeTools.scala:67 `q\"def baboonAdtType: $javaClass[?]\"` — emits `Class[?]` (short name; java.lang.Class auto-imported in Scala).","[correct] CONTRAST C#: CSDomainTreeTools.scala:57/97 `public $csTpe BaboonAdtType() => typeof(...)` uses System.Type, not a short `Class` binding — so C# is NOT affected; confirms the defect is JVM-family-specific."]
- ledgerRefs: ["defects:D3"]

### H4 — confirmed

- createdAt: 2026-06-09T22:21:34.303Z
- updatedAt: 2026-06-09T22:21:34.303Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: JvTypeTranslator.renderOwner Owner.Adt arm appends the ADT name raw (unescaped) as a package segment
- description: Root cause for D2.
- evidence: ["[correct] JvTypeTranslator.scala:193 `case Owner.Adt(id) => renderOwner(id.owner) :+ id.name.name` — read against source: the ADT name is appended UNESCAPED, unlike the Owner.Ns arm at :192 `path.map(s => JvTypeTranslator.escapeJvKeyword(s.name.toLowerCase))` which escapes+lowercases. So an ADT used as a package qualifier whose name maps to a Java keyword would emit an illegal package segment. Low severity: ADT names are conventionally capitalized."]
- ledgerRefs: ["defects:D2"]

### H5 — confirmed

- createdAt: 2026-06-09T22:21:38.461Z
- updatedAt: 2026-06-09T22:21:38.461Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: KtServiceWiringTranslator emits service-method call sites `impl.<m>()` with the raw method name, asymmetric with T6's now-escaped declaration
- description: Root cause for D4.
- evidence: ["[correct] KtServiceWiringTranslator.scala: `impl.${m.name.name}(...)` at lines 447, 448, 489, 490, 669, 677, 699, 707, 767, 775 (and symmetric) — read against source: the wiring call sites use the RAW `m.name.name`, while T6 escaped the method DECLARATION (`fun escapeKtKeyword(name)`). For a service method named after a Kotlin hard keyword the declaration would be backtick-escaped but the call site `impl.when(...)` would not → compile error. Low severity: no current fixture has a keyword-named service method."]
- ledgerRefs: ["defects:D4"]

## M7

### H6 — confirmed

- createdAt: 2026-06-09T22:48:49.554Z
- updatedAt: 2026-06-09T22:48:49.554Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Java generated `equals(Object)` (and short-named jvObject/jvString predefs) reference stdlib types by bare name, shadowed by a model type/branch of that name
- description: Root cause for D5 (general stdlib-shadowing class, sibling of D3).
- evidence: ["[correct] JvDefnTranslator.scala:431 `public boolean equals(Object other) {` — read against source: the equals parameter type is the BARE literal `Object` in the q-template, so a model ADT branch/type named `Object` (nested type) shadows java.lang.Object → the override is mistyped → ~15 javac errors. Not fixed by T15 (which only FQ'd `Class` in baboonAdtType).","[correct] JvTypes.scala:100-101 `jvString = JvType(javaLangPkg, \"String\", predef = true)` / `jvObject = JvType(javaLangPkg, \"Object\", predef = true)` — read against source: stdlib String/Object predefs render by SHORT name, same shadowing hazard as the Class predef (JvTypes:104) D3 covered. General class: any JVM stdlib predef short-name ref is shadowable by a model type of that name."]
- ledgerRefs: ["defects:D5"]

### H7 — confirmed

- createdAt: 2026-06-09T22:48:53.930Z
- updatedAt: 2026-06-09T22:48:53.930Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Kotlin client-stub method declarations at KtServiceWiringTranslator:864/882 emit the raw method name (not escaped), unlike T6's escaped interface declaration"
- description: Root cause for D6.
- evidence: ["[correct] KtServiceWiringTranslator.scala:864/882 `suspend fun ${m.name.name}(...)` / `${m.name.name}Json(...)` — confirmed by the T17 reviewer reading source: the client-stub DECLARATIONS use the raw model method name, not routed through escapeKtKeyword. A keyword-named service method emits an unparseable client stub. Transport string args (867/884) correctly stay raw (wire names). Low severity, latent."]
- ledgerRefs: ["defects:D6"]

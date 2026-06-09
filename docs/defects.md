---
ledger: defects
counters:
  milestone: 0
  item: 2
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

### D2 — open

- createdAt: 2026-06-09T21:42:07.419Z
- updatedAt: 2026-06-09T21:42:07.419Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "Java: ADT-name package segment in renderOwner not keyword-escaped"
- description: "Filed by T9 reviewer (out-of-scope for T9's field/accessor/capture-var scope). In JvTypeTranslator.renderOwner (JvTypeTranslator.scala:166), the Owner.Adt arm emits `renderOwner(id.owner) :+ id.name.name` retaining the ADT name at original casing as a package-path segment, unlike the Owner.Ns arm which now lowercases-and-escapes. If a model declared an ADT whose name lowercased to a Java keyword AND used it as a package qualifier, the emitted package segment could be illegal. Latent edge; ADT names are conventionally capitalized so collision is unlikely. Default disposition: FIX (fold into the C#/Java general-pass scope or a follow-up)."
- severity: low
- suggestedFix: Route ADT-as-package-segment names through escapeJvKeyword (as the Ns arm now does), or document the capitalization invariant that makes it collision-free.
- ledgerRefs: ["tasks:T9","goals:G1"]

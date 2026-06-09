---
ledger: goals
counters:
  milestone: 0
  item: 1
archives: []
---

# goals

## M2

### G1 — planning

- createdAt: 2026-06-09T19:05:26.509Z
- updatedAt: 2026-06-09T19:05:26.509Z
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

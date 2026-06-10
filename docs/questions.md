---
ledger: questions
counters:
  milestone: 0
  item: 4
archives: []
---

# questions

## M1

### Q1 — open

- createdAt: 2026-06-09T19:05:45.342Z
- updatedAt: 2026-06-09T19:05:45.342Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- question: "Root cause of D1 (reserved-word identifier collisions in generated code) is confirmed and a defect-seeded goal G1 is ready to plan — run `/cq:plan:advance G1` to produce the reviewed fix tasks."
- context: "D1 root-caused: C# JSON/UEBA codecs lowercase ADT branch names into unescaped pattern-capture variables (branch `Default` → `if (value is AvatarItem.Default default)` → CS1026; reproduced + compile-confirmed). Audit confirmed the same defect CLASS in all 8 other backends with per-language nuance (C#/Scala/Kotlin/Java/Python: no escaping; TypeScript: escaper exists but dead; Rust/Dart/Swift: partial escaping with gaps). Native fix mechanism differs per language (C# `@`, Scala/Kotlin/Swift backticks, Rust `r#`+rename, Java/Python/TS/Dart rename with wire-key preservation). Goal G1 (milestone M2) embeds the confirmed root cause + suggestedFix verbatim and is in `planning` status (defect-seeded → clarifying skipped per T35). Run `/cq:plan:advance G1`."
- ledgerRefs: ["defects:D1","goals:G1"]

## M6

### Q2 — open

- createdAt: 2026-06-09T22:20:35.386Z
- updatedAt: 2026-06-09T22:20:35.386Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- question: "Root cause of D3 (JVM-family `Class`-shadowing in baboonAdtType metadata) confirmed; defect-seeded goal G2 ready to plan. (Auto-launched inside /cq:advance — G2 will be auto-resumed by the parent flow; no manual action needed.)"
- context: "D3 root-caused: Scala/Kotlin/Java domain-tree-tools emit `baboonAdtType` using the predef SHORT `Class` (java.lang.Class) ref (JvTypes:104; JvDomainTreeTools:67 / KtDomainTreeTools:70 / ScDomainTreeTools:67); a model ADT branch/type named `Class` shadows it. Fix: fully-qualify the stdlib Class ref. G2 (milestone M6, planning) consolidates D3 + the pending D2/D4 follow-ups. D3's fix unblocks G1's T14 green gate. This investigation was auto-launched by /cq:advance."
- ledgerRefs: ["defects:D3","goals:G2"]

## M8

### Q3 — open

- createdAt: 2026-06-09T22:50:20.216Z
- updatedAt: 2026-06-09T22:50:20.216Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- question: "Root causes of D5 (stdlib-type-shadowing, general) + D6 (kt client-stub) confirmed; defect-seeded goal G3 ready to plan. (Auto-launched inside /cq:advance — G3 auto-resumed by the parent flow; no manual action needed.)"
- context: "D5 (high): JVM generated code references stdlib types by short name (JvDefnTranslator:431 bare `Object` in equals; jvObject/jvString predefs) → shadowed by model types named Object/String/Class. General FQ fix unblocks G2's T18 + G1's T14. D6 (low): kt client-stub decls (KtServiceWiringTranslator:864/882) unescaped. G3 (M8, planning) consolidates both. Auto-launched by /cq:advance."
- ledgerRefs: ["defects:D5","defects:D6","goals:G3"]

## M11

### Q4 — open

- createdAt: 2026-06-10T00:13:01.114Z
- updatedAt: 2026-06-10T00:13:01.114Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- question: "Root causes of D7 (C# System.Type shadow) + D8 (Java enum String shadow) confirmed; defect-seeded goal G4 ready to plan. (Auto-launched inside /cq:advance — G4 auto-resumed by the parent flow; no manual action needed.)"
- context: "D7 (high, blocks G1 T14 C#): FQ csTpe at CSDomainTreeTools:57/97 (+27-fixture rebaseline). D8 (low): FQ jvString in JvDefnTranslator enum parse template. Both same class as D3/D5. G4 (M11, planning). Auto-launched by /cq:advance."
- ledgerRefs: ["defects:D7","defects:D8","goals:G4"]

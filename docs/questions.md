---
ledger: questions
counters:
  milestone: 0
  item: 1
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

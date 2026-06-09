---
ledger: milestones
counters:
  milestone: 0
  item: 7
archives: []
---

# milestones

## active

### M-AMBIENT — open

- createdAt: 2026-06-09T18:20:20.693Z
- updatedAt: 2026-06-09T18:20:20.693Z
- title: ambient

### M1 — open

- createdAt: 2026-06-09T18:45:58.297Z
- updatedAt: 2026-06-09T18:45:58.297Z
- title: "Investigate: csharp-keyword-collision"
- description: "Investigate generated-code identifier collisions with target-language reserved words. Seed: C# model `adt AvatarItem { data Default {} data BuiltIn { id: str } }` emits incorrect C# (`Default` collides with the `default` keyword). Scope includes auditing all 9 backends for the same reserved-identifier collision class."

### M2 — open

- createdAt: 2026-06-09T19:04:42.486Z
- updatedAt: 2026-06-09T19:04:42.486Z
- title: "Plan: fix reserved-word identifier collisions (D1)"
- description: Plan-flow work milestone for the defect-seeded goal fixing reserved-word identifier collisions across all 9 Baboon backends. Root cause confirmed in investigate milestone M1 (defect D1).

### M3 — open

- createdAt: 2026-06-09T20:46:37.911Z
- updatedAt: 2026-06-09T20:46:37.911Z
- title: "G1-W1: Shared keyword-escaping infrastructure, wire-key-preservation decision, and reserved-word test model"
- description: "Cross-cutting groundwork for G1 (escape reserved-word identifier collisions across 9 backends). Locks the wire-key-preservation contract for RENAME backends (mirroring Rust #[serde(rename)]), and adds the shared reserved-word test model used by the per-language matrix. Precedes per-language work (G1-W2)."

### M4 — open

- createdAt: 2026-06-09T20:46:41.493Z
- updatedAt: 2026-06-09T20:46:41.493Z
- title: "G1-W2: Per-language reserved-word escaping/normalization fixes (9 backends)"
- description: "One fix task per backend (C# split: minimal symptom fix + general pass). Each applies the target-native mechanism at every identifier-emission site and, for RENAME backends, preserves the wire key. Depends on G1-W1."
- dependsOn: ["M3"]

### M5 — open

- createdAt: 2026-06-09T20:46:45.746Z
- updatedAt: 2026-06-09T20:46:45.746Z
- title: "G1-W3: Test-matrix wiring and full cross-language CI verification"
- description: Wire reserved-word stub coverage into per-language test actions, verify generated keyword-named code compiles in all 9 targets and cross-language JSON/UEBA round-trips preserve wire keys. Depends on G1-W2.
- dependsOn: ["M4"]

### M6 — open

- createdAt: 2026-06-09T22:20:06.934Z
- updatedAt: 2026-06-09T22:20:06.934Z
- title: "Plan: fix codegen identifier follow-ups (D3 Class-shadowing; +D2/D4)"
- description: Defect-seeded plan milestone for codegen identifier-correctness follow-ups discovered during G1's implementation. Seeded from D3 (JVM-family `Class`-shadowing in baboonAdtType metadata). Will consolidate D2 (Java renderOwner) and D4 (Kotlin wiring asymmetry) when their investigate passes confirm. D3's fix unblocks G1's T14 green-matrix gate.

### M7 — open

- createdAt: 2026-06-09T22:24:35.870Z
- updatedAt: 2026-06-09T22:24:35.870Z
- title: G2 codegen identifier follow-ups (D3 Class-shadowing + D2/D4)
- description: "Work milestone for goal G2: fix the three confirmed generated-code identifier-correctness defects (D3 JVM-family Class-shadowing in baboonAdtType metadata; D2 Java renderOwner ADT-name package segment; D4 Kotlin service-wiring call sites), then verify generated Scala+Kotlin+Java for the reserved-words-ok model compiles (closes D3, unblocks G1's T14)."

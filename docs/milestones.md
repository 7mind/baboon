---
ledger: milestones
counters:
  milestone: 0
  item: 2
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

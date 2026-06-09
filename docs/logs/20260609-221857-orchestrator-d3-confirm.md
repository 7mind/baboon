# Investigate session log — D3 / H3 (Class-shadowing, orchestrator confirmation)

- Defect: D3
- Hypothesis: H3
- Role: orchestrator (direct source validation, no explorer needed — defect well-characterized by T5/T6 workers)
- Verdict: confirmed

### Summary
- **Mechanism (validated against source):** the generated per-ADT metadata field `baboonAdtType` is emitted using the JVM stdlib `java.lang.Class` reference by SHORT/predef name in the three JVM-family domain-tree-tools:
  - `JvTypes.scala:104` — `val javaClass: JvType = JvType(javaLangPkg, "Class", predef = true)` → rendered unqualified as `Class`.
  - `JvDomainTreeTools.scala:67` — `q"public static final $javaClass<?> baboonAdtType"`.
  - `KtDomainTreeTools.scala:70` — `q"val baboonAdtType: $javaClass<*>"`.
  - `ScDomainTreeTools.scala:67` — `q"def baboonAdtType: $javaClass[?]"`.
  When a model ADT has a branch (or a type) named `Class`, the generated nested type `AvatarItem.Class` shadows `java.lang.Class` in scope, so the metadata field's type resolves to the wrong `Class` → compile errors (~32 in AvatarItem.kt; analogous in Scala/Java).
- **Scope:** JVM-family backends only (Scala, Kotlin, Java) — they emit a `Class`-typed metadata field by short name. C# uses `typeof(...) → System.Type` (`$csTpe`), no short `Class` binding. Rust/TypeScript/Dart/Swift/Python emit no such `Class`-typed field. Same defect CLASS could affect any other predef short-name reference shadowed by a model type/branch of the same name (e.g. `Type`, `String`) — the `Class` case is the reproduced one.
- **Distinct from D1:** `Class` is not a language keyword; keyword-escaping (T3/T5/T6/T9) does not and should not touch it.

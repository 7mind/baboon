package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

/** Kind discriminant for `has (mirror|contract) <Name>` extraction clauses (T37). */
sealed trait ExtractionKind
object ExtractionKind {
  case object Mirror   extends ExtractionKind
  case object Contract extends ExtractionKind
}

sealed trait RawDtoMember

object RawDtoMember {
  case class FieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  /** PR-33.9 (M33): typer-internal carrier for a `RawDtoMember.FieldDef` whose origin is a
    * template-arm inline expansion (`+ Template[Args]` lowered by `TemplateInstantiator`).
    *
    * Behaves identically to `FieldDef` everywhere downstream EXCEPT in
    * `BaboonTranslator.convertDto`, which uses the variant tag to narrow duplicate-name
    * detection: `NonUniqueFields` fires when ≥2 entries with the same lowercased name are
    * `TemplateArmFieldDef` (provenance-aware narrowing closing `[PR-33.3-D01]`). Contract-
    * diamond duplicates (e.g. pkg03 `T4_A1#B1` two `f2: #i32` from `+` ContractRef paths) are
    * left to the existing `.distinct` absorption.
    *
    * This node is NEVER produced by the parser; it is a typer-internal AST node only.
    * Inheriting from `FieldDef` would change `unapply` arity for existing 2-arg pattern
    * extractors (`case RawDtoMember.FieldDef(f, m)`) — so we model it as a sibling sealed-trait
    * variant. The lowering pass in `TemplateInstantiator.convertLoweredArm` (`Plus` arm) maps
    * each `FieldDef` to a `TemplateArmFieldDef`.
    */
  case class TemplateArmFieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  case class UnfieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  case class ParentDef(parent: ScopedRef, meta: RawNodeMeta, args: Option[NEList[RawTypeRef]] = None) extends RawDtoMember

  case class UnparentDef(parent: ScopedRef, meta: RawNodeMeta, args: Option[NEList[RawTypeRef]] = None) extends RawDtoMember

  case class IntersectionDef(parent: ScopedRef, meta: RawNodeMeta, args: Option[NEList[RawTypeRef]] = None) extends RawDtoMember

  case class ContractRef(contract: RawContractRef, meta: RawNodeMeta) extends RawDtoMember

  /** PR-33.2 (M33): typer-internal carrier for `^ Template[Args]` lowered via inline substitution.
    *
    * The typer (`TemplateInstantiator`) emits this node when it lowers an `IntersectionDef` whose
    * `args.isDefined`. The translator (`BaboonTranslator.convertDto`) treats it as an
    * intersection-limiter whose field set is the supplied `fields` list. This node is NEVER
    * produced by the parser; it is a typer-internal AST node only.
    *
    * Why `+`/`-` use M1 inline (FieldDef/UnfieldDef splice) and `^` uses M3 (this branch):
    * - `+`: substituted body's FieldDefs splice directly into the receiving DTO's member list.
    * - `-`: substituted body's FieldDefs are converted to UnfieldDefs and spliced — the existing
    *   `removed` walk handles them by Field equality, identical to `- ParentDto`.
    * - `^`: the existing `IntersectionDef` resolves a parent ScopedRef to a DTO and pulls its full
    *   field set as the intersection-limiter. Inline substitution has no `ScopedRef` to resolve;
    *   we'd need to either synthesize a domain member (forbidden by §3.b Option I) or invent a
    *   carrier whose field set survives to the translator. This branch is the smallest such
    *   carrier — one new sealed-trait arm with a 4-site exhaustive-match cost (M29 close-out
    *   playbook), which is cheaper than synthesising a DomainMember + later strip pass.
    */
  case class IntersectionFields(fields: Seq[RawDtoMember.FieldDef], meta: RawNodeMeta) extends RawDtoMember

  /** T37: parser-produced carrier for `has (mirror|contract) <Name>` extraction clauses.
    *
    * Grammar: `has (mirror | contract) <bare-identifier>` — no scoped/qualified names, no type args.
    * The parser accepts this clause in any dto-shaped body (data, id, contract, adt arms).
    * Host validation (templated-id-only vs non-templated-id-invalid, etc.) is deferred to T38.
    *
    * Until T38 lands the translator sees this as a no-op (explicit arm, never unhandled).
    */
  case class ExtractionDef(kind: ExtractionKind, name: RawTypeName, meta: RawNodeMeta) extends RawDtoMember

}

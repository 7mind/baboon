package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawDtoMember

object RawDtoMember {
  case class FieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

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

}

package io.septimalmind.baboon.parser.model

sealed trait RawDtoMember

object RawDtoMember {
  case class FieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  case class UnfieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  case class ParentDef(parent: ScopedRef, meta: RawNodeMeta)
      extends RawDtoMember

  case class UnparentDef(parent: ScopedRef, meta: RawNodeMeta)
      extends RawDtoMember

  case class IntersectionDef(parent: ScopedRef, meta: RawNodeMeta)
      extends RawDtoMember

  case class ContractRef(contract: RawContractRef, meta: RawNodeMeta)
      extends RawDtoMember

}

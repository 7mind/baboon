package io.septimalmind.baboon.parser.model

sealed trait RawDtoMember

object RawDtoMember {
  case class FieldDef(field: RawField, meta: RawNodeMeta) extends RawDtoMember

  case class ParentDef(parent: ScopedRef, meta: RawNodeMeta)
      extends RawDtoMember
}

package io.septimalmind.baboon.parser.model

sealed trait RawDefn {
  def name: RawTypeName
  def meta: RawNodeMeta
}

case class RawDto(name: RawTypeName,
                  members: Seq[RawDtoMember],
                  meta: RawNodeMeta)
    extends RawDefn

case class RawEnum(name: RawTypeName,
                   members: Seq[RawEnumMember],
                   meta: RawNodeMeta)
    extends RawDefn

case class RawAdt(name: RawTypeName,
                  members: Seq[RawAdtMember],
                  meta: RawNodeMeta)
    extends RawDefn

case class RawForeign(name: RawTypeName,
                      defns: Map[String, String],
                      meta: RawNodeMeta)
    extends RawDefn

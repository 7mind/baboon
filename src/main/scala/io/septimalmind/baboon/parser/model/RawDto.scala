package io.septimalmind.baboon.parser.model

import io.septimalmind.baboon.parser.model.RawDtoMember.ContractRef

sealed trait RawDefn {
  def name: RawTypeName
  def meta: RawNodeMeta
}

sealed trait RawDtoid {
  this: RawDefn =>
  def name: RawTypeName
  def members: Seq[RawDtoMember]
  def meta: RawNodeMeta
}
case class RawDto(name: RawTypeName,
                  members: Seq[RawDtoMember],
                  meta: RawNodeMeta)
    extends RawDefn
    with RawDtoid

case class RawContract(name: RawTypeName,
                       members: Seq[RawDtoMember],
                       meta: RawNodeMeta)
    extends RawDefn
    with RawDtoid

case class RawEnum(name: RawTypeName,
                   members: Seq[RawEnumMember],
                   meta: RawNodeMeta)
    extends RawDefn

case class RawAdt(name: RawTypeName,
                  members: Seq[RawAdtMember],
                  contracts: Seq[ContractRef],
                  meta: RawNodeMeta)
    extends RawDefn

case class RawForeign(name: RawTypeName,
                      defns: Map[String, String],
                      meta: RawNodeMeta)
    extends RawDefn

case class RawNamespace(name: RawTypeName,
                        defns: Seq[RawTLDef],
                        meta: RawNodeMeta)
    extends RawDefn

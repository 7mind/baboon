package io.septimalmind.baboon.parser.model

sealed trait RawAdtMember {
  def meta: RawNodeMeta
  def defn: RawDefn
}

case class RawAdtMemberDto(dto: RawDto, meta: RawNodeMeta) extends RawAdtMember {
  override def defn: RawDefn = dto
}

case class RawAdtMemberContract(contract: RawContract, meta: RawNodeMeta) extends RawAdtMember {
  override def defn: RawDefn = contract
}

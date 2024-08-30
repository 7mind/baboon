package io.septimalmind.baboon.parser.model

sealed trait RawAdtMember {
  def meta: RawNodeMeta
}

case class RawAdtMemberDto(dto: RawDto, meta: RawNodeMeta) extends RawAdtMember

case class RawAdtMemberContract(contract: RawContract, meta: RawNodeMeta)
    extends RawAdtMember

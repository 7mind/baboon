package io.septimalmind.baboon.parser.model

case class RawDto(name: RawTypeName, members: Seq[RawDtoMember], meta: RawNodeMeta)

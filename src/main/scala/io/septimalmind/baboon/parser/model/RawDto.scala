package io.septimalmind.baboon.parser.model

case class RawDto(name: TypeName, members: Seq[RawDtoMember], meta: RawNodeMeta)

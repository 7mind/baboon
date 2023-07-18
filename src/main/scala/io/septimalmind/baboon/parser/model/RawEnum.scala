package io.septimalmind.baboon.parser.model

case class RawEnum(name: TypeName,
                   members: Seq[RawEnumMember],
                   meta: RawNodeMeta)

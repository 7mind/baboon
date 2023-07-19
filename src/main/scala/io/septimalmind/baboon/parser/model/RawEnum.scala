package io.septimalmind.baboon.parser.model

case class RawEnum(name: RawTypeName,
                   members: Seq[RawEnumMember],
                   meta: RawNodeMeta)

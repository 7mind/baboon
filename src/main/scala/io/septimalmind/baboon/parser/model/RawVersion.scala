package io.septimalmind.baboon.parser.model

case class RawVersion(meta: RawNodeMeta, value: String)

case class RawInclude(meta: RawNodeMeta, value: String)

case class RawContent(includes: Seq[RawInclude], defs: Seq[RawTLDef])

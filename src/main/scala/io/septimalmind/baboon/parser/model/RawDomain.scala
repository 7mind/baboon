package io.septimalmind.baboon.parser.model

case class RawVersion(meta: RawNodeMeta, value: String)

case class RawDomain(header: RawHeader,
                     version: RawVersion,
                     members: Seq[RawTLDef])

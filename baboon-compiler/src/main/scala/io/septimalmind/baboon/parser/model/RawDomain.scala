package io.septimalmind.baboon.parser.model

case class RawPragma(key: String, value: String)

case class RawDomain(header: RawHeader, version: RawVersion, pragmas: Seq[RawPragma], imported: Option[RawImport], members: RawContent)

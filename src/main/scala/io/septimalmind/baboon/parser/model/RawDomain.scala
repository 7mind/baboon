package io.septimalmind.baboon.parser.model

case class RawDomain(header: RawHeader, version: RawVersion, imported: Option[RawImport], members: RawContent)

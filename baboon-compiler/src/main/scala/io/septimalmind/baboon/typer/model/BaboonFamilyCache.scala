package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.{FSPath, RawDomain}

case class BaboonFamilyCache(
  fileContents: Map[FSPath, String],
  parsedByPath: Map[FSPath, RawDomain],
  typedByKey: Map[DomainKey, Domain],
  domainFiles: Map[DomainKey, Set[FSPath]],
  domainPrimaryFiles: Map[DomainKey, FSPath],
  fileToDomains: Map[FSPath, Set[DomainKey]],
  depsByKey: Map[DomainKey, Set[DomainKey]],
)

object BaboonFamilyCache {
  val empty: BaboonFamilyCache = BaboonFamilyCache(
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
  )
}

package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyMap

case class BaboonFamily(domains: NonEmptyMap[Pkg, BaboonLineage]) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"Families: ${domains.toMap.values.toList.niceList(prefix = "* ")}"
}

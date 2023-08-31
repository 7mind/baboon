package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEMap

case class BaboonFamily(domains: NEMap[Pkg, BaboonLineage]) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"Families: ${domains.toMap.values.toList.niceList(prefix = "* ")}"
}

package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEMap

case class BaboonFamily(domains: NEMap[Pkg, BaboonLineage], cache: BaboonFamilyCache) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String = {
    s"Families: ${domains.toMap.values.toList.niceList(prefix = "* ")}"
  }
}

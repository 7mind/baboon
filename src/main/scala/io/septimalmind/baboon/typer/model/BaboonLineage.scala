package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NonEmptyMap

case class BaboonLineage(pkg: Pkg,
                         versions: NonEmptyMap[Version, Domain],
                         evolution: BaboonEvolution) {
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"""${pkg}: ${versions.toMap.values
         .niceList(prefix = "\uD83D\uDE4A ")
         .shift(2)}
       |evolution: ${evolution.toString.shift(2)}
       |""".stripMargin
}

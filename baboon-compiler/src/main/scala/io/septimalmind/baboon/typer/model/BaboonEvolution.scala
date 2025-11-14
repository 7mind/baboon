package io.septimalmind.baboon.typer.model

import izumi.fundamentals.platform.strings.IzString.*

case class BaboonEvolution(
  pkg: Pkg,
  latest: Version,
  diffs: Map[EvolutionStep, BaboonDiff],
  rules: Map[EvolutionStep, BaboonRuleset],
  typesUnchangedSince: Map[Version, Map[TypeId, UnmodifiedSince]],
) {
  override def toString: String = {
    diffs.map {
      case (v, diff) =>
        val modRepr = diff.diffs.map { case (id, d) => s"$id = ${d.ops.niceList().shift(2)}" }
        val ruleset = rules(v).conversions
        s"""$v => $latest:
           |${diff.changes.toString.shift(2)}
           |Modifications: ${modRepr.niceList().shift(2)}
           |Rules: ${ruleset.niceList().shift(2)}""".stripMargin
    }.niceList()
  }
}

case class EvolutionStep(from: Version, to: Version) {
  override def toString: String = s"$from->$to"
}

case class BaboonDiff(id: EvolutionStep, changes: BaboonChanges, diffs: Map[TypeId, TypedefDiff])

case class BaboonChanges(
  added: Set[TypeId],
  removed: Set[TypeId],
  unmodified: Set[TypeId],
  shallowModified: Set[TypeId],
  deepModified: Set[TypeId],
  fullyModified: Set[TypeId],
) {
  def changed: Set[TypeId] = shallowModified ++ deepModified ++ fullyModified

  override def toString: String = {
    List(
      s"Added: ${added.niceList()}",
      s"Removed: ${removed.niceList()}",
      s"Unmodified: ${unmodified.niceList()}",
      s"Modified (shallow): ${shallowModified.niceList()}",
      s"Modified (deep): ${deepModified.niceList()}",
      s"Modified (full): ${fullyModified.niceList()}",
    ).mkString("\n")
  }
}

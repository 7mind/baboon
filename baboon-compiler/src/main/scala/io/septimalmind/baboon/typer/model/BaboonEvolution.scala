package io.septimalmind.baboon.typer.model

import izumi.fundamentals.platform.strings.IzString.*

case class BaboonEvolution(
  pkg: Pkg,
  latest: Version,
  diffs: Map[EvolutionStep, BaboonDiff],
  rules: Map[EvolutionStep, BaboonRuleset],
  typesUnchangedSince: Map[Version, Map[TypeId, UnmodifiedSince]],
  typesForwardReadable: Map[Version, Map[TypeId, ForwardReadable]],
) {
  /** Inverse of [[typesForwardReadable]], from the WRITER's point of view: for a type
    * as encoded by `version`, the oldest reader version that can decode it under each
    * capability. Readers form a contiguous range per capability (a shorter chain is a
    * sub-chain), so one lower bound per capability is exact. The `Identical` bound
    * equals the type's `sameIn` head.
    *
    * Capabilities are resolved independently, so a chain that is UEBA-readable but
    * not JSON-readable (a declared rename) publishes the `prefix-*` bounds and
    * withholds `json-additive`, and a chain that is JSON-readable but not
    * UEBA-readable (a reorder or a mid-position insert) does the reverse.
    */
  def minReaders(version: Version, id: TypeId): Map[ForwardCompatTier, Version] = {
    val readers = typesForwardReadable.toList.collect {
      case (readerVersion, types) if readerVersion <= version =>
        types.get(id).flatMap(_.tierFor(version)).map(guarantee => (readerVersion, guarantee))
    }.flatten

    ForwardCompatTier.all.flatMap {
      cap =>
        readers
          .collect { case (readerVersion, guarantee) if guarantee.grants(cap) => readerVersion }
          .minOption(Version.ordering)
          .map(v => (cap, v))
    }.toMap
  }
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
  renamed: Map[TypeId.User, TypeId.User],
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
      s"Renamed: ${renamed.map { case (n, o) => s"$o -> $n" }.niceList()}",
    ).mkString("\n")
  }
}

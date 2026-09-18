package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.{BaboonEvolution, TypeId, Version}

final case class EvolutionMetadataPlan(
  sameIn: List[EvolutionMetadataPlan.SameIn],
  forwardReadable: List[EvolutionMetadataPlan.ForwardReadable],
)

object EvolutionMetadataPlan {
  final case class SameIn(typeId: TypeId, versions: List[String])
  final case class ReaderVersion(version: String, tier: String)
  final case class ForwardReadable(typeId: TypeId, versions: List[ReaderVersion])

  def apply(evolution: BaboonEvolution, version: Version): EvolutionMetadataPlan = {
    val sameIn = evolution.typesUnchangedSince(version).toList.sortBy(_._1.toString).map {
      case (id, unchanged) => SameIn(id, unchanged.sameIn.toList.map(_.v.toString))
    }
    val forwardReadable = evolution.typesForwardReadable(version).toList.sortBy(_._1.toString).map {
      case (id, readable) =>
        ForwardReadable(id, readable.readable.toList.map { case (v, tier) => ReaderVersion(v.v.toString, tier.wireName) })
    }
    EvolutionMetadataPlan(sameIn, forwardReadable)
  }
}

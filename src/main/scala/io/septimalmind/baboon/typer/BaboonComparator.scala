package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{
  BaboonDiff,
  BaboonEvolution,
  Domain,
  Pkg,
  Version
}
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*

trait BaboonComparator {
  def evolve(
    pkg: Pkg,
    versions: NonEmptyMap[Version, Domain]
  ): Either[NonEmptyList[BaboonIssue], BaboonEvolution]
}

object BaboonComparator {

  class BaboonComparatorImpl() extends BaboonComparator {
    private val enquiries = new BaboonEnquiries.BaboonEnquiriesImpl()

    override def evolve(
      pkg: Pkg,
      versions: NonEmptyMap[Version, Domain]
    ): Either[NonEmptyList[BaboonIssue], BaboonEvolution] = {
      val sortedVersions =
        versions.keySet.toList.sortBy(_.version)(Ordering.String.reverse)
      val pinnacleVersion = sortedVersions.head
      val prior = sortedVersions.tail

      val pinnacle = versions(pinnacleVersion)

      for {
        indexedDiffs <- prior
          .map(v => compare(pinnacle, versions(v)).map(diff => (v, diff)))
          .biAggregate
        asMap <- indexedDiffs.toUniqueMap(
          e => NonEmptyList(BaboonIssue.TODOEvoIssue())
        )
      } yield {
        BaboonEvolution(pkg, pinnacleVersion, asMap)
      }

    }

    private def compare(
      last: Domain,
      prev: Domain
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], BaboonDiff] = {
      val newTypes = last.defs.meta.nodes.keySet
      val oldTypes = prev.defs.meta.nodes.keySet

      val kept = newTypes.intersect(oldTypes)
      val added = newTypes.diff(oldTypes)
      val removed = oldTypes.diff(newTypes)
      println(last)

      ???
    }
  }

}

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

      val unmodified = kept.filter { id =>
        last.shallowSchema(id) == prev.shallowSchema(id) &&
        last.deepSchema(id) == prev.deepSchema(id)
      }

      val changed = kept.diff(unmodified)

      // different local structure and different dependencies
      val completelyModified = changed.filter { id =>
        last.shallowSchema(id) != prev.shallowSchema(id) &&
        last.deepSchema(id) != prev.deepSchema(id)
      }

      val partiallyModified = changed.diff(completelyModified)

      // same dependencies, different local structure
      val shallowModified = partiallyModified.filter { id =>
        last.shallowSchema(id) != prev.shallowSchema(id)
      }

      // same local structure, different dependencies
      val deepModified = partiallyModified.filter { id =>
        last.deepSchema(id) != prev.deepSchema(id)
      }

      assert(shallowModified.intersect(deepModified).isEmpty)
      assert(shallowModified.intersect(completelyModified).isEmpty)
      assert(deepModified.intersect(completelyModified).isEmpty)
      assert(changed.intersect(unmodified).isEmpty)
      assert(kept.intersect(added).isEmpty)
      assert(kept.intersect(removed).isEmpty)
      assert(removed.intersect(added).isEmpty)

//      println(s"added: $added")
//      println(s"removed: $removed")
//      println(s"kept: $kept")
//      println(s"* unmodified: $unmodified")
//      println(s"* locallyModified: $completelyModified")
//      println(s"* shallowModified: $shallowModified")
//      println(s"* deepmodified: $deepModified")
//
//      println("OLD:")
//      println(prev)
//      println("NEW:")
//      println(last)

      ???
    }
  }

}

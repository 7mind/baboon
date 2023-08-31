package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}
import izumi.fundamentals.platform.strings.TextTree.Quote

trait BaboonComparator {
  def evolve(
    pkg: Pkg,
    versions: NonEmptyMap[Version, Domain]
  ): Either[NonEmptyList[BaboonIssue], BaboonEvolution]
}

object BaboonComparator {

  class BaboonComparatorImpl(enquiries: BaboonEnquiries,
                             rules: BaboonRules,
                             logger: BLogger)
      extends BaboonComparator {
    override def evolve(
      pkg: Pkg,
      versions: NonEmptyMap[Version, Domain]
    ): Either[NonEmptyList[BaboonIssue], BaboonEvolution] = {
      val sortedVersions =
        versions.keySet.toList.sortBy(_.version)(Ordering.String.reverse)
      val pinnacleVersion = sortedVersions.head

      val toCompare = sortedVersions.sliding(2).toList

      logger.message(
        pkg.toString,
        q"conversions chain: ${toCompare.map(_.mkString("<-")).mkString("; ")}"
      )

      for {
        indexedDiffs <- toCompare.map {
          case fresh :: old :: Nil =>
            compare(versions(fresh), versions(old))
              .map(diff => (diff.id, diff))
          case o =>
            Left(NonEmptyList(BaboonIssue.BrokenComparison(o)))
        }.biAggregate
        diffMap <- indexedDiffs.toUniqueMap(
          e => NonEmptyList(BaboonIssue.NonUniqueDiff(e))
        )

        rulesets <- diffMap.map {
          case (v, diff) =>
            rules
              .compute(versions(v.from), versions(v.to), diff)
              .map(rs => (v, rs))
        }.biAggregate
        rulesetMap <- rulesets.toUniqueMap(
          e => NonEmptyList(BaboonIssue.NonUniqueRuleset(e))
        )
      } yield {
        BaboonEvolution(pkg, pinnacleVersion, diffMap, rulesetMap)
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
      val fullyModified = changed.filter { id =>
        last.shallowSchema(id) != prev.shallowSchema(id) &&
        last.deepSchema(id) != prev.deepSchema(id)
      }

      val partiallyModified = changed.diff(fullyModified)

      // same dependencies, different local structure
      val shallowModified = partiallyModified.filter { id =>
        last.shallowSchema(id) != prev.shallowSchema(id)
      }

      // same local structure, different dependencies
      val deepModified = partiallyModified.filter { id =>
        last.deepSchema(id) != prev.deepSchema(id)
      }

      assert(shallowModified.intersect(deepModified).isEmpty)
      assert(shallowModified.intersect(fullyModified).isEmpty)
      assert(deepModified.intersect(fullyModified).isEmpty)
      assert(changed.intersect(unmodified).isEmpty)
      assert(kept.intersect(added).isEmpty)
      assert(kept.intersect(removed).isEmpty)
      assert(removed.intersect(added).isEmpty)

      assert(partiallyModified == shallowModified ++ deepModified)
      assert(changed == partiallyModified ++ fullyModified)
      assert(added ++ unmodified ++ changed == newTypes)
      assert(removed ++ unmodified ++ changed == oldTypes)

      assert(changed.forall(_.isInstanceOf[TypeId.User]))

//      println(s"modified: $changed")
//      println(s"added: $added")
//      println(s"removed: $removed")
//      println(s"kept: $kept")
//      println(s"* unmodified: $unmodified")
//      println(s"* locallyModified: $fullyModified")
//      println(s"* shallowModified: $shallowModified")
//      println(s"* deepmodified: $deepModified")
//
//      println("OLD:")
//      println(prev)
//      println("NEW:")
//      println(last)

      val changes = BaboonChanges(
        added,
        removed,
        unmodified,
        shallowModified,
        deepModified,
        fullyModified
      )

      for {
        diffs <- changed.toList.map { id =>
          val defOld = prev.defs.meta.nodes(id)
          val defNew = last.defs.meta.nodes(id)

          (defOld, defNew) match {
            case (uold: DomainMember.User, unew: DomainMember.User) =>
              diff(changes, uold.defn, unew.defn).map(diff => (id, diff))

            case (o, n) =>
              Left(NonEmptyList(BaboonIssue.IncomparableTypedefs(o, n)))
          }

        }.biAggregate
        indexedDiffs <- diffs.toUniqueMap(
          e => NonEmptyList(BaboonIssue.NonUniqueDiffs(e))
        )
      } yield {
        BaboonDiff(
          EvolutionStep(prev.version, last.version),
          changes,
          indexedDiffs
        )
      }
    }

    private def diff(
      changes: BaboonChanges,
      prevDef: Typedef.User,
      nextDef: Typedef.User
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      (prevDef, nextDef) match {
        case (e1: Typedef.Enum, e2: Typedef.Enum) =>
          diffEnums(e1, e2)
        case (a1: Typedef.Adt, a2: Typedef.Adt) =>
          diffAdts(changes, a1, a2)
        case (d1: Typedef.Dto, d2: Typedef.Dto) =>
          diffDtos(changes, d1, d2)
        case (o1, o2) =>
          Left(NonEmptyList(BaboonIssue.MismatchingTypedefs(o1, o2)))
      }
    }

    private def diffEnums(
      e1: Typedef.Enum,
      e2: Typedef.Enum
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      val members1 = e1.members.map(m => (m.name, m)).toMap
      val members2 = e2.members.map(m => (m.name, m)).toMap

      val names1 = members1.keySet
      val names2 = members2.keySet
      val removedMembers = names1.diff(names2)
      val addedMembers = names2.diff(names1)
      val keptMembers = names1.intersect(names2)

      val ops = List(
        removedMembers.map(id => EnumOp.RemoveBranch(members1(id))),
        addedMembers.map(id => EnumOp.AddBranch(members2(id))),
        keptMembers.map(id => EnumOp.KeepBranch(members2(id))),
      ).flatten

      Right(TypedefDiff.EnumDiff(ops))
    }

    private def diffAdts(
      changes: BaboonChanges,
      a1: Typedef.Adt,
      a2: Typedef.Adt
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      val members1 = a1.members.toSet
      val members2 = a2.members.toSet

      val removedMembers = members1.diff(members2)
      val addedMembers = members2.diff(members1)
      val keptMembers = members1.intersect(members2).map { ref =>
        val modification =
          figureOutModification(changes, Set(ref))

        AdtOp.KeepBranch(ref, modification)
      }

      val ops = List(
        removedMembers.map(id => AdtOp.RemoveBranch(id)),
        addedMembers.map(id => AdtOp.AddBranch(id)),
        keptMembers,
      ).flatten

      Right(TypedefDiff.AdtDiff(ops))
    }

    private def diffDtos(
      changes: BaboonChanges,
      d1: Typedef.Dto,
      d2: Typedef.Dto
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      val members1 = d1.fields.map(m => (m.name, m)).toMap
      val members2 = d2.fields.map(m => (m.name, m)).toMap

      val names1 = members1.keySet
      val names2 = members2.keySet
      val removedMembers = names1.diff(names2)
      val addedMembers = names2.diff(names1)

      val keptMembers = names1.intersect(names2)

      val keptFields = keptMembers.map(name => (members1(name), members2(name)))
      val changedFields = keptFields.filter {
        case (f1, f2) =>
          f1.tpe != f2.tpe
      }
      val unchangedFields = keptFields
        .filter {
          case (f1, f2) =>
            f1.tpe == f2.tpe
        }
        .map {
          case (_, f2) =>
            val directRefs = enquiries.explode(f2.tpe)
            val modification =
              figureOutModification(changes, directRefs)

            DtoOp.KeepField(f2, modification)
        }

      val ops = List(
        removedMembers.map(id => DtoOp.RemoveField(members1(id))),
        addedMembers.map(id => DtoOp.AddField(members2(id))),
        changedFields.map(id => DtoOp.ChangeField(id._1, id._2.tpe)),
        unchangedFields,
      ).flatten

      Right(TypedefDiff.DtoDiff(ops))
    }

    private def figureOutModification(
      changes: BaboonChanges,
      directRefs: Set[TypeId]
    ): RefModification = {
      if (directRefs.exists(id => changes.changed.contains(id))) {

        if (directRefs.exists(id => changes.fullyModified.contains(id))) {
          RefModification.Full
        } else if (directRefs.exists(
                     id => changes.shallowModified.contains(id)
                   )) {
          RefModification.Shallow
        } else {
          assert(
            directRefs.forall(
              id =>
                changes.unmodified.contains(id) || changes.deepModified
                  .contains(id)
            )
          )
          RefModification.Deep
        }

      } else {
        RefModification.Unchanged
      }
    }
  }

}

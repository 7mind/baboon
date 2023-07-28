package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}

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
              diff(prev, last, changes, uold.defn, unew.defn).map(
                diff => (id, diff)
              )

            case _ =>
              Left(NonEmptyList(BaboonIssue.TODOEvoIssue()))
          }

        }.biAggregate
        indexedDiffs <- diffs.toUniqueMap(
          _ => NonEmptyList(BaboonIssue.TODOEvoIssue())
        )
      } yield {
        BaboonDiff(changes, indexedDiffs)
      }
    }

    private def diff(
      prev: Domain,
      last: Domain,
      changes: BaboonChanges,
      prevDef: Typedef.User,
      nextDef: Typedef.User
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      (prevDef, nextDef) match {
        case (e1: Typedef.Enum, e2: Typedef.Enum) =>
          diffEnums(e1, e2)
        case (a1: Typedef.Adt, a2: Typedef.Adt) =>
          diffAdts(prev, last, a1, a2)
        case (d1: Typedef.Dto, d2: Typedef.Dto) =>
          diffDtos(changes, d1, d2)
        case (o1, o2) =>
          Left(NonEmptyList(BaboonIssue.TODOEvoIssue()))
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
      prev: Domain,
      last: Domain,
      a1: Typedef.Adt,
      a2: Typedef.Adt
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], TypedefDiff] = {
      Right(TypedefDiff.AdtDiff(List.empty))
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
              if (directRefs.exists(id => changes.changed.contains(id))) {

                if (directRefs.exists(id => changes.fullyModified.contains(id))) {
                  FieldModification.Full
                } else if (directRefs.exists(
                             id => changes.shallowModified.contains(id)
                           )) {
                  FieldModification.Shallow
                } else {
                  assert(
                    directRefs.forall(
                      id =>
                        changes.unmodified.contains(id) || changes.deepModified
                          .contains(id)
                    )
                  )
                  FieldModification.Deep
                }

              } else {
                FieldModification.Unchanged
              }

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

  }

}

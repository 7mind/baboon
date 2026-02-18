package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, EvolutionIssue}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.collection.mutable

trait BaboonComparator[F[+_, +_]] {
  def evolve(
    pkg: Pkg,
    versions: NEMap[Version, Domain],
  ): F[NEList[BaboonIssue], BaboonEvolution]
}

object BaboonComparator {

  class BaboonComparatorImpl[F[+_, +_]: Error2](
    enquiries: BaboonEnquiries,
    rules: BaboonRules[F],
    logger: BLogger,
  ) extends BaboonComparator[F] {

    override def evolve(
      pkg: Pkg,
      versions: NEMap[Version, Domain],
    ): F[NEList[BaboonIssue], BaboonEvolution] = {
      val sortedVersions =
        versions.keySet.toList.sorted(Version.ordering.reverse)
      val pinnacleVersion = sortedVersions.head

      val toCompare = sortedVersions.sliding(2).toList

      logger.message(
        pkg.toString,
        q"conversions chain: ${toCompare.map(_.mkString("<-")).mkString("; "): AnyRef}",
      )

      for {
        indexedDiffs <-
          if (sortedVersions.size == 1) { F.pure(List.empty) }
          else {
            F.traverseAccumErrors(toCompare) {
              case fresh :: old :: Nil =>
                compare(versions(fresh), versions(old))
                  .map(diff => (diff.id, diff))

              case o =>
                F.fail(BaboonIssue.of(EvolutionIssue.BrokenComparison(o)))
            }
          }
        diffMap <- F.fromEither {
          indexedDiffs.toUniqueMap(e => BaboonIssue.of(EvolutionIssue.NonUniqueDiff(e)))
        }

        rulesets <- F.sequenceAccumErrors(diffMap.map {
          case (v, diff) =>
            rules
              .compute(versions(v.from), versions(v.to), diff)
              .map(rs => (v, rs))
        })
        rulesetMap <- F.fromEither {
          rulesets.toUniqueMap(e => BaboonIssue.of(EvolutionIssue.NonUniqueRuleset(e)))
        }

        previousVersions <- F.fromEither {
          sortedVersions
            .sliding(2)
            .filter(_.size == 2)
            .flatMap {
              case n :: p :: Nil => List((n, p))
              case _             => List.empty
            }
            .toSeq
            .toUniqueMap(e => BaboonIssue.of(EvolutionIssue.NonUniquePrevVersions(e)))
        }

        minVersions <- computeMinVersions(
          versions,
          diffMap,
          previousVersions,
          sortedVersions.reverse,
        )
      } yield {
        BaboonEvolution(pkg, pinnacleVersion, diffMap, rulesetMap, minVersions)
      }
    }

    private def computeMinVersions(
      domainVersions: NEMap[Version, Domain],
      diffs: Map[EvolutionStep, BaboonDiff],
      previousVersions: Map[Version, Version],
      versions: Seq[Version],
    ): F[NEList[BaboonIssue], Map[Version, Map[TypeId, UnmodifiedSince]]] = {

      for {
        out <- F.foldLeft(versions)(Map.empty[Version, Map[TypeId, UnmodifiedSinceMut]]) {
          case (acc, version) =>
            minVersionsDiff(domainVersions, diffs, previousVersions, acc, version)
        }
      } yield {
        out.map {
          case (v, tv) =>
            (
              v,
              tv.map {
                case (t, u) =>
                  (t, u.freeze)
              },
            )
        }
      }

    }

    case class UnmodifiedSinceMut(typeId: TypeId, in: Version, sameInAfter: mutable.ArrayBuffer[Version]) {
      def freeze: UnmodifiedSince = UnmodifiedSince(typeId, in, NEList.unsafeFrom(sameInAfter.toList))
    }

    private def minVersionsDiff(
      domainVersions: NEMap[Version, Domain],
      diffs: Map[EvolutionStep, BaboonDiff],
      previousVersions: Map[Version, Version],
      minVersions: Map[Version, Map[TypeId, UnmodifiedSinceMut]],
      currVersion: Version,
    ): F[NEList[BaboonIssue], Map[Version, Map[TypeId, UnmodifiedSinceMut]]] = {
      previousVersions.get(currVersion) match {
        case Some(prevVersion) =>
          val step       = EvolutionStep(prevVersion, currVersion)
          val diff       = diffs(step)
          val unmodified = diff.changes.unmodified

          val update = domainVersions(currVersion).defs.meta.nodes.map {
            case (id, _) =>
              if (unmodified.contains(id)) {
                val prevrecord = minVersions(prevVersion)(id)
                prevrecord.sameInAfter.addOne(currVersion)
                (id, UnmodifiedSinceMut(id, currVersion, prevrecord.sameInAfter))
              } else {
                (id, UnmodifiedSinceMut(id, currVersion, mutable.ArrayBuffer(currVersion)))
              }
          }

          F.pure(minVersions.updated(currVersion, update))

        case None =>
          // initial version
          F.pure(Map(currVersion -> domainVersions(currVersion).defs.meta.nodes.map {
            case (id, _) =>
              (id, UnmodifiedSinceMut(id, currVersion, mutable.ArrayBuffer(currVersion)))
          }))

      }
    }

    private def compare(
      last: Domain,
      prev: Domain,
    ): F[NEList[BaboonIssue], BaboonDiff] = {
      val newTypes = last.defs.meta.nodes.keySet
      val oldTypes = prev.defs.meta.nodes.keySet

      // Identify valid renames: new type has was[] pointing to an existing old type
      val validRenames: Map[TypeId.User, TypeId.User] = last.renames.filter {
        case (newId, oldId) =>
          newTypes.contains(newId) && oldTypes.contains(oldId) && !newTypes.contains(oldId)
      }
      val renamedNewIds = validRenames.keySet.asInstanceOf[Set[TypeId]]
      val renamedOldIds = validRenames.values.toSet.asInstanceOf[Set[TypeId]]

      val kept    = newTypes.intersect(oldTypes)
      val added   = newTypes.diff(oldTypes).diff(renamedNewIds)
      val removed = oldTypes.diff(newTypes).diff(renamedOldIds)

      val unmodified = kept.filter {
        id =>
          last.typeMeta(id).shallowId == prev.typeMeta(id).shallowId &&
          last.typeMeta(id).deepId == prev.typeMeta(id).deepId
      }

      val changed = kept.diff(unmodified)

      // different local structure or different dependencies
      val fullyModified = changed.filter {
        id =>
          last.typeMeta(id).shallowId != prev.typeMeta(id).shallowId &&
          last.typeMeta(id).deepId != prev.typeMeta(id).deepId
      }

      val partiallyModified = changed.diff(fullyModified)

      // same dependencies, different local structure
      val shallowModified = partiallyModified.filter {
        id =>
          last.typeMeta(id).shallowId != prev.typeMeta(id).shallowId
      }

      // same local structure, different dependencies
      val deepModified = partiallyModified.filter {
        id =>
          last.typeMeta(id).deepId != prev.typeMeta(id).deepId
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

      assert(changed.forall(_.isInstanceOf[TypeId.User]))

      val changes = BaboonChanges(
        added,
        removed,
        unmodified,
        shallowModified,
        deepModified,
        fullyModified,
        validRenames,
      )

      for {
        // Compute diffs for types that kept the same ID
        keptDiffs <- F.traverseAccumErrors(changed.toList) {
          id =>
            val defOld = prev.defs.meta.nodes(id)
            val defNew = last.defs.meta.nodes(id)

            (defOld, defNew) match {
              case (uold: DomainMember.User, unew: DomainMember.User) =>
                diff(changes, uold.defn, unew.defn).map(diff => (id, diff))

              case (o, n) =>
                F.fail(BaboonIssue.of(EvolutionIssue.IncomparableTypedefs(o, n)))
            }
        }
        // Compute diffs for renamed types (old ID -> new definition)
        renamedDiffs <- F.traverseAccumErrors(validRenames.toList) {
          case (newId, oldId) =>
            val defOld = prev.defs.meta.nodes(oldId)
            val defNew = last.defs.meta.nodes(newId)

            (defOld, defNew) match {
              case (uold: DomainMember.User, unew: DomainMember.User) =>
                diff(changes, uold.defn, unew.defn).map(diff => (oldId, diff))

              case (o, n) =>
                F.fail(BaboonIssue.of(EvolutionIssue.IncomparableTypedefs(o, n)))
            }
        }
        allDiffs      = keptDiffs ++ renamedDiffs
        indexedDiffs <- F.fromEither(allDiffs.toUniqueMap(e => BaboonIssue.of(EvolutionIssue.NonUniqueDiffs(e))))
      } yield {
        BaboonDiff(
          EvolutionStep(prev.version, last.version),
          changes,
          indexedDiffs,
        )
      }
    }

    private def diff(
      changes: BaboonChanges,
      prevDef: Typedef.User,
      nextDef: Typedef.User,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      (prevDef, nextDef) match {
        case (e1: Typedef.Enum, e2: Typedef.Enum) =>
          diffEnums(e1, e2)
        case (a1: Typedef.Adt, a2: Typedef.Adt) =>
          diffAdts(changes, a1, a2)
        case (d1: Typedef.Dto, d2: Typedef.Dto) =>
          diffDtos(changes, d1, d2)
        case (o1, o2) =>
          F.fail(BaboonIssue.of(EvolutionIssue.MismatchingTypedefs(o1, o2)))
      }
    }

    private def diffEnums(
      e1: Typedef.Enum,
      e2: Typedef.Enum,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      val members1 = e1.members.map(m => (m.name, m)).toMap
      val members2 = e2.members.map(m => (m.name, m)).toMap

      val names1 = members1.keySet
      val names2 = members2.keySet
      val invalidRenames = e2.members.toList.flatMap {
        newMember =>
          newMember.prevName.flatMap {
            prevName =>
              if (!members1.contains(prevName)) {
                Some(EvolutionIssue.InvalidEnumMemberRename(e2.id, newMember.name, prevName))
              } else {
                None
              }
          }.toList
      }

      for {
        _ <- F.traverseAccumErrors(invalidRenames)(issue => F.fail(BaboonIssue.of(issue)))
      } yield {
        val renamedMembers = e2.members.toList.flatMap {
          newMember =>
            newMember.prevName.flatMap {
              prevName =>
                members1.get(prevName).map(oldMember => (newMember.name, (oldMember, newMember)))
            }.toList
        }.toMap

        val renamedNewNames = renamedMembers.keySet
        val renamedOldNames = renamedMembers.values.map(_._1.name).toSet

        val removedMembers = names1.diff(names2).diff(renamedOldNames)
        val addedMembers   = names2.diff(names1).diff(renamedNewNames)
        val keptMembers    = names1.intersect(names2)

        val ops = List(
          removedMembers.map(id => EnumOp.RemoveBranch(members1(id))),
          addedMembers.map(id => EnumOp.AddBranch(members2(id))),
          keptMembers.map(id => EnumOp.KeepBranch(members2(id))),
          renamedMembers.values.map { case (_, newMember) => EnumOp.KeepBranch(newMember) },
        ).flatten

        TypedefDiff.EnumDiff(ops)
      }
    }

    private def diffAdts(
      changes: BaboonChanges,
      a1: Typedef.Adt,
      a2: Typedef.Adt,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      // Check if this is a renamed ADT comparison (old ADT id is in renamed values)
      val isRenamed = changes.renamed.values.toSet.contains(a1.id)
      val branchRenames = changes.renamed.collect {
        case (newId, oldId) if a2.members.contains(newId) && a1.members.contains(oldId) =>
          (newId, oldId)
      }

      if (isRenamed) {
        // For renamed ADTs, compare branches by name since TypeIds will differ
        val members1ByName = a1.members.map(m => (m.name.name, m)).toMap
        val members2ByName = a2.members.map(m => (m.name.name, m)).toMap
        val renamedByName  = branchRenames.map { case (newId, oldId) => (oldId.name.name, newId.name.name) }

        val names1       = members1ByName.keySet
        val names2       = members2ByName.keySet
        val removedNames = names1.diff(names2).diff(renamedByName.keySet)
        val addedNames   = names2.diff(names1).diff(renamedByName.values.toSet)
        val keptNames    = names1.intersect(names2)

        val keptMembers = keptNames.map {
          name =>
            val oldRef = members1ByName(name)
            val newRef = members2ByName(name)
            // Check modification status based on the new branch ref
            val modification = figureOutModification(changes, Set(newRef))
            AdtOp.KeepBranch(newRef, modification)
        }
        val renamedMembers = renamedByName.map {
          case (_, newName) =>
            val newRef       = members2ByName(newName)
            val modification = figureOutModification(changes, Set(newRef))
            AdtOp.KeepBranch(newRef, modification)
        }

        val ops = List(
          removedNames.map(name => AdtOp.RemoveBranch(members1ByName(name))),
          addedNames.map(name => AdtOp.AddBranch(members2ByName(name))),
          keptMembers,
          renamedMembers,
        ).flatten

        F.pure(TypedefDiff.AdtDiff(ops))
      } else {
        // Non-renamed ADTs: compare by TypeId as before
        val members1 = a1.members.toSet
        val members2 = a2.members.toSet

        val renamedOld     = branchRenames.values.toSet
        val renamedNew     = branchRenames.keySet
        val removedMembers = members1.diff(members2).diff(renamedOld)
        val addedMembers   = members2.diff(members1).diff(renamedNew)
        val keptMembers = members1.intersect(members2).map {
          ref =>
            val modification =
              figureOutModification(changes, Set(ref))

            AdtOp.KeepBranch(ref, modification)
        }
        val renamedMembers = branchRenames.map {
          case (newId, _) =>
            val modification = figureOutModification(changes, Set(newId))
            AdtOp.KeepBranch(newId, modification)
        }

        val ops = List(
          removedMembers.map(id => AdtOp.RemoveBranch(id)),
          addedMembers.map(id => AdtOp.AddBranch(id)),
          keptMembers,
          renamedMembers,
        ).flatten

        F.pure(TypedefDiff.AdtDiff(ops))
      }
    }

    private def diffDtos(
      changes: BaboonChanges,
      d1: Typedef.Dto,
      d2: Typedef.Dto,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      val members1 = d1.fields.map(m => (m.name, m)).toMap
      val members2 = d2.fields.map(m => (m.name, m)).toMap

      val names1 = members1.keySet
      val names2 = members2.keySet

      val invalidRenames = d2.fields.flatMap {
        newField =>
          newField.prevName.flatMap {
            prevName =>
              if (!members1.contains(prevName)) {
                Some(EvolutionIssue.InvalidFieldRename(d2.id, newField.name, prevName))
              } else {
                None
              }
          }
      }

      for {
        _ <- F.traverseAccumErrors(invalidRenames)(issue => F.fail(BaboonIssue.of(issue)))
      } yield {
        val renamedFields: Map[FieldName, (Field, Field)] = d2.fields.flatMap {
          newField =>
            newField.prevName.flatMap {
              prevName =>
                members1.get(prevName).map(oldField => (newField.name, (oldField, newField)))
            }
        }.toMap

        val renamedNewNames = renamedFields.keySet
        val renamedOldNames = renamedFields.values.map(_._1.name).toSet

        val removedMembers = names1.diff(names2).diff(renamedOldNames)
        val addedMembers   = names2.diff(names1).diff(renamedNewNames)

        val keptMembers = names1.intersect(names2)

        val keptFields = keptMembers.map(name => (members1(name), members2(name)))
        val changedFields = keptFields.filter {
          case (f1, f2) =>
            f1.tpe != f2.tpe
        }
        val unchangedFields = keptFields.filter {
          case (f1, f2) =>
            f1.tpe == f2.tpe
        }.map {
          case (_, f2) =>
            val directRefs = enquiries.explode(f2.tpe)
            val modification =
              figureOutModification(changes, directRefs)

            DtoOp.KeepField(f2, modification)
        }

        val renamedFieldOps = renamedFields.values.map {
          case (oldField, newField) =>
            val directRefs   = enquiries.explode(newField.tpe)
            val modification = figureOutModification(changes, directRefs)
            DtoOp.RenameField(oldField, newField, modification)
        }

        val ops = List(
          removedMembers.map(id => DtoOp.RemoveField(members1(id))),
          addedMembers.map(id => DtoOp.AddField(members2(id))),
          changedFields.map(id => DtoOp.ChangeField(id._1, id._2.tpe)),
          unchangedFields,
          renamedFieldOps,
        ).flatten

        TypedefDiff.DtoDiff(ops)
      }
    }

    private def figureOutModification(
      changes: BaboonChanges,
      directRefs: Set[TypeId],
    ): RefModification = {
      if (directRefs.exists(id => changes.changed.contains(id))) {
        if (directRefs.exists(id => changes.fullyModified.contains(id))) {
          RefModification.Full
        } else if (directRefs.exists(id => changes.shallowModified.contains(id))) {
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

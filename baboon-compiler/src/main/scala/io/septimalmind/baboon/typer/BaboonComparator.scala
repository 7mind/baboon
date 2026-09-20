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

  def compare(
    last: Domain,
    prev: Domain,
  ): F[NEList[BaboonIssue], BaboonDiff]
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
        _ <- validateRenameAncestry(versions)
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
        forwardReadable = computeForwardReadable(versions, sortedVersions.reverse)
      } yield {
        BaboonEvolution(pkg, pinnacleVersion, diffMap, rulesetMap, minVersions, forwardReadable)
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

    /** Forward-readability ranges (see docs/drafts/20260911-0937-forward-compat-metadata.md).
      *
      * For each version V and each type present in V, computes the ascending
      * contiguous run of versions W >= V whose encoded data the V-version codec
      * can decode, with the strongest guarantee tier per W. The chain tier is
      * the minimum of the per-step tiers (composition argument in the doc).
      */
    private def computeForwardReadable(
      domainVersions: NEMap[Version, Domain],
      ascendingVersions: List[Version],
    ): Map[Version, Map[TypeId, ForwardReadable]] = {
      val steps: Map[Version, Map[TypeId, ForwardGuarantee]] =
        ascendingVersions
          .sliding(2)
          .collect {
            case from :: to :: Nil =>
              (from, stepForwardGuarantees(domainVersions(from), domainVersions(to)))
          }
          .toMap

      ascendingVersions.reverse
        .foldLeft(Map.empty[Version, Map[TypeId, ForwardReadable]]) {
          case (acc, version) =>
            val successor = steps.get(version).flatMap {
              stepTiers =>
                // the version right above `version`, already computed (newest-first fold)
                ascendingVersions.dropWhile(_ != version).drop(1).headOption.map(next => (stepTiers, acc(next)))
            }

            val entries = domainVersions(version).defs.meta.nodes.map {
              case (id, _) =>
                val tail = successor match {
                  case Some((stepTiers, nextRuns)) =>
                    (stepTiers.get(id), nextRuns.get(id)) match {
                      case (Some(stepGuarantee), Some(nextRun)) =>
                        nextRun.readable.toList
                          .map { case (v, g) => (v, g.meet(stepGuarantee)) }
                          .takeWhile(_._2.nonEmpty)
                      case _ =>
                        List.empty
                    }
                  case None =>
                    List.empty
                }
                (id, ForwardReadable(id, version, NEList.unsafeFrom((version, ForwardGuarantee.identical) :: tail)))
            }

            acc.updated(version, entries)
        }
    }

    /** Per-step (prev -> last) forward-readability for every type kept under the same
      * TypeId in both versions. Absent key = not forward-readable in either format.
      * Combines the type's own structural guarantee with a fixpoint over its
      * codec-relevant dependencies in the OLD version (the types the old decoder
      * actually touches). The two axes propagate independently: the outer UEBA read
      * stays in sync only while every nested value is UEBA byte-identical, and the
      * outer JSON read survives only while every nested value is JSON-readable.
      */
    private def stepForwardGuarantees(prev: Domain, last: Domain): Map[TypeId, ForwardGuarantee] = {
      val kept = prev.defs.meta.nodes.keySet.intersect(last.defs.meta.nodes.keySet)

      val own: Map[TypeId, ForwardGuarantee] = kept.map {
        id =>
          val guarantee = (prev.defs.meta.nodes(id), last.defs.meta.nodes(id)) match {
            case (_: DomainMember.Builtin, _: DomainMember.Builtin) =>
              ForwardGuarantee.identical
            case (o: DomainMember.User, n: DomainMember.User) =>
              ownForwardGuarantee(o.defn, n.defn, prev, last)
            case _ =>
              ForwardGuarantee.none
          }
          (id, guarantee)
      }.toMap

      // codec-relevant direct dependencies in the OLD version: what the old decoder touches
      val deps: Map[TypeId, Set[TypeId]] = kept.map {
        id =>
          val d = prev.defs.meta.nodes(id) match {
            case _: DomainMember.Builtin => Set.empty[TypeId]
            case u: DomainMember.User =>
              u.defn match {
                case d: Typedef.Dto      => d.fields.flatMap(f => enquiries.explode(f.tpe)).toSet
                case a: Typedef.Adt      => a.dataMembers(prev).toSet[TypeId]
                case _: Typedef.Enum     => Set.empty[TypeId]
                case _: Typedef.Foreign  => Set.empty[TypeId]
                case _: Typedef.Contract => Set.empty[TypeId]
                case _: Typedef.Service  => Set.empty[TypeId]
              }
          }
          (id, d)
      }.toMap

      // monotone-descending fixpoint on a finite lattice: terminates
      var current = own
      var changed = true
      while (changed) {
        changed = false
        current = current.map {
          case (id, guarantee) =>
            val depGuarantees = deps(id).toList.map(dep => current.getOrElse(dep, ForwardGuarantee.none))
            val next = ForwardGuarantee(
              // a nested value that merely prefix-reads would leave the outer cursor
              // mid-value; only byte-identical nesting keeps the outer read in sync
              ueba = if (depGuarantees.forall(_.ueba.contains(UebaRead.Full))) guarantee.ueba else None,
              json = guarantee.json && depGuarantees.forall(_.json),
            )
            if (next != guarantee) changed = true
            (id, next)
        }
      }

      current.filter(_._2.nonEmpty)
    }

    /** The type's OWN structural forward-readability for one step, ignoring
      * dependencies, resolved separately for the two wire formats.
      *
      * UEBA identifies members by position and never puts a name on the wire, so a
      * rename declared with `was` is invisible to it. JSON identifies members by
      * name and is blind to position, so it forgives exactly the opposite set of
      * changes. Neither subsumes the other.
      *
      * Renames of a TYPE (rather than of a field or an enum member) are not handled
      * here: they change the TypeId, so the renamed type is absent from the
      * intersection the caller iterates and from its dependents' dependency sets,
      * which collapses both axes. That needs rename-aware dependency resolution.
      */
    private def ownForwardGuarantee(o: Typedef.User, n: Typedef.User, prev: Domain, last: Domain): ForwardGuarantee = {
      (o, n) match {
        case (d1: Typedef.Dto, d2: Typedef.Dto) =>
          // `prevName` is carried forward by every later version, so it only denotes a
          // rename performed by THIS step while it still names a field of the old type.
          val oldFieldNames = d1.fields.map(_.name).toSet
          def renamedAtThisStep(nf: Field): Option[FieldName] =
            nf.prevName.filter(prev => prev != nf.name && oldFieldNames.contains(prev))

          // JSON: every old key must still carry that same old field's value. A new
          // field that took the name over from a different field via `was` would hand
          // the old reader the wrong value, so it does not count.
          val json = d1.fields.forall {
            of => d2.fields.exists(nf => nf.name == of.name && nf.tpe == of.tpe && renamedAtThisStep(nf).isEmpty)
          }

          // UEBA: the position is the identity. A name change at a position is
          // invisible on the wire, and sound exactly when it is a declared rename of
          // the field that occupied that position.
          val prefixAligned = d2.fields.size >= d1.fields.size && d1.fields.zip(d2.fields).forall {
            case (of, nf) => of.tpe == nf.tpe && (of.name == nf.name || renamedAtThisStep(nf).contains(of.name))
          }

          val ueba = if (!prefixAligned) {
            None
          } else if (d2.fields.size == d1.fields.size) {
            Some(UebaRead.Full)
          } else {
            val appended = d2.fields.drop(d1.fields.size)
            val allFixed = appended.forall {
              f =>
                last.refMeta(f.tpe).len match {
                  case _: BinReprLen.Fixed => true
                  case _                   => false
                }
            }
            Some(if (allFixed) UebaRead.PrefixAnyMode else UebaRead.PrefixCompact)
          }

          ForwardGuarantee(ueba, json)

        case (e1: Typedef.Enum, e2: Typedef.Enum) =>
          val m1 = e1.members.toList
          val m2 = e2.members.toList

          // As for fields, `prevName` survives into later versions and only denotes a
          // rename performed by THIS step.
          val oldMemberNames = m1.map(_.name).toSet
          def renamedAtThisStep(m: EnumMember): Option[String] =
            m.prevName.filter(prev => prev != m.name && oldMemberNames.contains(prev))

          // JSON encodes the member name. An added member is unreadable once it is
          // actually sent, so the name sets must match exactly; a member that has
          // taken over another member's name would decode to the wrong constant.
          val json = m1.map(_.name).toSet == m2.map(_.name).toSet &&
            m2.forall(m => renamedAtThisStep(m).isEmpty)

          // UEBA encodes the positional index, so a rename in place is invisible.
          // Explicit `const` values never reach the wire, but they stay in the
          // comparison so a const-only change keeps its existing classification.
          val uebaFull = m1.size == m2.size && m1.zip(m2).forall {
            case (a, b) => a.const == b.const && (a.name == b.name || renamedAtThisStep(b).contains(a.name))
          }

          ForwardGuarantee(Option.when(uebaFull)(UebaRead.Full), json)

        case (a1: Typedef.Adt, a2: Typedef.Adt) =>
          val b1 = a1.dataMembers(prev)
          val b2 = a2.dataMembers(last)

          // JSON tags a branch by its name: the new writer must only emit branches
          // the old reader already knows.
          val json = b2.toSet.subsetOf(b1.toSet)
          // UEBA encodes the positional branch index. A branch rename is a type
          // rename, excluded above.
          ForwardGuarantee(Option.when(b1 == b2)(UebaRead.Full), json)

        case (f1: Typedef.Foreign, f2: Typedef.Foreign) =>
          // hand-written codecs: only byte-level sameness is derivable
          if (f1 == f2) ForwardGuarantee.identical else ForwardGuarantee.none

        case _ =>
          // Contract/Service carry no codecs; kind changes are unreadable
          ForwardGuarantee.none
      }
    }

    /**
      * True when the new version of `id` declares a `was` rename that still names a field or enum
      * member of the previous version. Such a rename is honoured by the per-pair diffs, so the type
      * must not be reported as structurally unchanged.
      */
    private def hasEffectiveRename(last: Domain, prev: Domain, id: TypeId): Boolean = {
      (last.defs.meta.nodes.get(id), prev.defs.meta.nodes.get(id)) match {
        case (Some(DomainMember.User(_, nd: Typedef.Dto, _, _)), Some(DomainMember.User(_, od: Typedef.Dto, _, _))) =>
          val oldNames = od.fields.map(_.name).toSet
          nd.fields.exists(f => f.prevName.exists(was => was != f.name && oldNames.contains(was)))
        case (Some(DomainMember.User(_, ne: Typedef.Enum, _, _)), Some(DomainMember.User(_, oe: Typedef.Enum, _, _))) =>
          val oldNames = oe.members.map(_.name).toSet
          ne.members.exists(m => m.prevName.exists(was => was != m.name && oldNames.contains(was)))
        case _ =>
          false
      }
    }

    private def renameableNames(domain: Domain, id: TypeId.User): Set[String] = {
      domain.defs.meta.nodes.get(id) match {
        case Some(DomainMember.User(_, dto: Typedef.Dto, _, _))  => dto.fields.map(_.name.name).toSet
        case Some(DomainMember.User(_, enm: Typedef.Enum, _, _)) => enm.members.map(_.name).toSet
        case _                                                   => Set.empty
      }
    }

    /**
      * A `was` annotation survives into every later version of a type, so an individual version pair
      * cannot tell a carried-forward annotation apart from a typo: at the 1.1.0 -> 1.2.0 step the
      * field renamed in 1.1.0 still says `was b` while 1.1.0 no longer has a `b`. Ancestry is
      * therefore validated here, where every version of the package is visible, and the per-pair
      * diffs honour a `prevName` only while it still names a member of the version they compare
      * against.
      */
    private def validateRenameAncestry(
      versions: NEMap[Version, Domain]
    ): F[NEList[BaboonIssue], Unit] = {
      val ascending = versions.keySet.toList.sorted(Version.ordering)
      // names carried by a type's lineage in all versions strictly preceding the keyed one
      val history = mutable.HashMap.empty[(Version, TypeId.User), Set[String]]

      val issues = ascending.zipWithIndex.flatMap {
        case (version, idx) =>
          val domain = versions(version)

          domain.defs.meta.nodes.values.toList.flatMap {
            case DomainMember.User(_, defn: Typedef.User, _, _) =>
              val ancestralNames = if (idx == 0) {
                Set.empty[String]
              } else {
                val prevVersion = ascending(idx - 1)
                val prevId      = domain.renames.getOrElse(defn.id, defn.id)
                history.getOrElse((prevVersion, prevId), Set.empty[String]) ++
                renameableNames(versions(prevVersion), prevId)
              }
              history.put((version, defn.id), ancestralNames)

              if (ancestralNames.isEmpty) {
                // the type has no ancestor to have been renamed from; no pair diff looks at it either
                List.empty
              } else {
                defn match {
                  case dto: Typedef.Dto =>
                    dto.fields.flatMap {
                      field =>
                        field.prevName
                          .filterNot(prev => ancestralNames.contains(prev.name))
                          .map(prev => EvolutionIssue.InvalidFieldRename(dto.id, field.name, prev))
                    }
                  case enm: Typedef.Enum =>
                    enm.members.toList.flatMap {
                      member =>
                        member.prevName
                          .filterNot(ancestralNames.contains)
                          .map(prev => EvolutionIssue.InvalidEnumMemberRename(enm.id, member.name, prev))
                    }
                  case _ => List.empty
                }
              }
            case _ => List.empty
          }
      }

      F.traverseAccumErrors(issues)(issue => F.fail(BaboonIssue.of(issue))).map(_ => ())
    }

    override def compare(
      last: Domain,
      prev: Domain,
    ): F[NEList[BaboonIssue], BaboonDiff] = {
      val newTypes = last.defs.meta.nodes.keySet
      val oldTypes = prev.defs.meta.nodes.keySet

      // Identify valid renames: new type has was[] pointing to an existing old type
      val explicitRenames: Map[TypeId.User, TypeId.User] = last.renames.filter {
        case (newId, oldId) =>
          newTypes.contains(newId) && oldTypes.contains(oldId) && !newTypes.contains(oldId)
      }
      val explicitSources = explicitRenames.values.toSet
      // Moving an ADT changes its branches' owner IDs even when their names stay the same.
      val branchRenames = explicitRenames.toList.flatMap {
        case (newId, oldId) =>
          (last.defs.meta.nodes(newId), prev.defs.meta.nodes(oldId)) match {
            case (DomainMember.User(_, current: Typedef.Adt, _, _), DomainMember.User(_, previous: Typedef.Adt, _, _)) =>
              val oldBranches = previous.dataMembers(prev).map(id => id.name -> id).toMap
              current.dataMembers(last).flatMap {
                newBranch =>
                  oldBranches
                    .get(newBranch.name).filter {
                      oldBranch => !explicitRenames.contains(newBranch) && !explicitSources.contains(oldBranch) && !newTypes.contains(oldBranch)
                    }.map(oldBranch => newBranch -> oldBranch)
              }
            case _ => Nil
          }
      }.toMap
      val validRenames  = explicitRenames ++ branchRenames
      val renamedNewIds = validRenames.keySet.asInstanceOf[Set[TypeId]]
      val renamedOldIds = validRenames.values.toSet.asInstanceOf[Set[TypeId]]

      val kept    = newTypes.intersect(oldTypes)
      val added   = newTypes.diff(oldTypes).diff(renamedNewIds)
      val removed = oldTypes.diff(newTypes).diff(renamedOldIds)

      // A `was` rename that still names a member of the previous version is a real change even when
      // both structural signatures stay put: `shallowId` sorts `name:type` and `deepId` is
      // positional, so a declared name swap is invisible to both. Such a type must be classified as
      // locally modified so that it reaches `diffDtos`/`diffEnums` and the rename becomes a
      // conversion op instead of a positional transfer.
      val declaredRenames = kept.filter(id => hasEffectiveRename(last, prev, id))

      val unmodified = kept.filter {
        id =>
          last.typeMeta(id).shallowId == prev.typeMeta(id).shallowId &&
          last.typeMeta(id).deepId == prev.typeMeta(id).deepId
      }.diff(declaredRenames)

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
          last.typeMeta(id).shallowId != prev.typeMeta(id).shallowId ||
          declaredRenames.contains(id)
      }

      // same local structure, different dependencies
      val deepModified = partiallyModified.diff(shallowModified)

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
        // §5.1 (M18 identifiers plan): both `data` and `id` types are Typedef.Dto.
        // diffDtos compares fields only — isIdentifier is deliberately
        // ignored so that a data→id or id→data shape-preserving change is classified
        // as Unchanged by the comparator (wire formats are byte-identical).
        case (d1: Typedef.Dto, d2: Typedef.Dto) =>
          diffDtos(changes, d1, d2)
        case (s1: Typedef.Service, s2: Typedef.Service) =>
          diffServices(s1, s2)
        case (c1: Typedef.Contract, c2: Typedef.Contract) =>
          diffContracts(c1, c2)
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

      // `prevName` is carried forward by every later version of a type, so here it denotes a rename
      // only while it still names a member of the version we compare against. Ancestry of a stale
      // annotation is validated once per package in `evolve`.
      val renamedMembers = e2.members.toList.flatMap {
        newMember =>
          newMember.prevName.flatMap {
            prevName =>
              members1.get(prevName).map(oldMember => (newMember.name, (oldMember, newMember)))
          }.toList
      }.toMap

      val renamedNewNames = renamedMembers.keySet
      val renamedOldNames = renamedMembers.values.map(_._1.name).toSet

      // A renamed member is accounted for by its rename op alone: a declared name swap or a rename
      // onto a name the previous version also used would otherwise be both "kept" and "renamed".
      val survivors1 = names1.diff(renamedOldNames)
      val survivors2 = names2.diff(renamedNewNames)

      val removedMembers = survivors1.diff(survivors2)
      val addedMembers   = survivors2.diff(survivors1)
      val keptMembers    = survivors1.intersect(survivors2)

      val ops = List(
        removedMembers.map(id => EnumOp.RemoveBranch(members1(id))),
        addedMembers.map(id => EnumOp.AddBranch(members2(id))),
        keptMembers.map(id => EnumOp.KeepBranch(members2(id))),
        renamedMembers.values.map { case (_, newMember) => EnumOp.KeepBranch(newMember) },
      ).flatten

      F.pure(TypedefDiff.EnumDiff(ops))
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
        val renamedByName = branchRenames.collect {
          case (newId, oldId) if newId.name != oldId.name => (oldId.name.name, newId.name.name)
        }

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

      // `prevName` is carried forward by every later version of a type, so here it denotes a rename
      // only while it still names a field of the version we compare against. Ancestry of a stale
      // annotation is validated once per package in `evolve`.
      val renamedFields: Map[FieldName, (Field, Field)] = d2.fields.flatMap {
        newField =>
          newField.prevName.flatMap {
            prevName =>
              members1.get(prevName).map(oldField => (newField.name, (oldField, newField)))
          }
      }.toMap

      val renamedNewNames = renamedFields.keySet
      val renamedOldNames = renamedFields.values.map(_._1.name).toSet

      // A renamed field is accounted for by its rename op alone: a declared name swap or a rename
      // onto a name the previous version also used would otherwise be both "kept" and "renamed",
      // producing two conflicting ops for one target field.
      val survivors1 = names1.diff(renamedOldNames)
      val survivors2 = names2.diff(renamedNewNames)

      val removedMembers = survivors1.diff(survivors2)
      val addedMembers   = survivors2.diff(survivors1)
      val keptMembers    = survivors1.intersect(survivors2)

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

      F.pure(TypedefDiff.DtoDiff(ops))
    }

    private def diffServices(
      s1: Typedef.Service,
      s2: Typedef.Service,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      val methods1 = s1.methods.map(m => (m.name, m)).toMap
      val methods2 = s2.methods.map(m => (m.name, m)).toMap

      val names1 = methods1.keySet
      val names2 = methods2.keySet

      val removedMethods = names1.diff(names2)
      val addedMethods   = names2.diff(names1)
      val keptMethods    = names1.intersect(names2)

      val ops = List(
        removedMethods.map(name => ServiceOp.RemoveMethod(methods1(name))),
        addedMethods.map(name => ServiceOp.AddMethod(methods2(name))),
        keptMethods.map(name => ServiceOp.KeepMethod(methods2(name))),
      ).flatten

      F.pure(TypedefDiff.ServiceDiff(ops))
    }

    private def diffContracts(
      c1: Typedef.Contract,
      c2: Typedef.Contract,
    ): F[NEList[BaboonIssue], TypedefDiff] = {
      val fields1 = c1.fields.map(f => (f.name, f)).toMap
      val fields2 = c2.fields.map(f => (f.name, f)).toMap

      val fieldNames1 = fields1.keySet
      val fieldNames2 = fields2.keySet

      val removedFields = fieldNames1.diff(fieldNames2)
      val addedFields   = fieldNames2.diff(fieldNames1)
      val keptFields    = fieldNames1.intersect(fieldNames2)

      val contracts1 = c1.contracts.toSet
      val contracts2 = c2.contracts.toSet

      val removedContracts = contracts1.diff(contracts2)
      val addedContracts   = contracts2.diff(contracts1)
      val keptContracts    = contracts1.intersect(contracts2)

      val ops = List(
        removedFields.map(name => ContractOp.RemoveField(fields1(name))),
        addedFields.map(name => ContractOp.AddField(fields2(name))),
        keptFields.map(name => ContractOp.KeepField(fields2(name))),
        removedContracts.map(id => ContractOp.RemoveContract(id)),
        addedContracts.map(id => ContractOp.AddContract(id)),
        keptContracts.map(id => ContractOp.KeepContract(id)),
      ).flatten

      F.pure(TypedefDiff.ContractDiff(ops))
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

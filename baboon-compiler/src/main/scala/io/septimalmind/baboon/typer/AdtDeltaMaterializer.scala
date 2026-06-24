package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.issues.TyperIssue.AdtDeltaConflictDetail
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeName}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** G27 (T162): the PURE-SUGAR keep/drop materialization pre-pass.
  *
  * Runs at the RAW/family seam (driven by `BaboonFamilyManager.resolveImports`, in toposort order so
  * the immediately-prior version is already fully materialized) BEFORE per-domain typing and well
  * before `comparator.evolve`. For each `RawAdt` whose member list carries `RawAdtMember.Keep` /
  * `RawAdtMember.Drop` arms (a "delta body"), it rewrites the ADT into an equivalent `RawAdt` whose
  * member list is a fully materialized `RawAdtMemberDto` / `RawAdtMemberContract` branch list — with
  * NO Keep/Drop arms surviving. After this pass the ADT is byte-equivalent to an explicit whole-ADT
  * re-declaration, so `TypedefDiff` / comparator / the 9 backends / codecs need ZERO change (the
  * `convertAdt` Keep/Drop guard never fires).
  *
  * The pass also DETECTS the conflict conditions and RAISES the `AdtDeltaConflict` TyperIssue
  * (defined + wired in T163) through the F/Either issue channel — no raw exceptions.
  *
  * Lowering algorithm (deterministic, per the T162 spec):
  *   1. Start set = prior `Foo`'s branches: `keep *` → ALL prior branches; `keep A, B` → only those
  *      named (in keep-list order).
  *   2. Apply `drop X`: remove branch X from the start set.
  *   3. Apply in-body `data Y {…}`: a redefined Y REPLACES the kept Y in place; a new Y is appended.
  *   4. Emit the resulting branch members in deterministic order (kept/prior order first, then
  *      in-body new branches in source order). Kept-unchanged branches carry over the prior version's
  *      branch member verbatim (preserving meta/docs) so the emitted output matches the explicit twin.
  */
class AdtDeltaMaterializer[F[+_, +_]: Error2] {

  /** True iff this ADT carries any Keep/Drop arm (i.e. it is a delta body). */
  private def isDeltaBody(adt: RawAdt): Boolean =
    adt.members.exists {
      case _: RawAdtMember.Keep => true
      case _: RawAdtMember.Drop => true
      case _                    => false
    }

  /** Materialize every delta-body ADT in `current`'s member list, descending into namespaces.
    * `priorMembersOpt` is the immediately-prior version's already-materialized member list (resolved
    * via THIS domain's `import "<old>" { * }` header), or None when no import header is in scope.
    */
  def materialize(
    pkg: Pkg,
    currentMembers: Seq[RawTLDef],
    priorMembersOpt: Option[Seq[RawTLDef]],
  ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
    rewriteMembers(pkg, currentMembers, priorMembersOpt, Owner.Toplevel, List.empty)
  }

  private def rewriteMembers(
    pkg: Pkg,
    members: Seq[RawTLDef],
    priorMembersOpt: Option[Seq[RawTLDef]],
    owner: Owner,
    nsPath: List[TypeName],
  ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
    F.traverseAccumErrors(members.toList) {
      case adtTL @ RawTLDef.ADT(_, raw) if isDeltaBody(raw) =>
        val adtId = TypeId.User(pkg, owner, TypeName(raw.name.name))
        materializeSingleAdt(adtId, raw, priorMembersOpt).map(m => adtTL.copy(value = m))

      case nsTL @ RawTLDef.Namespace(ns) =>
        val nextNsPath  = nsPath :+ TypeName(ns.name.name)
        val nextOwner   = Owner.Ns(nextNsPath)
        val priorNsDefs = priorMembersOpt.map(prior => nestedNamespaceDefs(prior, ns.name.name))
        rewriteMembers(pkg, ns.defns, priorNsDefs, nextOwner, nextNsPath)
          .map(children => nsTL.copy(value = ns.copy(defns = children)))

      case other =>
        F.pure(other)
    }.map(_.toSeq)
  }

  /** Extract the defns of the prior-version namespace named `name` (or empty when absent). */
  private def nestedNamespaceDefs(priorMembers: Seq[RawTLDef], name: String): Seq[RawTLDef] = {
    priorMembers.collectFirst {
      case RawTLDef.Namespace(ns) if ns.name.name == name => ns.defns
    }.getOrElse(Seq.empty)
  }

  private def materializeSingleAdt(
    adtId: TypeId.User,
    adt: RawAdt,
    priorMembersOpt: Option[Seq[RawTLDef]],
  ): F[NEList[BaboonIssue], RawAdt] = {
    def conflict(detail: AdtDeltaConflictDetail, meta: RawNodeMeta): NEList[BaboonIssue] =
      BaboonIssue.of(TyperIssue.AdtDeltaConflict(adtId, detail, meta))

    // The import header legality precondition: a delta body with no `import "<old>" { * }` in scope.
    priorMembersOpt match {
      case None =>
        F.fail(conflict(AdtDeltaConflictDetail.MissingImportHeader, adt.meta))

      case Some(priorMembers) =>
        val keeps: Seq[RawAdtMember.Keep] = adt.members.collect { case k: RawAdtMember.Keep => k }
        val drops: Seq[RawAdtMember.Drop] = adt.members.collect { case d: RawAdtMember.Drop => d }

        // In-body redefinitions / new branches: the literal DTO/Contract branch members.
        val inBodyBranches: Seq[RawAdtMember] = adt.members.collect {
          case d: RawAdtMemberDto      => d
          case c: RawAdtMemberContract => c
        }
        // Non-keep/drop, non-branch arms (structural +/-/^) pass through untouched for the
        // current-domain expander. They never co-occur with keep/drop in a well-formed delta body,
        // but we never silently drop them.
        val passthroughArms: Seq[RawAdtMember] = adt.members.collect {
          case i: RawAdtMember.Include    => i
          case e: RawAdtMember.Exclude    => e
          case it: RawAdtMember.Intersect => it
        }

        // Prior same-named ADT's branch members, in prior declaration order.
        val priorBranches: Seq[RawAdtMember] = priorMembers.collectFirst {
          case RawTLDef.ADT(_, priorAdt) if priorAdt.name.name == adt.name.name =>
            priorAdt.members.collect {
              case d: RawAdtMemberDto      => d: RawAdtMember
              case c: RawAdtMemberContract => c: RawAdtMember
            }
        }.getOrElse(Seq.empty)

        val priorByName: Map[String, RawAdtMember] =
          priorBranches.map(b => branchName(b) -> b).toMap
        val priorOrder: Seq[String] = priorBranches.map(branchName)

        val inBodyNames: Set[String] = inBodyBranches.map(branchName).toSet

        // keep * (wildcard) vs selective keep A, B.
        val hasWildcardKeep: Boolean = keeps.exists(_.branches.isEmpty)
        val selectiveKeepNames: Seq[(RawTypeName, RawNodeMeta)] =
          keeps.flatMap(k => k.branches.toList.flatMap(_.toList).map(n => (n, k.meta)))

        for {
          // CONFLICT: selective keep names a branch absent from prior (KeepOfAbsent).
          _ <- F.traverseAccumErrors_(selectiveKeepNames.toList) {
            case (name, meta) =>
              F.when(!priorByName.contains(name.name)) {
                F.fail(conflict(AdtDeltaConflictDetail.KeepOfAbsent(name), meta))
              }
          }
          // CONFLICT: drop X where X absent from prior (DropOfAbsent).
          _ <- F.traverseAccumErrors_(drops.toList) { d =>
            F.when(!priorByName.contains(d.branch.name)) {
              F.fail(conflict(AdtDeltaConflictDetail.DropOfAbsent(d.branch), d.meta))
            }
          }
          // CONFLICT: a branch named by both keep (selective) and drop (KeepDropSame).
          dropNames = drops.map(_.branch.name).toSet
          _ <- F.traverseAccumErrors_(selectiveKeepNames.toList) {
            case (name, meta) =>
              F.when(dropNames.contains(name.name)) {
                F.fail(conflict(AdtDeltaConflictDetail.KeepDropSame(name), meta))
              }
          }
          // CONFLICT: selective keep A AND in-body data A (KeepRedefineSame).
          _ <- F.traverseAccumErrors_(selectiveKeepNames.toList) {
            case (name, meta) =>
              F.when(inBodyNames.contains(name.name)) {
                F.fail(conflict(AdtDeltaConflictDetail.KeepRedefineSame(name), meta))
              }
          }
          // CONFLICT: drop X AND in-body data X (DropRedefineSame).
          _ <- F.traverseAccumErrors_(drops.toList) { d =>
            F.when(inBodyNames.contains(d.branch.name)) {
              F.fail(conflict(AdtDeltaConflictDetail.DropRedefineSame(d.branch), d.meta))
            }
          }
        } yield {
          // Step 1: start set (kept branch names) in deterministic order.
          val keptNames: Seq[String] =
            if (hasWildcardKeep) priorOrder
            else selectiveKeepNames.map(_._1.name)

          // Step 2: apply drops.
          val afterDrop: Seq[String] = keptNames.filterNot(dropNames.contains)
          val keptSet                = afterDrop.toSet

          // Step 3 + 4: kept branches carry over verbatim, with an in-body redefinition (only possible
          // under `keep *`, since selective-keep + redefine was rejected above) replacing in place;
          // genuine new in-body branches are appended in source order.
          val (redefinitions, newBranches) =
            inBodyBranches.partition(b => keptSet.contains(branchName(b)))
          val redefByName = redefinitions.map(b => branchName(b) -> b).toMap

          val keptResolved: Seq[RawAdtMember] =
            afterDrop.flatMap(n => redefByName.get(n).orElse(priorByName.get(n)))

          val materializedBranches: Seq[RawAdtMember] =
            passthroughArms ++ keptResolved ++ newBranches

          adt.copy(members = materializedBranches)
        }
    }
  }

  private def branchName(m: RawAdtMember): String = m match {
    case d: RawAdtMemberDto      => d.dto.name.name
    case c: RawAdtMemberContract => c.contract.name.name
    case other                   => throw new RuntimeException(s"BUG: unexpected non-branch ADT member in materializer: $other")
  }
}

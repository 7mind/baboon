package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Scope.NestedScope
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** PR-63 (M20.2) typer-early pass: rewrites every `RawAdt` in a domain by replacing the
  * `RawAdtMember.{Include, Exclude, Intersect}` arms (introduced in PR-62) with literal
  * `RawAdtMemberDto` / `RawAdtMemberContract` entries pulled from the referenced ADTs. After this
  * pass runs, no `Include` / `Exclude` / `Intersect` arms remain — the standard
  * `BaboonTranslator.convertAdt` pipeline runs unchanged.
  *
  * Algorithm (per plan §3 + Q-FU-3):
  *   1. Process ADTs in toposort order so that when we expand `A + B`, B has already been
  *      expanded (any `B + C` chain has been flattened into B's literal branches by the time we
  *      reach A).
  *   2. For each ADT, resolve every `+ X` / `- X` / `^ X` ref via `scopeSupport.resolveScopedRef`
  *      to a `TypeId.User`. Determine whether it names an ADT (All-form: all branches included)
  *      or a single branch of an ADT (Branch-form: just that one branch). If neither, emit
  *      `WrongAdtInclusion`. If the resolved ref points to a different `Pkg` than the receiving
  *      ADT, emit `CrossVersionAdtInclusion`.
  *   3. Compute the final branch set: `local ∪ ⋃includes − ⋃excludes` and, if any intersects are
  *      present, `∩ ⋃intersects`. Set algebra is keyed by branch *name* (the last segment of the
  *      type id), because branch identity is per-receiving-ADT after re-emit (Q-M20-1).
  *   4. Reject with `DuplicatedAdtBranches` if two branches in the final set share the same name
  *      from different sources (Q-M20-2).
  *   5. Rebuild `Seq[RawTLDef]` with the rewritten ADTs in place of the originals, descending into
  *      `RawNamespace` recursively. The caller (`BaboonTyper.runTyper`) re-runs scope-building
  *      and toposort over the rewritten defns so that re-emitted branches are registered as
  *      first-class scope entries under the receiving ADT.
  */
class AdtInheritanceExpander[F[+_, +_]: Error2](
  scopeSupport: ScopeSupport[F]
) {

  /** Run the expansion. Returns a rewritten `members` list with all `RawAdtMember.{Include,
    * Exclude, Intersect}` arms desugared to literal entries. The original toposort `ordered` is
    * used to drive the expansion order; `flattened` carries the ADT-bearing scopes for ref
    * resolution.
    */
  def expand(
    pkg: Pkg,
    members: Seq[RawTLDef],
    ordered: List[NestedScope[ExtendedRawDefn]],
    meta: RawNodeMeta,
  ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {

    // Index every ADT-scope by (pkg, owner, name) so we can look up the resolved id of an
    // ADT in O(1) when expanding its branch set.
    val adtScopes: List[(NestedScope[ExtendedRawDefn], RawAdt)] = ordered.collect {
      case s if s.defn.defn.isInstanceOf[RawAdt] =>
        (s, s.defn.defn.asInstanceOf[RawAdt])
    }

    for {
      // Resolve every ADT scope to its TypeId.User up-front so we have a stable index key.
      adtIds <- F.traverseAccumErrors(adtScopes) {
        case (scope, adt) =>
          scopeSupport.resolveUserTypeId(adt.name, scope, pkg, adt.meta).map(id => (id, scope, adt))
      }

      // Process ADTs in topo order, accumulating expanded RawAdts. The accumulated map is
      // keyed by TypeId.User so that an `A + B` expansion can look up B's already-expanded
      // RawAdt (which may itself have absorbed `+ C` branches).
      expandedById <- F.foldLeft(adtIds.toList)(Map.empty[TypeId.User, RawAdt]) {
        case (acc, (id, scope, adt)) =>
          for {
            expanded <- expandSingleAdt(pkg, id, scope, adt, acc)
          } yield acc + (id -> expanded)
      }
      // Domain-level meta is currently unused (each issue carries the offending ADT/ref's own
      // meta); keep the parameter on the public `expand` API to leave a hook for future
      // domain-wide diagnostics without an API churn.
      _ = meta

      // Substitute the expanded ADTs back into the top-level RawTLDef list, descending into
      // namespaces. Each ADT is matched to its expanded form by (pkg, owner, name).
      rewrittenMembers = substituteMembers(members, expandedById, pkg, ownerForCurrent = Owner.Toplevel, nsPath = List.empty)
    } yield rewrittenMembers
  }

  /** Expand a single ADT's inheritance arms into literal `RawAdtMemberDto` / `RawAdtMemberContract`
    * entries. Returns the rewritten `RawAdt`.
    */
  private def expandSingleAdt(
    pkg: Pkg,
    receivingAdtId: TypeId.User,
    scope: NestedScope[ExtendedRawDefn],
    adt: RawAdt,
    expandedById: Map[TypeId.User, RawAdt],
  ): F[NEList[BaboonIssue], RawAdt] = {

    // Partition the receiving ADT's members. Local entries pass through unchanged; the three
    // inheritance arm kinds get re-resolved via `scopeSupport.resolveScopedRef`.
    val localMembers: Seq[RawAdtMember] = adt.members.collect {
      case d: RawAdtMemberDto      => d
      case c: RawAdtMemberContract => c
    }
    val includes:   Seq[RawAdtMember.Include]   = adt.members.collect { case i: RawAdtMember.Include   => i }
    val excludes:   Seq[RawAdtMember.Exclude]   = adt.members.collect { case e: RawAdtMember.Exclude   => e }
    val intersects: Seq[RawAdtMember.Intersect] = adt.members.collect { case i: RawAdtMember.Intersect => i }

    for {
      includeBranches   <- F.flatTraverseAccumErrors(includes)(i => resolveArm(pkg, receivingAdtId, scope, i.ref, i.meta, expandedById, "+"))
      excludeBranches   <- F.flatTraverseAccumErrors(excludes)(e => resolveArm(pkg, receivingAdtId, scope, e.ref, e.meta, expandedById, "-"))
      intersectBranches <- F.flatTraverseAccumErrors(intersects)(i => resolveArm(pkg, receivingAdtId, scope, i.ref, i.meta, expandedById, "^"))

      result <- {
        // Compute the candidate branch set. Set algebra is keyed by branch *name* — the last
        // segment of the source's type id — because re-emit gives each receiving ADT its own
        // branch identity.
        val excludeNames:   Set[String] = excludeBranches.iterator.map { p => branchName(p._1) }.toSet
        val intersectNames: Set[String] = intersectBranches.iterator.map { p => branchName(p._1) }.toSet

        // Collect all candidate branches (local + included), tagging the source for collision
        // diagnostics. Local branches use the receiving ADT's id as their source.
        val withSources: List[(RawAdtMember, TypeId)] =
          localMembers.map(m => (m, receivingAdtId: TypeId)).toList ++ includeBranches

        val afterExclude: List[(RawAdtMember, TypeId)] =
          withSources.filterNot(p => excludeNames.contains(branchName(p._1)))

        // Multiple `^` arms compose by UNION of intersect targets per plan §3 formula
        // `candidates ∩ ⋃ intersectSets`. `intersectNames` is the union of branch names across
        // ALL `^ X` arms (built via flatTraverseAccumErrors above). A branch survives if its name
        // appears in ANY referenced intersect target — NOT pairwise-intersection across arms.
        // This matches the plan §3 literal but reads counter-intuitively; intended.
        val afterIntersect: List[(RawAdtMember, TypeId)] =
          if (intersects.isEmpty) afterExclude
          else afterExclude.filter(p => intersectNames.contains(branchName(p._1)))

        // Detect duplicate branch names from different sources (Q-M20-2).
        val grouped: Map[String, List[(RawAdtMember, TypeId)]] = afterIntersect.groupBy(p => branchName(p._1))
        val duplicates: List[(String, List[TypeId])] = grouped.iterator.flatMap {
          case (name, occurrences) =>
            val srcs = occurrences.map(_._2).distinct
            if (srcs.size > 1) Iterator.single((name, srcs)) else Iterator.empty
        }.toList

        for {
          _ <- F.when(duplicates.nonEmpty) {
            val first = duplicates.head
            F.fail(BaboonIssue.of(TyperIssue.DuplicatedAdtBranches(receivingAdtId, first._1, first._2, adt.meta)))
          }
          // Empty post-intersection result is a hard error (Q-M20-4). The original source could
          // have been non-empty (`+ A; ^ B` with A having branches but B disjoint from A); the
          // empty-set comes solely from the intersection arm.
          _ <- F.when(intersects.nonEmpty && afterIntersect.isEmpty) {
            F.fail(BaboonIssue.of(TyperIssue.EmptyAdt(receivingAdtId, adt.meta)))
          }
        } yield {
          // De-duplicate by name preserving declaration order. Same-source duplicates collapse
          // silently; cross-source duplicates were caught above.
          val deduped = dedupByName(afterIntersect.map(_._1))
          adt.copy(members = deduped)
        }
      }
    } yield {
      result
    }
  }

  /** Resolve a single inheritance-arm ref to a list of branch members.
    *
    *   - All-form (`ref` names an ADT): returns all branches of that ADT (already-expanded).
    *   - Branch-form (`ref` names a branch of an ADT): returns just that one branch.
    *   - Otherwise: emits `WrongAdtInclusion`.
    *   - Cross-package: emits `CrossVersionAdtInclusion`.
    *
    * Returns pairs of (member, sourceId) so the caller can attribute branches to their origin
    * for collision diagnostics.
    */
  private def resolveArm(
    pkg: Pkg,
    receivingAdtId: TypeId.User,
    scope: NestedScope[ExtendedRawDefn],
    ref: ScopedRef,
    refMeta: RawNodeMeta,
    expandedById: Map[TypeId.User, RawAdt],
    armKind: String,
  ): F[NEList[BaboonIssue], List[(RawAdtMember, TypeId)]] = {
    // `resolveScopedRef` resolves any in-scope ScopedRef to a `TypeId.User`. PR-63 widens
    // ScopeSupport's `LookupResult` to accept SubScopes (ADTs are SubScopes, not Leafs), so
    // this works for `+ ErrorAtom` (single-segment ADT ref), `- ErrorAtom.Foo` (two-segment
    // branch ref) and `+ pkg.sub.MyAdt` (namespace-qualified ref) uniformly. The All-vs-Branch
    // decision is made downstream by inspecting the resolved id's owner.
    for {
      resolvedId <- scopeSupport.resolveScopedRef(ref, scope, pkg, refMeta)

      // Cross-version check: refs into a different package version are forbidden.
      _ <- F.when(resolvedId.pkg != pkg) {
        F.fail(BaboonIssue.of(TyperIssue.CrossVersionAdtInclusion(receivingAdtId, ref, resolvedId, refMeta)))
      }

      result <- expandedById.get(resolvedId) match {
        case Some(targetAdt) =>
          // All-form: copy every branch of the target ADT.
          F.pure(targetAdt.members.collect {
            case m: RawAdtMemberDto      => (m: RawAdtMember, resolvedId: TypeId)
            case m: RawAdtMemberContract => (m: RawAdtMember, resolvedId: TypeId)
          }.toList)

        case None =>
          // Branch-form candidate: ref's owner must be Owner.Adt(parentAdtId), and the
          // receiving ADT-by-id must have a matching branch in its expanded members.
          resolvedId.owner match {
            case Owner.Adt(parentAdtId) =>
              expandedById.get(parentAdtId) match {
                case Some(parentAdt) =>
                  val needle = resolvedId.name.name
                  parentAdt.members.collectFirst {
                    case m: RawAdtMemberDto if m.dto.name.name == needle           => (m: RawAdtMember, resolvedId: TypeId)
                    case m: RawAdtMemberContract if m.contract.name.name == needle => (m: RawAdtMember, resolvedId: TypeId)
                  } match {
                    case Some(found) => F.pure(List(found))
                    case None =>
                      F.fail(BaboonIssue.of(TyperIssue.WrongAdtInclusion(
                        receivingAdtId,
                        ref,
                        s"$armKind ${ref.path.toList.map(_.name).mkString(".")}: branch '$needle' not found in ADT ${parentAdtId.name.name}",
                        refMeta,
                      )))
                  }
                case None =>
                  F.fail(BaboonIssue.of(TyperIssue.WrongAdtInclusion(
                    receivingAdtId,
                    ref,
                    s"$armKind ${ref.path.toList.map(_.name).mkString(".")}: parent ADT ${parentAdtId.name.name} not found among expanded ADTs",
                    refMeta,
                  )))
              }

            case _ =>
              F.fail(BaboonIssue.of(TyperIssue.WrongAdtInclusion(
                receivingAdtId,
                ref,
                s"$armKind ${ref.path.toList.map(_.name).mkString(".")}: ref does not resolve to an ADT or a branch of an ADT",
                refMeta,
              )))
          }
      }
    } yield result
  }

  /** Walk the top-level RawTLDef list (and recursively into RawNamespace) replacing each RawAdt
    * with its expanded form. The rewritten list is byte-for-byte structurally identical to the
    * input except for ADT member lists.
    */
  private def substituteMembers(
    members: Seq[RawTLDef],
    expandedById: Map[TypeId.User, RawAdt],
    pkg: Pkg,
    ownerForCurrent: Owner,
    nsPath: List[TypeName],
  ): Seq[RawTLDef] = {
    members.map {
      case adtTL @ RawTLDef.ADT(_, raw) =>
        val id = TypeId.User(pkg, ownerForCurrent, TypeName(raw.name.name))
        expandedById.get(id) match {
          case Some(expanded) => adtTL.copy(value = expanded)
          case None           => adtTL
        }

      case nsTL @ RawTLDef.Namespace(ns) =>
        val nextNsPath = nsPath :+ TypeName(ns.name.name)
        val nextOwner = Owner.Ns(nextNsPath)
        val rewrittenChildren = substituteMembers(ns.defns, expandedById, pkg, nextOwner, nextNsPath)
        nsTL.copy(value = ns.copy(defns = rewrittenChildren))

      case other => other
    }
  }

  private def branchName(m: RawAdtMember): String = m match {
    case d: RawAdtMemberDto      => d.dto.name.name
    case c: RawAdtMemberContract => c.contract.name.name
    case i: RawAdtMember.Include =>
      throw new RuntimeException(s"BUG: unexpected Include arm during expansion: $i")
    case e: RawAdtMember.Exclude =>
      throw new RuntimeException(s"BUG: unexpected Exclude arm during expansion: $e")
    case it: RawAdtMember.Intersect =>
      throw new RuntimeException(s"BUG: unexpected Intersect arm during expansion: $it")
  }

  private def dedupByName(members: Seq[RawAdtMember]): Seq[RawAdtMember] = {
    val seen   = scala.collection.mutable.HashSet.empty[String]
    val result = scala.collection.mutable.ArrayBuffer.empty[RawAdtMember]
    for (m <- members) {
      val n = branchName(m)
      if (!seen.contains(n)) {
        seen += n
        result += m
      }
    }
    result.toSeq
  }
}

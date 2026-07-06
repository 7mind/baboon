package io.septimalmind.baboon.bincompat

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.*
import io.septimalmind.baboon.typer.{BaboonComparator, BaboonEnquiries, BaboonRules, TypeInfo}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.{Error2, ParallelErrorAccumulatingOps2}
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.fundamentals.collections.nonempty.NEList

/** Wire-compatibility verdict for an ordered pair of `Domain` values (FROM → TO).
  *
  * The numeric code is the process exit code a later entrypoint (T196) will
  * surface; it is part of the contract, hence pinned here.
  *
  *   - [[BincompatVerdict.NoBreak]] (0): strictly additions and/or unmodified
  *     types — no changed or removed type.
  *   - [[BincompatVerdict.BreakingDerivable]] (1): at least one changed/removed
  *     type, but every removal/change is automatically derivable (no
  *     `CustomConversionRequired` anywhere).
  *   - [[BincompatVerdict.NonDerivable]] (2): at least one change requires a
  *     hand-written conversion (a `CustomConversionRequired`), or a removed type
  *     that is still referenced by a surviving TO type.
  */
sealed abstract class BincompatVerdict(val code: Int)

object BincompatVerdict {
  case object NoBreak extends BincompatVerdict(0)
  case object BreakingDerivable extends BincompatVerdict(1)
  case object NonDerivable extends BincompatVerdict(2)
}

/** A single change that is automatically derivable (contributes to exit 1 but
  * never to exit 2). `sourceTpe` is the FROM-side type id.
  */
final case class DerivableChange(sourceTpe: TypeId.User, kind: DerivableChange.Kind)

object DerivableChange {
  sealed trait Kind
  object Kind {
    /** A type present in both FROM and TO with a differing shape/dependencies. */
    case object ModifiedType extends Kind

    /** A removed type that no surviving TO type still references. */
    case object DerivableRemoval extends Kind
  }
}

/** A single change that is NOT automatically derivable (forces exit 2), tagged
  * with the reason. `RemovedTypeStillReferenced` has no `DerivationFailure`
  * because it arises from the reference scan, not from the rules engine.
  */
final case class NonDerivableChange(sourceTpe: TypeId.User, reason: NonDerivableChange.Reason)

object NonDerivableChange {
  sealed trait Reason
  object Reason {
    final case class CustomConversion(failure: DerivationFailure) extends Reason
    case object RemovedTypeStillReferenced extends Reason
  }
}

/** Full, structured result of classifying a FROM → TO domain pair. The `verdict`
  * is the reduced enum; the two lists carry the per-type evidence so that a
  * later renderer (T195) can explain the verdict.
  */
final case class BincompatResult(
  verdict: BincompatVerdict,
  derivable: List[DerivableChange],
  nonDerivable: List[NonDerivableChange],
)

/** Pure wire-compat classifier: FROM `Domain` × TO `Domain` → [[BincompatResult]].
  *
  * No CLI, no git, no I/O. Runs [[BaboonComparator.compare]] and
  * [[BaboonRules.compute]] over the two already-typed domains under a synchronous
  * `Either` effect and reduces their output to a verdict per the T185 spec.
  *
  * Reasoning why the reduction consults BOTH the [[Conversion]] list AND
  * [[BaboonChanges]]: a NEW type yields no `Conversion` at all, and an unmodified
  * type yields a trivial `DtoConversion`/`CopyEnumByName`/etc — so the conversion
  * list alone cannot separate exit 0 from exit 1. The set of changed type ids is
  * taken from [[BaboonChanges.changed]] (shallow/deep/fully modified) ∪ renamed;
  * removals are taken from the rules engine's `RemovedTypeNoConversion` and
  * resolved to derivable/non-derivable by the surviving-reference scan.
  */
object BincompatClassifier {
  private type EitherF[+E, +A] = Either[E, A]

  private implicit val error2: Error2[EitherF] = BioEither
  private implicit val maybeSuspend2: MaybeSuspend2[EitherF] = new MaybeSuspend2[EitherF]
  private implicit val parallel2: ParallelErrorAccumulatingOps2[EitherF] = new ParallelErrorAccumulatingOps2[EitherF] {
    override def InnerF: Error2[EitherF] = BioEither
    override def parTraverse[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      InnerF.traverse(l)(f)
    override def parTraverseN[E, A, B](maxConcurrent: Int)(l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      parTraverse(l)(f)
    override def parTraverseNCore[E, A, B](l: Iterable[A])(f: A => Either[E, B]): Either[E, List[B]] =
      parTraverse(l)(f)
    override def zipWithPar[E, A, B, C](fa: Either[E, A], fb: Either[E, B])(f: (A, B) => C): Either[E, C] =
      InnerF.map2(fa, fb)(f)
  }

  private val enquiries: BaboonEnquiries = new BaboonEnquiries.BaboonEnquiriesImpl
  private val typeInfo: TypeInfo         = new TypeInfo.TypeInfoImpl
  private val rules: BaboonRules[EitherF] =
    new BaboonRules.BaboonRulesImpl[EitherF](BLogger.Noop, typeInfo)
  private val comparator: BaboonComparator[EitherF] =
    new BaboonComparator.BaboonComparatorImpl[EitherF](enquiries, rules, BLogger.Noop)

  /** Signals a comparison/rules failure for a pair that the caller asserted was
    * comparable. The classifier is a pure function of two already-typed,
    * evolvable domains; a Left here indicates a defect in the caller's inputs,
    * not a wire-compat verdict, so we fail fast rather than encode it as a
    * verdict.
    */
  final class BincompatClassifierException(val issues: NEList[BaboonIssue])
      extends RuntimeException(s"Domain comparison failed: ${issues.toList.mkString("; ")}")

  def classify(fromDomain: Domain, toDomain: Domain): BincompatResult = {
    // Spec: compare(last = TO, prev = FROM); compute(prev = FROM, last = TO, diff).
    val outcome: Either[NEList[BaboonIssue], BincompatResult] = for {
      diff    <- comparator.compare(last = toDomain, prev = fromDomain)
      ruleset <- rules.compute(prev = fromDomain, last = toDomain, diff)
    } yield reduce(toDomain, diff, ruleset)

    outcome match {
      case Right(result) => result
      case Left(issues)  => throw new BincompatClassifierException(issues)
    }
  }

  private def reduce(
    toDomain: Domain,
    diff: BaboonDiff,
    ruleset: BaboonRuleset,
  ): BincompatResult = {
    val changes = diff.changes

    // Non-derivable via the rules engine: any CustomConversionRequired.
    val customConversions: List[NonDerivableChange] = ruleset.conversions.collect {
      case CustomConversionRequired(sourceTpe, reason, _) =>
        NonDerivableChange(sourceTpe, NonDerivableChange.Reason.CustomConversion(reason))
    }

    // Surviving-reference scan: type-ids still referenced by any TO user type.
    val referencedInTo: Set[TypeId] = survivingReferences(toDomain)

    // RemovedTypeNoConversion → non-derivable IF still referenced by a surviving
    // TO type, else a derivable removal.
    val removedNoConversions: List[RemovedTypeNoConversion] = ruleset.conversions.collect {
      case r: RemovedTypeNoConversion => r
    }
    val (removedReferenced, removedUnreferenced) =
      removedNoConversions.partition(r => referencedInTo.contains(r.sourceTpe))

    val removedNonDerivable: List[NonDerivableChange] = removedReferenced.map { r =>
      NonDerivableChange(r.sourceTpe, NonDerivableChange.Reason.RemovedTypeStillReferenced)
    }
    val removedDerivable: List[DerivableChange] = removedUnreferenced.map { r =>
      DerivableChange(r.sourceTpe, DerivableChange.Kind.DerivableRemoval)
    }

    // 'Change to an existing type' = a type present in BaboonDiff.changes as
    // shallow/deep/fully-modified OR renamed. Only TypeId.User entries survive
    // (the comparator asserts `changed.forall(_.isInstanceOf[TypeId.User])`).
    val modifiedUserIds: Set[TypeId.User] =
      (changes.changed ++ changes.renamed.values.toSet).collect { case u: TypeId.User => u }
    val modifiedDerivable: List[DerivableChange] =
      modifiedUserIds.toList.map(id => DerivableChange(id, DerivableChange.Kind.ModifiedType))

    val nonDerivable: List[NonDerivableChange] = customConversions ++ removedNonDerivable
    val derivable: List[DerivableChange]       = modifiedDerivable ++ removedDerivable

    val verdict =
      if (nonDerivable.nonEmpty) {
        BincompatVerdict.NonDerivable
      } else if (derivable.nonEmpty) {
        // No CustomConversionRequired anywhere AND ≥1 change to an existing or
        // (derivably) removed type. NonDataTypeTypeNoConversion is wire-neutral
        // and never lands in `derivable`, so it cannot force exit 1 on its own.
        BincompatVerdict.BreakingDerivable
      } else {
        // Strictly additions and/or unmodified types.
        BincompatVerdict.NoBreak
      }

    BincompatResult(verdict, derivable, nonDerivable)
  }

  /** All type-ids referenced by any surviving (user) type in the TO domain, via
    * field types, ADT branch members and own fields, contract fields, service
    * method signatures, and foreign runtime/ref mappings. Used to decide whether
    * a removed FROM type is still reachable on the TO side.
    */
  private def survivingReferences(toDomain: Domain): Set[TypeId] = {
    toDomain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, defn, _, _) => defn
    }.flatMap(referencesOf).toSet
  }

  private def referencesOf(defn: Typedef.User): Set[TypeId] = defn match {
    case d: Typedef.Dto =>
      d.fields.flatMap(f => enquiries.explode(f.tpe)).toSet ++ d.contracts.toSet
    case _: Typedef.Enum =>
      Set.empty
    case a: Typedef.Adt =>
      a.members.toList.toSet[TypeId] ++
        a.fields.flatMap(f => enquiries.explode(f.tpe)).toSet ++
        a.contracts.toSet
    case c: Typedef.Contract =>
      c.fields.flatMap(f => enquiries.explode(f.tpe)).toSet ++ c.contracts.toSet
    case s: Typedef.Service =>
      s.methods.flatMap(m => (Set(m.sig) ++ m.out.toSet ++ m.err.toSet).flatMap(enquiries.explode)).toSet
    case f: Typedef.Foreign =>
      val bindingRefs = f.bindings.values.flatMap {
        case Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(typeRef)) => enquiries.explode(typeRef)
        case _                                                                  => Set.empty[TypeId]
      }.toSet
      bindingRefs ++ f.runtimeMapping.map(enquiries.explode).getOrElse(Set.empty)
  }
}

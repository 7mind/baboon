package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{
  RawDefn,
  RawDomain,
  RawHeader,
  RawTLDef
}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NonEmptyList
import izumi.fundamentals.graphs.ToposortError
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.language.{
  SourceFilePosition,
  SourceFilePositionMaterializer
}

sealed trait BaboonIssue

object BaboonIssue {

  sealed trait BaboonBug {
    this: BaboonIssue =>
  }

  //
  sealed trait IOIssue extends BaboonIssue
  case class CantReadInput(issue: Throwable) extends IOIssue
  case class CantWriteOutput(path: String, issue: Throwable) extends IOIssue

  //
  sealed trait ParserIssue extends BaboonIssue
  case class ParserFailed(fail: Parsed.Failure) extends ParserIssue

  //
  sealed trait TyperIssue extends BaboonIssue
  case class ScalarExpected(id: TypeId) extends TyperIssue with BaboonBug
  case class CollectionExpected(id: TypeId) extends TyperIssue with BaboonBug

  case class NonUniqueDomainVersions(problems: Map[Version, List[Domain]])
      extends TyperIssue
  case class EmptyDomainFamily(pkg: Pkg) extends TyperIssue with BaboonBug
  case class NonUniqueLineages(problems: Map[Pkg, List[BaboonLineage]])
      extends TyperIssue
  case class EmptyFamily(input: List[BaboonParser.Input])
      extends TyperIssue
      with BaboonBug
  case class DuplicatedTypeId(ids: Set[TypeId],
                              pkg: Pkg,
                              owner: Owner,
                              defn: RawTLDef)
      extends TyperIssue

  case class DuplicatedTypedef(dupes: Map[TypeId, List[DomainMember.User]],
                               pkg: Pkg,
                               owner: Owner,
                               defn: RawDefn)
      extends TyperIssue

  case class UnexpectedBuiltin(id: TypeId.Builtin, owner: Owner)
      extends TyperIssue

  case class MissingTypeId(domain: Pkg, missing: Set[TypeId]) extends TyperIssue

  case class NonUniqueEnumBranches(problems: Map[String, List[EnumMember]],
                                   id: TypeId.User,
  ) extends TyperIssue
  case class EmptyEnum(id: TypeId.User) extends TyperIssue

  case class NonUniqueFields(id: TypeId.User, e: Map[String, List[Field]])
      extends TyperIssue

  case class EmptyAdt(id: TypeId.User) extends TyperIssue

  case class EmptyGenericArgs(id: TypeId) extends TyperIssue

  case class DuplicatedTypedefs(model: RawDomain,
                                problem: Map[TypeId, List[DomainMember]])
      extends TyperIssue

  case class CircularDependency(model: RawDomain,
                                problem: ToposortError[TypeId])
      extends TyperIssue

  case class EmptyPackageId(header: RawHeader) extends TyperIssue

  case class NonUniqueBuiltins(problem: Map[TypeId, List[DomainMember]])
      extends TyperIssue
      with BaboonBug

  //
  sealed trait EvolutionIssue extends BaboonIssue

  case class BrokenComparison(o: List[Version])
      extends EvolutionIssue
      with BaboonBug

  case class UnexpectedDiffType(o: TypedefDiff)
      extends EvolutionIssue
      with BaboonBug

  case class NonUniqueDiff(e: Map[EvolutionStep, List[BaboonDiff]])
      extends EvolutionIssue
      with BaboonBug

  case class NonUniqueRuleset(e: Map[EvolutionStep, List[BaboonRuleset]])
      extends EvolutionIssue
      with BaboonBug

  case class IncomparableTypedefs(o: DomainMember, n: DomainMember)
      extends EvolutionIssue

  case class NonUniqueDiffs(e: Map[TypeId, List[TypedefDiff]])
      extends EvolutionIssue
      with BaboonBug

  case class MismatchingTypedefs(o1: Typedef.User, o2: Typedef.User)
      extends EvolutionIssue
      with BaboonBug

  //
  sealed trait VerificationIssue extends BaboonIssue

  case class MissingTypeDef(domain: Domain, missing: Set[TypeId])
      extends VerificationIssue

  case class ReferentialCyclesFound(domain: Domain,
                                    loops: Set[LoopDetector.Cycles[TypeId]])
      extends VerificationIssue

  case class ConflictingDtoFields(u: Typedef.Dto,
                                  dupes: Map[String, List[Field]])
      extends VerificationIssue

  case class ConflictingEnumBranches(
    u: Typedef.Enum,
    dupes: Map[String, NonEmptyList[EnumMember]]
  ) extends VerificationIssue

  case class ConflictingAdtBranches(
    u: Typedef.Adt,
    dupes: Map[String, NonEmptyList[TypeId.User]]
  ) extends VerificationIssue

  case class ConflictingTypeIds(domain: Domain,
                                dupes: Map[String, Iterable[DomainMember]])
      extends VerificationIssue

  case class EmptyEnumDef(e: Typedef.Enum) extends VerificationIssue

  case class EmptyAdtDef(a: Typedef.Adt) extends VerificationIssue

  case class MissingEvoDiff(prev: Domain,
                            next: Domain,
                            missingDiffs: Set[TypeId])
      extends VerificationIssue
      with BaboonBug

  case class MissingEvoConversion(prev: Domain,
                                  next: Domain,
                                  missingConversions: Set[TypeId],
                                  extraConversions: Set[TypeId])
      extends VerificationIssue
      with BaboonBug

  case class BrokenConversion(c: Conversion)
      extends VerificationIssue
      with BaboonBug

  sealed trait ConversionIssue
  object ConversionIssue {
    case object TypeMismatch extends ConversionIssue
    case object RemovedEnumBranches extends ConversionIssue
    case object RemovedAdtBranches extends ConversionIssue
    case object RemovedDtoFields extends ConversionIssue
    case object IncorrectFields extends ConversionIssue
    case object ConflictingFields extends ConversionIssue
  }
  case class IncorrectConversionApplication(c: Conversion,
                                            o: DomainMember,
                                            n: DomainMember,
                                            issue: ConversionIssue)
      extends VerificationIssue
      with BaboonBug

  //
  sealed trait TranslationIssue extends BaboonIssue
  case class NonUniqueOutputFiles(c: Map[String, List[String]])
      extends TranslationIssue

  case class TranslationBug()(implicit context: ExceptionContext)
      extends TranslationIssue
      with BaboonBug {
    override def toString: String = s"TranslationBug(${context})"
  }
}

final case class ExceptionContext(sourceFilePosition: SourceFilePosition,
                                  stackTrace: Throwable,
) {
  import izumi.fundamentals.platform.exceptions.IzThrowable.*
  import izumi.fundamentals.platform.strings.IzString.*
  override def toString: String =
    s"""ExceptionContext($sourceFilePosition) {
       |${stackTrace.stackTrace.shift(4)}
       |}""".stripMargin
}

object ExceptionContext {
  implicit def materializeExceptionContext(
    implicit pos: SourceFilePositionMaterializer
  ): ExceptionContext = new ExceptionContext(pos.get, new Throwable())
}

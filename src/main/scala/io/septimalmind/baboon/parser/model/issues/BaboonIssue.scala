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
import izumi.fundamentals.graphs.ToposortError

sealed trait BaboonIssue

object BaboonIssue {

  sealed trait TranslationIssue extends BaboonIssue

  sealed trait BaboonBug {
    this: BaboonIssue =>
  }

  //
  sealed trait IOIssue extends BaboonIssue
  case class CantReadInput(issue: Throwable) extends IOIssue

  //
  sealed trait ParserIssue extends BaboonIssue
  case class ParserFailed(fail: Parsed.Failure) extends ParserIssue

  //
  sealed trait TyperIssue extends BaboonIssue
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

  case class UnexpectedDiffType(o: TypedefDiff)
      extends EvolutionIssue
      with BaboonBug

  case class NonUniqueDiff(e: Map[Version, List[BaboonDiff]])
      extends EvolutionIssue
      with BaboonBug

  case class NonUniqueRuleset(e: Map[Version, List[BaboonRuleset]])
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

}

package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Typedef.ForeignEntry
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.ToposortError
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.exceptions.Issue.IssueContext

sealed trait BaboonIssue

object BaboonIssue {

  sealed trait BaboonBug {
    this: BaboonIssue =>
  }

  //
  sealed trait IOIssue extends BaboonIssue

  case class CantReadInput(path: String, throwable: Throwable) extends IOIssue

  case class CantWriteOutput(path: String, throwable: Throwable) extends IOIssue

  case class CantCleanupTarget(paths: Seq[String], safeToRemoveExtensions: Seq[String], error: Option[Throwable]) extends IOIssue

  //
  sealed trait ParserIssue extends BaboonIssue

  case class ParserFailed(error: Parsed.Failure, path: FSPath) extends ParserIssue
  case class IncludeNotFound(path: String) extends ParserIssue

  //
  sealed trait TyperIssue extends BaboonIssue

  case class ScalarExpected(id: TypeId, meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class CollectionExpected(id: TypeId, meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class NonUniqueDomainVersions(
    duplicateDomainVersion: Map[Version, List[Domain]]
  ) extends TyperIssue

  case class EmptyDomainFamily(pkg: Pkg) extends TyperIssue with BaboonBug

  case class NonUniqueLineages(nonUniqueLineages: Map[Pkg, List[BaboonLineage]]) extends TyperIssue

  case class EmptyFamily(input: List[BaboonParser.Input]) extends TyperIssue with BaboonBug

  case class UnexpectedBuiltin(id: TypeId.Builtin, pkg: Pkg, meta: RawNodeMeta) extends TyperIssue

  case class UnexpectedNonBuiltin(name: TypeName, pkg: Pkg, path: Scope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class MissingTypeId(domain: Pkg, missing: Set[TypeId], meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueEnumBranches(
    duplicateEnumMembers: Map[String, List[EnumMember]],
    id: TypeId.User,
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class NonUniqueForeignEntries(
    duplicateForeignEntries: Map[String, List[ForeignEntry]],
    id: TypeId.User,
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class EmptyEnum(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueFields(id: TypeId.User, duplicateFields: Map[String, List[Field]], meta: RawNodeMeta) extends TyperIssue

  case class MissingContractFields(id: TypeId.User, missingFields: Seq[Field], meta: RawNodeMeta) extends TyperIssue

  case class EmptyAdt(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue

  case class EmptyGenericArgs(id: TypeId, meta: RawNodeMeta) extends TyperIssue

  case class DuplicatedTypedefs(
    model: RawDomain,
    duplicateTypeDefs: Map[TypeId, List[DomainMember]],
  ) extends TyperIssue

  case class EmptyPackageId(header: RawHeader) extends TyperIssue
  case class ScopedRefToNamespacedGeneric(prefix: Seq[RawTypeName], meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueTypedefs(
    duplicateTypedefs: Map[TypeId, List[DomainMember]],
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class NonUniqueScope(
    nonUniqueScopes: Map[Scope.ScopeName, List[Scope.NestedScope[FullRawDefn]]],
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class UnexpectedScoping(e: List[Scope[ExtendedRawDefn]], meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class TodoTyperIssue(descr: String) extends TyperIssue

  case class ScopeCannotBeEmpty(member: RawDefn) extends TyperIssue

  case class BadEnumName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadFieldName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadTypeName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadInheritance(
    bad: Map[TypeId.User, List[(Set[TypeId.User], Scope[ExtendedRawDefn])]],
    meta: RawNodeMeta,
  ) extends TyperIssue
    with BaboonBug

  case class CircularInheritance(error: ToposortError[TypeId.User], meta: RawNodeMeta) extends TyperIssue

  case class NameNotFound(pkg: Pkg, name: ScopedRef, meta: RawNodeMeta) extends TyperIssue

  case class UnexpectedScopeLookup(b: Scope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class NamSeqeNotFound(names: Seq[RawTypeName], scope: Scope.SubScope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class DuplicatedTypes(dupes: Set[TypeId], meta: RawNodeMeta) extends TyperIssue

  case class WrongParent(id: TypeId.User, id1: TypeId, meta: RawNodeMeta) extends TyperIssue

  //
  sealed trait EvolutionIssue extends BaboonIssue

  case class BrokenComparison(versions: List[Version]) extends EvolutionIssue with BaboonBug

  case class UnexpectedDiffType(o: TypedefDiff, expected: String) extends EvolutionIssue with BaboonBug

  case class NonUniqueDiff(evolutions: Map[EvolutionStep, List[BaboonDiff]]) extends EvolutionIssue with BaboonBug

  case class NonUniqueRuleset(
    evolutions: Map[EvolutionStep, List[BaboonRuleset]]
  ) extends EvolutionIssue
    with BaboonBug

  case class NonUniquePrevVersions(versions: Map[Version, List[Version]]) extends EvolutionIssue with BaboonBug

  case class IncomparableTypedefs(old: DomainMember, newMember: DomainMember) extends EvolutionIssue

  case class NonUniqueDiffs(nonUniqueDiffs: Map[TypeId, List[TypedefDiff]]) extends EvolutionIssue with BaboonBug

  case class MismatchingTypedefs(o1: Typedef.User, o2: Typedef.User) extends EvolutionIssue with BaboonBug

  //
  sealed trait VerificationIssue extends BaboonIssue

  case class MissingTypeDef(domain: Domain, missing: Set[TypeId]) extends VerificationIssue

  case class ReferentialCyclesFound(domain: Domain, loops: Set[LoopDetector.Cycles[TypeId]]) extends VerificationIssue

  case class IncorrectRootFound(domain: Domain, badRoots: Seq[TypeId.User]) extends VerificationIssue

  case class ConflictingDtoFields(dto: TypeId.User, dupes: Map[String, List[Field]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingEnumBranches(uEnum: Typedef.Enum, dupes: Map[String, NEList[EnumMember]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingAdtBranches(adt: Typedef.Adt, dupes: Map[String, NEList[TypeId.User]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingTypeIds(domain: Domain, dupes: Map[String, Iterable[DomainMember]]) extends VerificationIssue

  case class BadFieldNames(e: TypeId.User, bad: Seq[String], meta: RawNodeMeta) extends VerificationIssue

  case class EmptyEnumDef(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class EitherAllOrNoneEnumMembersMustHaveConstants(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class WrongEnumConstant(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class EmptyAdtDef(a: Typedef.Adt, meta: RawNodeMeta) extends VerificationIssue

  case class UnderscoredDefinitionRetained(s: DomainMember.User, meta: RawNodeMeta) extends VerificationIssue

  case class MissingEvoDiff(prev: Domain, next: Domain, missingDiffs: Set[TypeId]) extends VerificationIssue with BaboonBug

  case class MissingEvoConversion(prev: Domain, next: Domain, missingConversions: Set[TypeId], extraConversions: Set[TypeId]) extends VerificationIssue with BaboonBug

  case class BrokenConversion(c: Conversion) extends VerificationIssue with BaboonBug

  sealed trait ConversionIssue
  object ConversionIssue {
    case object TypeMismatch extends ConversionIssue
    case object RemovedEnumBranches extends ConversionIssue
    case object RemovedAdtBranches extends ConversionIssue
    case object RemovedDtoFields extends ConversionIssue
    case object IncorrectFields extends ConversionIssue
    case object ConflictingFields extends ConversionIssue
  }
  case class IncorrectConversionApplication(c: Conversion, o: DomainMember, n: DomainMember, issue: ConversionIssue) extends VerificationIssue with BaboonBug

  case class PathologicGenerics(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  case class SetsCantContainGenerics(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  case class MapKeysShouldNotBeGeneric(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  //
  sealed trait TranslationIssue extends BaboonIssue
  case class NonUniqueOutputFiles(c: Map[String, List[OutputFile]]) extends TranslationIssue

  case class TranslationBug()(implicit val context: IssueContext) extends TranslationIssue with BaboonBug with Issue

}

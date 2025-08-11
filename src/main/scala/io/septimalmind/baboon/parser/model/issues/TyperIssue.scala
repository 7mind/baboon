package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Typedef.ForeignEntry
import izumi.fundamentals.graphs.ToposortError
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait TyperIssue extends IssueGroup

object TyperIssue {
  implicit def wrap(issue: TyperIssue): BaboonIssue = BaboonIssue.Typer(issue)

  case class GenericTyperIssue(message: String, meta: RawNodeMeta) extends TyperIssue

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

  case class NonUniqueMethodNames(serviceName: String, duplicateMethods: Map[String, List[String]], meta: RawNodeMeta) extends TyperIssue

  case class ServiceMissingOutput(serviceName: String, methodName: String, meta: RawNodeMeta) extends TyperIssue

  case class ServiceMultipleOutputs(serviceName: String, methodName: String, count: Int, meta: RawNodeMeta) extends TyperIssue

  case class ServiceMultipleErrors(serviceName: String, methodName: String, count: Int, meta: RawNodeMeta) extends TyperIssue

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

  implicit val typerIssuePrinter: IssuePrinter[TyperIssue] = {
    case i: GenericTyperIssue  => IssuePrinter[GenericTyperIssue].stringify(i)
    case i: ScalarExpected     => IssuePrinter[ScalarExpected].stringify(i)
    case i: CollectionExpected => IssuePrinter[CollectionExpected].stringify(i)
    case i: NonUniqueDomainVersions =>
      IssuePrinter[NonUniqueDomainVersions].stringify(i)
    case i: EmptyDomainFamily     => IssuePrinter[EmptyDomainFamily].stringify(i)
    case i: NonUniqueLineages     => IssuePrinter[NonUniqueLineages].stringify(i)
    case i: EmptyFamily           => IssuePrinter[EmptyFamily].stringify(i)
    case i: UnexpectedBuiltin     => IssuePrinter[UnexpectedBuiltin].stringify(i)
    case i: UnexpectedNonBuiltin  => IssuePrinter[UnexpectedNonBuiltin].stringify(i)
    case i: MissingTypeId         => IssuePrinter[MissingTypeId].stringify(i)
    case i: NonUniqueEnumBranches => IssuePrinter[NonUniqueEnumBranches].stringify(i)
    case i: NonUniqueForeignEntries =>
      IssuePrinter[NonUniqueForeignEntries].stringify(i)
    case i: EmptyEnum             => IssuePrinter[EmptyEnum].stringify(i)
    case i: NonUniqueFields       => IssuePrinter[NonUniqueFields].stringify(i)
    case i: EmptyAdt              => IssuePrinter[EmptyAdt].stringify(i)
    case i: EmptyGenericArgs      => IssuePrinter[EmptyGenericArgs].stringify(i)
    case i: DuplicatedTypedefs    => IssuePrinter[DuplicatedTypedefs].stringify(i)
    case i: EmptyPackageId        => IssuePrinter[EmptyPackageId].stringify(i)
    case i: NonUniqueTypedefs     => IssuePrinter[NonUniqueTypedefs].stringify(i)
    case i: NonUniqueScope        => IssuePrinter[NonUniqueScope].stringify(i)
    case i: UnexpectedScoping     => IssuePrinter[UnexpectedScoping].stringify(i)
    case i: ScopeCannotBeEmpty    => IssuePrinter[ScopeCannotBeEmpty].stringify(i)
    case i: BadEnumName           => IssuePrinter[BadEnumName].stringify(i)
    case i: BadFieldName          => IssuePrinter[BadFieldName].stringify(i)
    case i: BadTypeName           => IssuePrinter[BadTypeName].stringify(i)
    case i: BadInheritance        => IssuePrinter[BadInheritance].stringify(i)
    case i: CircularInheritance   => IssuePrinter[CircularInheritance].stringify(i)
    case i: NameNotFound          => IssuePrinter[NameNotFound].stringify(i)
    case i: UnexpectedScopeLookup => IssuePrinter[UnexpectedScopeLookup].stringify(i)
    case i: NamSeqeNotFound       => IssuePrinter[NamSeqeNotFound].stringify(i)
    case i: DuplicatedTypes       => IssuePrinter[DuplicatedTypes].stringify(i)
    case i: WrongParent           => IssuePrinter[WrongParent].stringify(i)
    case i: MissingContractFields => IssuePrinter[MissingContractFields].stringify(i)
    case i: TodoTyperIssue        => i.descr
    case i: ScopedRefToNamespacedGeneric =>
      IssuePrinter[ScopedRefToNamespacedGeneric].stringify(i)
    case i: NonUniqueMethodNames =>
      IssuePrinter[NonUniqueMethodNames].stringify(i)
    case i: ServiceMissingOutput =>
      IssuePrinter[ServiceMissingOutput].stringify(i)
    case i: ServiceMultipleOutputs =>
      IssuePrinter[ServiceMultipleOutputs].stringify(i)
    case i: ServiceMultipleErrors =>
      IssuePrinter[ServiceMultipleErrors].stringify(i)
  }

  implicit val scalaExpectedPrinter: IssuePrinter[ScalarExpected] =
    new BugPrinter[ScalarExpected] {
      override def errorMessage(bug: ScalarExpected): String = {
        s"""${extractLocation(bug.meta)}
           |Scalar type is expected, instead provided: ${bug.id.toString}
           |""".stripMargin
      }
    }

  implicit val collectionExpectedPrinter: IssuePrinter[CollectionExpected] =
    new BugPrinter[CollectionExpected] {
      override def errorMessage(bug: CollectionExpected): String = {
        s"""${extractLocation(bug.meta)}
           |One of collection types expected: map, opt, list, set
           |Instead provided: ${bug.id.toString}
           |""".stripMargin
      }
    }

  implicit val nonUniqueDomainVersionsPrinter: IssuePrinter[NonUniqueDomainVersions] =
    (issue: NonUniqueDomainVersions) => {
      val stringProblems = issue.duplicateDomainVersion.map {
        case (version, domains) =>
          s"""Version: $version => models: ${domains
              .map(_.id.toString)
              .mkString(", ")}
             |""".stripMargin
      }
        .mkString("\n")
      s"""Duplicate domain versions have been found
         |$stringProblems
         |""".stripMargin
    }

  implicit val emptyDomainFamilyPrinter: IssuePrinter[EmptyDomainFamily] =
    new BugPrinter[EmptyDomainFamily] {
      override def errorMessage(bug: EmptyDomainFamily): String = {
        s"Empty model: ${bug.pkg.toString}"
      }
    }

  implicit val nonUniqueLineagesPrinter: IssuePrinter[NonUniqueLineages] =
    (issue: NonUniqueLineages) => {
      s"Non unique models found: ${issue.nonUniqueLineages.keys.toList.mkString(", ")}"
    }

  implicit val emptyFamilyPrinter: IssuePrinter[EmptyFamily] =
    new BugPrinter[EmptyFamily] {
      override def errorMessage(bug: EmptyFamily): String = {
        s"Empty models in files:${bug.input.niceList()}"
      }
    }

  implicit val unexpectedBuiltinPrinter: IssuePrinter[UnexpectedBuiltin] =
    (issue: UnexpectedBuiltin) => {
      s"""${extractLocation(issue.meta)}
         |In Model ${issue.pkg.toString}, the type ${issue.id.toString} - is a built in type it cannot be used as a user defined type
         |""".stripMargin
    }

  implicit val unexpectedNonBuiltin: IssuePrinter[UnexpectedNonBuiltin] =
    (issue: UnexpectedNonBuiltin) => {
      s"""${extractLocation(issue.meta)}
         |Model: ${issue.pkg.toString}
         |The type ${issue.name.name} takes a builtin position but there is no such builtin type""".stripMargin
    }

  implicit val missingTypeIdPrinter: IssuePrinter[MissingTypeId] =
    (issue: MissingTypeId) => {
      s"""${extractLocation(issue.meta)}
         |Missing type ids: ${issue.missing.toList.map(_.name.name).niceList()}
         |""".stripMargin
    }

  implicit val nonUniqueEnumBranchesPrinter: IssuePrinter[NonUniqueEnumBranches] = (issue: NonUniqueEnumBranches) => {
    val stringProblems = issue.duplicateEnumMembers.values
      .map(_.map(_.name).niceList())
      .mkString("\n")
    s"""${extractLocation(issue.meta)}
       |Enum ${issue.id.toString} has members with identical names:$stringProblems
       |""".stripMargin
  }

  implicit val nonUniqueForeignEntriesPrinter: IssuePrinter[NonUniqueForeignEntries] =
    (issue: NonUniqueForeignEntries) => {
      val stringProblems = issue.duplicateForeignEntries.values
        .map(_.map(_.lang).niceList())
        .mkString("\n")
      s"""${extractLocation(issue.meta)}
         |Foreign type ${issue.id.toString} has members with identical names:$stringProblems
         |""".stripMargin
    }

  implicit val emptyEnumPrinter: IssuePrinter[EmptyEnum] =
    (issue: EmptyEnum) => {
      s"""${extractLocation(issue.meta)}
         |Found empty enum: ${issue.id.toString}
         |""".stripMargin
    }

  implicit val nonUniqueFieldsPrinter: IssuePrinter[NonUniqueFields] =
    (issue: NonUniqueFields) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.id.toString} has fields with identical names:${issue.duplicateFields.values.flatten
          .niceList()}
         |""".stripMargin
    }

  implicit val emptyADTPrinter: IssuePrinter[EmptyAdt] = (issue: EmptyAdt) => {
    s"""${extractLocation(issue.meta)}
       |Found empty ADT: ${issue.id.toString}
       |""".stripMargin
  }

  implicit val emptyGenericArgsPrinter: IssuePrinter[EmptyGenericArgs] =
    (issue: EmptyGenericArgs) => {
      s"""${extractLocation(issue.meta)}
         |The type ${issue.id.name.name} is expected to be used with type argument
         |""".stripMargin
    }

  implicit val duplicatedTypedefsPrinter: IssuePrinter[DuplicatedTypedefs] =
    (issue: DuplicatedTypedefs) => {
      val duplicateTypesString = issue.duplicateTypeDefs.map {
        case (typeId, members) =>
          s"type: $typeId => members: ${members.map(_.id.name.name).mkString(", ")}"
      }
      s"""${extractLocation(issue.model.header.meta)}
         |Duplicate type definitions have been found:${duplicateTypesString
          .niceList()}
         |""".stripMargin
    }

  implicit val emptyPackageIdPrinter: IssuePrinter[EmptyPackageId] =
    (issue: EmptyPackageId) => {
      s"""${extractLocation(issue.header.meta)}
         |There is an empty package name
         |""".stripMargin
    }

  implicit val nonUniqueTypedefsPrinter: IssuePrinter[NonUniqueTypedefs] =
    (issue: NonUniqueTypedefs) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate type members found: ${issue.duplicateTypedefs.values.flatten
          .map(_.id.name.name)
          .mkString}
         |""".stripMargin
    }

  implicit val nonUniqueScopePrinter: IssuePrinter[NonUniqueScope] =
    (issue: NonUniqueScope) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate scopes have been found:${issue.nonUniqueScopes
          .map(_._2.map(_.name.name).mkString(", "))
          .niceList()}
         |""".stripMargin
    }

  implicit val unexpectedScopingPrinter: IssuePrinter[UnexpectedScoping] =
    new BugPrinter[UnexpectedScoping] {
      override def errorMessage(bug: UnexpectedScoping): String = {
        s"""${extractLocation(bug.meta)}
           |Unexpected scoping: ${bug.e.niceList()}
           |""".stripMargin
      }
    }

  implicit val scopeCannotBeEmptyPrinter: IssuePrinter[ScopeCannotBeEmpty] =
    (issue: ScopeCannotBeEmpty) => {
      val memberType = issue.member match {
        case _: RawDto       => "DTO"
        case _: RawEnum      => "Enum"
        case _: RawAdt       => "ADT"
        case _: RawForeign   => "Foreign"
        case _: RawContract  => "Contract"
        case _: RawNamespace => "Namespace"
        case _: RawService   => "Service"
      }
      s"""${extractLocation(issue.member.meta)}
         |Found an empty $memberType: ${issue.member.name.name}
         |""".stripMargin
    }

  implicit val badEnumNamePrinter: IssuePrinter[BadEnumName] =
    (issue: BadEnumName) => {
      s"""${extractLocation(issue.meta)}
         |Bad enum name: ${issue.name}
         |""".stripMargin
    }

  implicit val badFieldNamePrinter: IssuePrinter[BadFieldName] =
    (issue: BadFieldName) => {
      s"""${extractLocation(issue.meta)}
         |Bad field name: ${issue.name}
         |""".stripMargin
    }

  implicit val badTypeNamePrinter: IssuePrinter[BadTypeName] =
    (issue: BadTypeName) => {
      s"""${extractLocation(issue.meta)}
         |Bad type name: ${issue.name}
         |""".stripMargin
    }

  implicit val badInheritancePrinter: IssuePrinter[BadInheritance] =
    new BugPrinter[BadInheritance] {
      override def errorMessage(bug: BadInheritance): String = {
        val badStr = bug.bad.map {
          case (id, deps) =>
            val depsStr = deps.map {
              case (deps, scope) =>
                s"""Scope: ${scope.toString}
                   |Dependencies: ${deps.map(_.toString).mkString(", ")}
                   |""".stripMargin
            }
              .niceList()

            s"""Type: ${id.toString}
               |$depsStr
               |""".stripMargin
        }
          .niceList("\t")
        s"""${extractLocation(bug.meta)}
           |Bad inheritance:
           |$badStr
           |""".stripMargin
      }
    }

  implicit val circularInheritancePrinter: IssuePrinter[CircularInheritance] =
    (issue: CircularInheritance) => {
      val errorString = issue.error match {
        case ToposortError.UnexpectedLoop(_, matrix) =>
          matrix.links.map {
            case (typeId, children) =>
              s"type: ${typeId.name.name} => children: ${children.toList.map(_.name.name).mkString(", ")}"
          }
            .niceList()
        case _ => ""
      }
      s"""${extractLocation(issue.meta)}
         |Circular inheritance have been found:$errorString
         |""".stripMargin
    }

  implicit val nameNotFoundPrinter: IssuePrinter[NameNotFound] =
    (issue: NameNotFound) => {
      s"""${extractLocation(issue.meta)}
         |Type not found: ${issue.name.path.head.name}
         |""".stripMargin
    }

  implicit val unexpectedScopeLookupPrinter: IssuePrinter[UnexpectedScopeLookup] = (issue: UnexpectedScopeLookup) => {
    s"""${extractLocation(issue.meta)}
       |Unexpected scope: ${issue.b.toString}
       |""".stripMargin
  }

  implicit val namSeqeNotFoundPrinter: IssuePrinter[NamSeqeNotFound] =
    (issue: NamSeqeNotFound) => {
      s"""${extractLocation(issue.meta)}
         |Members: ${issue.names
          .map(_.name)
          .mkString} are not found in scope: ${issue.scope.name.name}
         |""".stripMargin
    }

  implicit val duplicatedTypesPrinter: IssuePrinter[DuplicatedTypes] =
    (issue: DuplicatedTypes) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate types have been found:${issue.dupes
          .map(_.name.name)
          .niceList()}
         |""".stripMargin
    }

  implicit val wrongParentPrinter: IssuePrinter[WrongParent] =
    (issue: WrongParent) => {
      s"""${extractLocation(issue.meta)}
         |DTO parent is expected instead provided => name:${issue.id.toString} type:${issue.id1.name.name}
         |""".stripMargin
    }

  implicit val missingContractFields: IssuePrinter[MissingContractFields] =
    (issue: MissingContractFields) => {
      s"""${extractLocation(issue.meta)}
         |Contract fields were removed => name:${issue.id.toString} type:${issue.id}, missing: ${issue.missingFields
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val scopedRefToNamespacedGeneric: IssuePrinter[ScopedRefToNamespacedGeneric] =
    (issue: ScopedRefToNamespacedGeneric) => {
      s"""${extractLocation(issue.meta)}
         |A reference to user-defined generic, which is not supported, prefix: ${issue.prefix
          .map(_.name)
          .mkString(".")}
         |""".stripMargin
    }

  implicit val nonUniqueMethodNamesPrinter: IssuePrinter[NonUniqueMethodNames] = (issue: NonUniqueMethodNames) => {
    val duplicatesString = issue.duplicateMethods.map {
      case (methodName, occurrences) =>
        s"method: $methodName => occurrences: ${occurrences.mkString(", ")}"
    }.mkString("\n")
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName} has methods with identical names:
       |$duplicatesString
       |""".stripMargin
  }

  implicit val serviceMissingOutputPrinter: IssuePrinter[ServiceMissingOutput] = (issue: ServiceMissingOutput) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: missing output type definition
       |""".stripMargin
  }

  implicit val serviceMultipleOutputsPrinter: IssuePrinter[ServiceMultipleOutputs] = (issue: ServiceMultipleOutputs) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: has ${issue.count} output type definitions, expected exactly 1
       |""".stripMargin
  }

  implicit val serviceMultipleErrorsPrinter: IssuePrinter[ServiceMultipleErrors] = (issue: ServiceMultipleErrors) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: has ${issue.count} error type definitions, expected at most 1
       |""".stripMargin
  }

  implicit val genericTyperIssue: IssuePrinter[GenericTyperIssue] = (issue: GenericTyperIssue) => {
    s"""${extractLocation(issue.meta)}: ${issue.message}""".stripMargin
  }

  private def extractLocation(meta: RawNodeMeta): String = {
    meta.pos match {
      case full: InputPointer.Full =>
        s"""In model: ${full.file.name.theString}
           |  line=${full.start.line} position=${full.start.column}
           |  line=${full.stop.line}  position=${full.stop.column}""".stripMargin
      case offset: InputPointer.Offset =>
        s"""In model: ${offset.file.name.theString}
           |  line=${offset.start.line} position=${offset.start.column}""".stripMargin
      case file: InputPointer.JustFile =>
        s"In model: ${file.file.name.theString}"
      case InputPointer.Undefined => ""
    }
  }
}

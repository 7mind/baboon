package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.*
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.issuesUrl
import io.septimalmind.baboon.typer.model.DomainMember
import izumi.fundamentals.graphs.ToposortError
import izumi.fundamentals.platform.exceptions.IzThrowable.*
import izumi.fundamentals.platform.strings.IzString.toRichIterable

trait IssuePrinter[T <: BaboonIssue] {
  def stringify(issue: T): String
}

trait BugPrinter[T <: BaboonBug & BaboonIssue] extends IssuePrinter[T] {
  final override def stringify(issue: T): String = {
    s"""BABOON BUG!!!
       |${errorMessage(issue)}
       |Report: $issuesUrl
       |""".stripMargin
  }

  def errorMessage(bug: T): String
}

object IssuePrinter {
  private[issues] val issuesUrl = "https://github.com/7mind/baboon/issues"

  def apply[T <: BaboonIssue](
    implicit printer: IssuePrinter[T]
  ): IssuePrinter[T] = printer

  implicit val cantReadInputPrinter: IssuePrinter[CantReadInput] =
    (issue: CantReadInput) => {
      s"""Can't read form file: ${issue.path}
         |Due to: ${issue.throwable.stacktraceString}
         |""".stripMargin
    }

  implicit val cantWriteOutputPrinter: IssuePrinter[CantWriteOutput] =
    (issue: CantWriteOutput) => {
      s"""Can't write to file: $issue.path
         |Due to: ${issue.throwable.stacktraceString}
         |""".stripMargin
    }

  implicit val parserFailedPrinter: IssuePrinter[ParserFailed] =
    (issue: ParserFailed) => {
      val Array(line, character, _*) =
        issue.error.extra.input.prettyIndex(issue.error.index).split(":")
      s"Parser error occurred in ${issue.path} @ line:$line position:$character".stripMargin
    }

  implicit val includeNotFoundPrinter: IssuePrinter[IncludeNotFound] =
    (issue: IncludeNotFound) => {
      s"Failed to find inclusion `${issue.path}``".stripMargin
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
        case _: RawDto                    => "DTO"
        case _: RawEnum                   => "Enum"
        case _: RawAdt                    => "ADT"
        case _: RawForeign                => "Foreign"
        case _: RawContract               => "Contract"
        case _: RawNamespace              => "Namespace"
        case _: RawService                => "Service"
        case _: RawServiceMethodNamespace => "MethodNamespace"
      }
      s"""${extractLocation(issue.member.meta)}
         |Found an empty $memberType: ${issue.member.name.name}
         |""".stripMargin
    }

  implicit val badEnumNamePrinter: IssuePrinter[BadEnumName] =
    (issue: BadEnumName) => {
      s"""${extractLocation(issue.meta)}
         |Bad enum name: $issue.name
         |""".stripMargin
    }

  implicit val badFieldNamePrinter: IssuePrinter[BadFieldName] =
    (issue: BadFieldName) => {
      s"""${extractLocation(issue.meta)}
         |Bad field name: $issue.name
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

  implicit val brokenComparisonPrinter: IssuePrinter[BrokenComparison] =
    new BugPrinter[BrokenComparison] {
      override def errorMessage(bug: BrokenComparison): String = {
        s"""Broken comparison
           |Versions: ${bug.versions.mkString(", ")}
           |""".stripMargin
      }
    }

  implicit val unexpectedDiffTypePrinter: IssuePrinter[UnexpectedDiffType] =
    new BugPrinter[UnexpectedDiffType] {
      override def errorMessage(bug: UnexpectedDiffType): String = {
        s"""$bug.expected is expected
           |Instead provided:${bug.o.ops.niceList()}
           |""".stripMargin
      }
    }

  implicit val nonUniqueDiffPrinter: IssuePrinter[NonUniqueDiff] =
    new BugPrinter[NonUniqueDiff] {
      override def errorMessage(bug: NonUniqueDiff): String = {
        val evolutionsStr = bug.evolutions.map {
          case (evolution, differences) =>
            s"""Evolution: from${evolution.from} to ${evolution.to}
               |Differences:
               |${differences.niceList()}
               |""".stripMargin
        }
          .mkString("\n")
        s"""Non unique differences:$evolutionsStr
           |""".stripMargin
      }
    }

  implicit val nonUniqueRulesetPrinter: IssuePrinter[NonUniqueRuleset] =
    new BugPrinter[NonUniqueRuleset] {
      override def errorMessage(bug: NonUniqueRuleset): String = {
        val evolutionsStr = bug.evolutions.map {
          case (evolution, ruleset) =>
            s"""Evolution: from${evolution.from} to ${evolution.to}
               |Ruleset:${ruleset.niceList()}
               |""".stripMargin
        }
          .mkString("\n")
        s"""Non unique rule sets:$evolutionsStr
           |""".stripMargin
      }
    }

  implicit val nonUniqueVersionsPrinter: IssuePrinter[NonUniquePrevVersions] =
    new BugPrinter[NonUniquePrevVersions] {
      override def errorMessage(bug: NonUniquePrevVersions): String = {
        val versions = bug.versions.map {
          case (v, conflicts) =>
            s"""Version: $v
               |Conflicts:${conflicts.niceList()}
               |""".stripMargin
        }
          .mkString("\n")
        s"""Non unique versions in version chain:$versions
           |""".stripMargin
      }
    }

  implicit val incomparableTypedefsPrinter: IssuePrinter[IncomparableTypedefs] =
    (issue: IncomparableTypedefs) => {
      s"""In order to compare two types for evolution they should be user defined
         |Instead provided:
         |   ${issue.old.id.toString}
         |   ${issue.newMember.id.toString}
         |""".stripMargin
    }

  implicit val nonUniqueDiffsPrinter: IssuePrinter[NonUniqueDiffs] =
    new BugPrinter[NonUniqueDiffs] {
      override def errorMessage(bug: NonUniqueDiffs): String = {
        val nonUniqueDiffsStr = bug.nonUniqueDiffs.map {
          case (typeId, diffs) =>
            s"""Type:        ${typeId.name}
               |Differences:${diffs.niceList()}
               |""".stripMargin
        }
          .mkString("\n")
        s"""Non unique differences:$nonUniqueDiffsStr
           |""".stripMargin
      }
    }

  implicit val mismatchingTypedefs: IssuePrinter[MismatchingTypedefs] =
    new BugPrinter[MismatchingTypedefs] {
      override def errorMessage(bug: MismatchingTypedefs): String = {
        s"""Mismatching type defs:
           |   ${bug.o1.toString}
           |   ${bug.o2.toString}
           |""".stripMargin
      }
    }

  implicit val missingTypeDef: IssuePrinter[MissingTypeDef] =
    (issue: MissingTypeDef) => {
      s"""Model: ${issue.domain.id.toString}
         |Has missing type definitions: ${issue.missing.map(_.name.name).mkString}
         |""".stripMargin
    }

  implicit val referentialCyclesFoundPrinter: IssuePrinter[ReferentialCyclesFound] =
    (issue: ReferentialCyclesFound) => {
      val stringLoops = issue.loops.toList
        .map(
          cycle =>
            s"type: ${cycle.node.name.name} => loop: ${cycle.loops
                .flatMap(_.loop.map(_.name.name))
                .mkString("->")}"
        )
        .niceList()
      s"""Model: ${issue.domain.id.toString}
         |Referential cycles have been found:$stringLoops
         |""".stripMargin
    }

  implicit val conflictingDtoFieldsPrinter: IssuePrinter[ConflictingDtoFields] =
    (issue: ConflictingDtoFields) => {
      s"""${extractLocation(issue.meta)}
         |In DTO: ${issue.dto.name.name}
         |Conflicting fields have been found:${issue.dupes.values.flatten
          .niceList()}
         |""".stripMargin
    }

  implicit val conflictingEnumBranchesPrinter: IssuePrinter[ConflictingEnumBranches] =
    (issue: ConflictingEnumBranches) => {
      s"""${extractLocation(issue.meta)}
         |In Enum: ${issue.uEnum.id.name.name}
         |Conflicting members have been found: ${issue.dupes.values.flatten
          .map(_.name)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val conflictingAdtBranchesPrinter: IssuePrinter[ConflictingAdtBranches] =
    (issue: ConflictingAdtBranches) => {
      s"""${extractLocation(issue.meta)}
         |In ADT: ${issue.adt.id.name.name}
         |Conflicting branches have been found: ${issue.dupes.values.flatten
          .map(_.name.name)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val conflictingTypeIdsPrinter: IssuePrinter[ConflictingTypeIds] =
    (issue: ConflictingTypeIds) => {
      s"""In model: ${issue.domain.id.toString}
         |Conflicting types have been found: ${issue.dupes.values.flatten
          .map(_.id.toString)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val emptyEnumDefPrinter: IssuePrinter[EmptyEnumDef] =
    (issue: EmptyEnumDef) => {
      s"""${extractLocation(issue.meta)}
         |Empty enum: ${issue.e.id.toString}
         |""".stripMargin
    }

  implicit val emptyAdtDefPrinter: IssuePrinter[EmptyAdtDef] =
    (issue: EmptyAdtDef) => {
      s"""${extractLocation(issue.meta)}
         |Empty ADT: ${issue.a.id.toString}
         |""".stripMargin
    }

  implicit val missingEvoDiffPrinter: IssuePrinter[MissingEvoDiff] =
    new BugPrinter[MissingEvoDiff] {
      override def errorMessage(bug: MissingEvoDiff): String = {
        s"""Missing differences:${bug.missingDiffs.niceList()}
           |In model: ${bug.prev.id}
           |""".stripMargin
      }
    }

  implicit val missingEvoConversionPrinter: IssuePrinter[MissingEvoConversion] =
    new BugPrinter[MissingEvoConversion] {
      override def errorMessage(bug: MissingEvoConversion): String = {
        s"""Missing evolution conversion
           |Missing conversions:${(bug.missingConversions ++ bug.extraConversions)
            .niceList()}
           |In model:
           |   Previous version: ${bug.prev.id.toString}
           |   Next version:     ${bug.next.id.toString}
           |""".stripMargin
      }
    }

  implicit val brokenConversionPrinter: IssuePrinter[BrokenConversion] =
    new BugPrinter[BrokenConversion] {
      override def errorMessage(bug: BrokenConversion): String = {
        s"""Removed type: ${bug.c.sourceTpe.toString}
           |Is not present as removed type in conversion:
           |${bug.c.toString}
           |""".stripMargin
      }
    }

  implicit val incorrectConversionApplicationPrinter: IssuePrinter[IncorrectConversionApplication] =
    new BugPrinter[IncorrectConversionApplication] {
      override def errorMessage(bug: IncorrectConversionApplication): String = {
        val location = (bug.o, bug.n) match {
          case (old: DomainMember.User, newMember: DomainMember.User) =>
            s"""Old member location:
               |   ${extractLocation(old.meta)}
               |
               |New member location:
               |   ${extractLocation(newMember.meta)}
               |""".stripMargin
          case _ => ""
        }
        location +
        s"""Incorrect conversion: ${bug.c.toString}
           |Member:
           |   Old version: ${bug.o.id.toString}
           |   New version: ${bug.n.id.toString}
           |Issue: ${bug.issue.toString}
           |""".stripMargin
      }
    }

  implicit val pathologicGenericsPrinter: IssuePrinter[PathologicGenerics] =
    (issue: PathologicGenerics) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has nested option fields:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val setsCantContainGenericsPrinter: IssuePrinter[SetsCantContainGenerics] =
    (issue: SetsCantContainGenerics) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has set fields with generic type parameter:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val mapKeysShouldNotBeGenericPrinter: IssuePrinter[MapKeysShouldNotBeGeneric] =
    (issue: MapKeysShouldNotBeGeneric) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has map fields with generic key:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val nonUniqueOutputFilesPrinter: IssuePrinter[NonUniqueOutputFiles] =
    (issue: NonUniqueOutputFiles) => {
      s"""Non unique output files:${issue.c.niceList()}
         |""".stripMargin
    }

  implicit val incorrectRootFound: IssuePrinter[IncorrectRootFound] =
    (issue: IncorrectRootFound) => {
      s"""Incorrect root found in model: ${issue.domain.id.toString} ${issue.badRoots
          .map(_.name.name)
          .niceList()}
         |""".stripMargin
    }

  implicit val badFieldNames: IssuePrinter[BadFieldNames] = issue => {
    s"""Bad field names: ${issue.bad.niceList()}""".stripMargin
  }

  implicit val eitherAllOrNoneEnumMembersMustHaveConstants: IssuePrinter[EitherAllOrNoneEnumMembersMustHaveConstants] = issue => {
    s"""Either all or none of enum members must have associated constants in ${issue.e.id.toString}""".stripMargin
  }
  implicit val underscoredDefinitionRetained: IssuePrinter[UnderscoredDefinitionRetained] = issue => {
    s"""Underscored definitions were retained in ${issue.s.id.toString}""".stripMargin
  }
  implicit val wrongEnumConstant: IssuePrinter[WrongEnumConstant] = issue => {
    s"""Enum constants must be within Int.MinValue..Int.MaxValue range ${issue.e.id.toString}""".stripMargin
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

  implicit val baboonIssuePrinter: IssuePrinter[BaboonIssue] = {
    case issue: IOIssue           => apply[IOIssue].stringify(issue)
    case issue: ParserIssue       => apply[ParserIssue].stringify(issue)
    case issue: TyperIssue        => apply[TyperIssue].stringify(issue)
    case issue: EvolutionIssue    => apply[EvolutionIssue].stringify(issue)
    case issue: VerificationIssue => apply[VerificationIssue].stringify(issue)
    case issue: TranslationIssue  => apply[TranslationIssue].stringify(issue)
  }

  implicit val IOIssuePrinter: IssuePrinter[IOIssue] = {
    case i: CantReadInput   => apply[CantReadInput].stringify(i)
    case i: CantWriteOutput => apply[CantWriteOutput].stringify(i)
  }

  implicit val parserIssuePrinter: IssuePrinter[ParserIssue] = {
    case i: ParserFailed    => apply[ParserFailed].stringify(i)
    case i: IncludeNotFound => apply[IncludeNotFound].stringify(i)
  }

  implicit val typerIssue: IssuePrinter[TyperIssue] = {
    case i: ScalarExpected     => apply[ScalarExpected].stringify(i)
    case i: CollectionExpected => apply[CollectionExpected].stringify(i)
    case i: NonUniqueDomainVersions =>
      apply[NonUniqueDomainVersions].stringify(i)
    case i: EmptyDomainFamily     => apply[EmptyDomainFamily].stringify(i)
    case i: NonUniqueLineages     => apply[NonUniqueLineages].stringify(i)
    case i: EmptyFamily           => apply[EmptyFamily].stringify(i)
    case i: UnexpectedBuiltin     => apply[UnexpectedBuiltin].stringify(i)
    case i: UnexpectedNonBuiltin  => apply[UnexpectedNonBuiltin].stringify(i)
    case i: MissingTypeId         => apply[MissingTypeId].stringify(i)
    case i: NonUniqueEnumBranches => apply[NonUniqueEnumBranches].stringify(i)
    case i: NonUniqueForeignEntries =>
      apply[NonUniqueForeignEntries].stringify(i)
    case i: EmptyEnum             => apply[EmptyEnum].stringify(i)
    case i: NonUniqueFields       => apply[NonUniqueFields].stringify(i)
    case i: EmptyAdt              => apply[EmptyAdt].stringify(i)
    case i: EmptyGenericArgs      => apply[EmptyGenericArgs].stringify(i)
    case i: DuplicatedTypedefs    => apply[DuplicatedTypedefs].stringify(i)
    case i: EmptyPackageId        => apply[EmptyPackageId].stringify(i)
    case i: NonUniqueTypedefs     => apply[NonUniqueTypedefs].stringify(i)
    case i: NonUniqueScope        => apply[NonUniqueScope].stringify(i)
    case i: UnexpectedScoping     => apply[UnexpectedScoping].stringify(i)
    case i: ScopeCannotBeEmpty    => apply[ScopeCannotBeEmpty].stringify(i)
    case i: BadEnumName           => apply[BadEnumName].stringify(i)
    case i: BadFieldName          => apply[BadFieldName].stringify(i)
    case i: BadTypeName           => apply[BadTypeName].stringify(i)
    case i: BadInheritance        => apply[BadInheritance].stringify(i)
    case i: CircularInheritance   => apply[CircularInheritance].stringify(i)
    case i: NameNotFound          => apply[NameNotFound].stringify(i)
    case i: UnexpectedScopeLookup => apply[UnexpectedScopeLookup].stringify(i)
    case i: NamSeqeNotFound       => apply[NamSeqeNotFound].stringify(i)
    case i: DuplicatedTypes       => apply[DuplicatedTypes].stringify(i)
    case i: WrongParent           => apply[WrongParent].stringify(i)
    case i: MissingContractFields => apply[MissingContractFields].stringify(i)
    case i: TodoTyperIssue        => i.descr
    case i: ScopedRefToNamespacedGeneric =>
      apply[ScopedRefToNamespacedGeneric].stringify(i)
  }

  implicit val evolutionIssuePrinter: IssuePrinter[EvolutionIssue] = {
    case i: BrokenComparison      => apply[BrokenComparison].stringify(i)
    case i: UnexpectedDiffType    => apply[UnexpectedDiffType].stringify(i)
    case i: NonUniqueDiff         => apply[NonUniqueDiff].stringify(i)
    case i: NonUniqueRuleset      => apply[NonUniqueRuleset].stringify(i)
    case i: IncomparableTypedefs  => apply[IncomparableTypedefs].stringify(i)
    case i: NonUniqueDiffs        => apply[NonUniqueDiffs].stringify(i)
    case i: NonUniquePrevVersions => apply[NonUniquePrevVersions].stringify(i)
    case i: MismatchingTypedefs   => apply[MismatchingTypedefs].stringify(i)
  }

  implicit val verificationIssuePrinter: IssuePrinter[VerificationIssue] = {
    case i: MissingTypeDef         => apply[MissingTypeDef].stringify(i)
    case i: ReferentialCyclesFound => apply[ReferentialCyclesFound].stringify(i)
    case i: IncorrectRootFound     => apply[IncorrectRootFound].stringify(i)
    case i: ConflictingDtoFields   => apply[ConflictingDtoFields].stringify(i)
    case i: ConflictingEnumBranches =>
      apply[ConflictingEnumBranches].stringify(i)
    case i: ConflictingAdtBranches => apply[ConflictingAdtBranches].stringify(i)
    case i: ConflictingTypeIds     => apply[ConflictingTypeIds].stringify(i)
    case i: EmptyEnumDef           => apply[EmptyEnumDef].stringify(i)
    case i: EmptyAdtDef            => apply[EmptyAdtDef].stringify(i)
    case i: MissingEvoDiff         => apply[MissingEvoDiff].stringify(i)
    case i: MissingEvoConversion   => apply[MissingEvoConversion].stringify(i)
    case i: IncorrectConversionApplication =>
      apply[IncorrectConversionApplication].stringify(i)
    case i: PathologicGenerics => apply[PathologicGenerics].stringify(i)
    case i: SetsCantContainGenerics =>
      apply[SetsCantContainGenerics].stringify(i)
    case i: MapKeysShouldNotBeGeneric =>
      apply[MapKeysShouldNotBeGeneric].stringify(i)
    case i: BrokenConversion => apply[BrokenConversion].stringify(i)
    case i: BadFieldNames    => apply[BadFieldNames].stringify(i)
    case i: EitherAllOrNoneEnumMembersMustHaveConstants =>
      apply[EitherAllOrNoneEnumMembersMustHaveConstants].stringify(i)
    case i: UnderscoredDefinitionRetained =>
      apply[UnderscoredDefinitionRetained].stringify(i)
    case i: WrongEnumConstant => apply[WrongEnumConstant].stringify(i)

  }

  implicit val translationIssuePrinter: IssuePrinter[TranslationIssue] = {
    case i: NonUniqueOutputFiles => apply[NonUniqueOutputFiles].stringify(i)
    case i: TranslationBug       => i.toString()
  }

  implicit class IssuePrinterOps[T <: BaboonIssue: IssuePrinter](issue: T) {
    def stringify: String = apply[T].stringify(issue)
  }

  implicit class IssuePrinterListOps[T <: BaboonIssue: IssuePrinter](
    issues: Seq[T]
  ) {
    def stringifyIssues: String = {
      issues.map(_.stringify).mkString("\n")
    }
  }
}

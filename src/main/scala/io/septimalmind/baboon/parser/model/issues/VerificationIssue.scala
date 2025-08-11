package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait VerificationIssue extends IssueGroup

object VerificationIssue {
  implicit def wrap(issue: VerificationIssue): BaboonIssue = BaboonIssue.Verification(issue)

  case class LockedVersionModified(pkg: Pkg, version: Version) extends VerificationIssue

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

  implicit val verificationIssuePrinter: IssuePrinter[VerificationIssue] = {
    case i: LockedVersionModified                       => IssuePrinter[LockedVersionModified].stringify(i)
    case i: MissingTypeDef                              => IssuePrinter[MissingTypeDef].stringify(i)
    case i: ReferentialCyclesFound                      => IssuePrinter[ReferentialCyclesFound].stringify(i)
    case i: IncorrectRootFound                          => IssuePrinter[IncorrectRootFound].stringify(i)
    case i: ConflictingDtoFields                        => IssuePrinter[ConflictingDtoFields].stringify(i)
    case i: ConflictingEnumBranches                     => IssuePrinter[ConflictingEnumBranches].stringify(i)
    case i: ConflictingAdtBranches                      => IssuePrinter[ConflictingAdtBranches].stringify(i)
    case i: ConflictingTypeIds                          => IssuePrinter[ConflictingTypeIds].stringify(i)
    case i: EmptyEnumDef                                => IssuePrinter[EmptyEnumDef].stringify(i)
    case i: EmptyAdtDef                                 => IssuePrinter[EmptyAdtDef].stringify(i)
    case i: PathologicGenerics                          => IssuePrinter[PathologicGenerics].stringify(i)
    case i: SetsCantContainGenerics                     => IssuePrinter[SetsCantContainGenerics].stringify(i)
    case i: MapKeysShouldNotBeGeneric                   => IssuePrinter[MapKeysShouldNotBeGeneric].stringify(i)
    case i: BadFieldNames                               => IssuePrinter[BadFieldNames].stringify(i)
    case i: EitherAllOrNoneEnumMembersMustHaveConstants => IssuePrinter[EitherAllOrNoneEnumMembersMustHaveConstants].stringify(i)
    case i: UnderscoredDefinitionRetained               => IssuePrinter[UnderscoredDefinitionRetained].stringify(i)
    case i: WrongEnumConstant                           => IssuePrinter[WrongEnumConstant].stringify(i)
    // BaboonBug cases
    case i: MissingEvoDiff                 => IssuePrinter[MissingEvoDiff].stringify(i)
    case i: MissingEvoConversion           => IssuePrinter[MissingEvoConversion].stringify(i)
    case i: IncorrectConversionApplication => IssuePrinter[IncorrectConversionApplication].stringify(i)
    case i: BrokenConversion               => IssuePrinter[BrokenConversion].stringify(i)
  }

  implicit val lockedVersionModifiedPrinter: IssuePrinter[LockedVersionModified] =
    (issue: LockedVersionModified) => {
      s"""Model ${issue.pkg.toString}@${issue.version} was modified but it's not the latest version so it's locked with the lockfile""".stripMargin
    }

  implicit val missingTypeDefPrinter: IssuePrinter[MissingTypeDef] =
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

  implicit val incorrectRootFoundPrinter: IssuePrinter[IncorrectRootFound] =
    (issue: IncorrectRootFound) => {
      s"""Incorrect root found in model: ${issue.domain.id.toString} ${issue.badRoots
          .map(_.name.name)
          .niceList()}
         |""".stripMargin
    }

  implicit val badFieldNamesPrinter: IssuePrinter[BadFieldNames] = issue => {
    s"""Bad field names: ${issue.bad.niceList()}""".stripMargin
  }

  implicit val eitherAllOrNoneEnumMembersMustHaveConstantsPrinter: IssuePrinter[EitherAllOrNoneEnumMembersMustHaveConstants] = issue => {
    s"""Either all or none of enum members must have associated constants in ${issue.e.id.toString}""".stripMargin
  }

  implicit val underscoredDefinitionRetainedPrinter: IssuePrinter[UnderscoredDefinitionRetained] = issue => {
    s"""Underscored definitions were retained in ${issue.s.id.toString}""".stripMargin
  }

  implicit val wrongEnumConstantPrinter: IssuePrinter[WrongEnumConstant] = issue => {
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
}

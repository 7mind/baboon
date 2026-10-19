package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait EvolutionIssue extends IssueGroup

object EvolutionIssue {
  implicit def wrap(issue: EvolutionIssue): BaboonIssue = BaboonIssue.Evolution(issue)

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

  /** A type id names a different kind of type (`data` -> `enum`, ...) in the next version. */
  case class TypeKindChanged(step: EvolutionStep, prev: Typedef.User, next: Typedef.User) extends EvolutionIssue

  /** Conversions copy foreign values as they are, so a language's binding cannot change in place. */
  case class ForeignBindingChanged(step: EvolutionStep, id: TypeId.User, prev: Typedef.ForeignEntry, next: Typedef.ForeignEntry) extends EvolutionIssue

  case class InvalidFieldRename(typeId: TypeId, newFieldName: FieldName, prevFieldName: FieldName) extends EvolutionIssue

  case class InvalidEnumMemberRename(typeId: TypeId, newMemberName: String, prevMemberName: String) extends EvolutionIssue

  /** The `was` clause names a type no earlier version of the package ever defined. */
  case class InvalidTypeRename(typeId: TypeId, prevTypeId: TypeId) extends EvolutionIssue

  /** The `was` clause names a type this version still defines, so nothing was renamed away. */
  case class RenameSourceStillPresent(typeId: TypeId, prevTypeId: TypeId) extends EvolutionIssue

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
        s"""Non unique diffs found
           |$evolutionsStr
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

  implicit val typeKindChangedPrinter: IssuePrinter[TypeKindChanged] =
    (issue: TypeKindChanged) => {
      s"""Type ${issue.next.id.toString} is declared `${kindOf(issue.prev)}` in ${issue.step.from} but `${kindOf(issue.next)}` in ${issue.step.to}.
         |   A type cannot change its kind between versions: give the new `${kindOf(issue.next)}` another name,
         |   and remove the old `${kindOf(issue.prev)}` or keep it alongside.
         |""".stripMargin
    }

  implicit val foreignBindingChangedPrinter: IssuePrinter[ForeignBindingChanged] =
    (issue: ForeignBindingChanged) => {
      s"""Foreign type ${issue.id.toString} changes its ${issue.prev.lang.asString} binding between ${issue.step.from} and ${issue.step.to}:
         |   ${bindingOf(issue.prev)} -> ${bindingOf(issue.next)}
         |   Conversions copy foreign values as they are, so a binding cannot change in place; declare a new
         |   foreign type for the new representation. `rt` may change, and bindings may be added or removed.
         |""".stripMargin
    }

  private def kindOf(defn: Typedef.User): String = defn match {
    case d: Typedef.Dto if d.isIdentifier => "id"
    case _: Typedef.Dto                   => "data"
    case _: Typedef.Adt                   => "adt"
    case _: Typedef.Enum                  => "enum"
    case _: Typedef.Foreign               => "foreign"
    case _: Typedef.Contract              => "contract"
    case _: Typedef.Service               => "service"
  }

  private def bindingOf(entry: Typedef.ForeignEntry): String = entry.mapping match {
    case Typedef.ForeignMapping.Custom(decl, attrs) =>
      val attributes = if (attrs.attrs.isEmpty) "" else attrs.attrs.map(a => s"\"${a.name}\" = \"${a.value}\"").mkString(" with { ", " ", " }")
      s"\"$decl\"$attributes"
    case Typedef.ForeignMapping.BaboonRef(typeRef) => typeRef.toString
  }

  implicit val invalidFieldRenamePrinter: IssuePrinter[InvalidFieldRename] =
    (issue: InvalidFieldRename) => {
      s"""Invalid field rename in type ${issue.typeId.toString}:
         |   Field '${issue.newFieldName.name}' was declared as renamed from '${issue.prevFieldName.name}',
         |   but no earlier version of this type has a field named '${issue.prevFieldName.name}'.
         |""".stripMargin
    }

  implicit val invalidTypeRenamePrinter: IssuePrinter[InvalidTypeRename] =
    (issue: InvalidTypeRename) => {
      s"""Invalid type rename: ${issue.typeId.toString}
         |   was declared as renamed from ${issue.prevTypeId.toString},
         |   but no earlier version of this package defines that type.
         |""".stripMargin
    }

  implicit val renameSourceStillPresentPrinter: IssuePrinter[RenameSourceStillPresent] =
    (issue: RenameSourceStillPresent) => {
      s"""Invalid type rename: ${issue.typeId.toString}
         |   was declared as renamed from ${issue.prevTypeId.toString},
         |   but that type still exists in this version.
         |   A rename moves a type off its old identity; declare the old type as removed,
         |   or point the `was` clause at a type this version no longer defines.
         |""".stripMargin
    }

  implicit val invalidEnumMemberRenamePrinter: IssuePrinter[InvalidEnumMemberRename] =
    (issue: InvalidEnumMemberRename) => {
      s"""Invalid enum member rename in type ${issue.typeId.toString}:
         |   Member '${issue.newMemberName}' was declared as renamed from '${issue.prevMemberName}',
         |   but no earlier version of this type has a member named '${issue.prevMemberName}'.
         |""".stripMargin
    }
}

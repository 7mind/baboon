package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.IzCollections.*
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

  case class MismatchingTypedefs(o1: Typedef.User, o2: Typedef.User) extends EvolutionIssue with BaboonBug

  implicit val evolutionIssuePrinter: IssuePrinter[EvolutionIssue] = {
    case issue: BrokenComparison       => brokenComparisonPrinter.stringify(issue)
    case issue: UnexpectedDiffType      => unexpectedDiffTypePrinter.stringify(issue)
    case issue: NonUniqueDiff           => nonUniqueDiffPrinter.stringify(issue)
    case issue: NonUniqueRuleset        => nonUniqueRulesetPrinter.stringify(issue)
    case issue: NonUniquePrevVersions   => nonUniqueVersionsPrinter.stringify(issue)
    case issue: IncomparableTypedefs    => incomparableTypedefsPrinter.stringify(issue)
    case issue: NonUniqueDiffs          => nonUniqueDiffsPrinter.stringify(issue)
    case issue: MismatchingTypedefs     => mismatchingTypedefsPrinter.stringify(issue)
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

  implicit val mismatchingTypedefsPrinter: IssuePrinter[MismatchingTypedefs] =
    new BugPrinter[MismatchingTypedefs] {
      override def errorMessage(bug: MismatchingTypedefs): String = {
        s"""Mismatching type defs:
           |   ${bug.o1.toString}
           |   ${bug.o2.toString}
           |""".stripMargin
      }
    }
}

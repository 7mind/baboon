package io.septimalmind.baboon.parser.model.issues

trait BaboonBug {
  this: IssueGroup =>
}

sealed trait BaboonIssue

object BaboonIssue {

  case class Parser(issue: ParserIssue) extends BaboonIssue

  case class IO(issue: IOIssue) extends BaboonIssue

  case class Verification(issue: VerificationIssue) extends BaboonIssue

  case class Typer(issue: TyperIssue) extends BaboonIssue

  case class Translation(issue: TranslationIssue) extends BaboonIssue

  case class Evolution(issue: EvolutionIssue) extends BaboonIssue

  implicit val wTranslationIssuePrinter: IssuePrinter[Translation] = {
    case i: Translation => IssuePrinter[TranslationIssue].stringify(i.issue)
  }

  implicit val wParserIssuePrinter: IssuePrinter[Parser] = {
    case i: Parser => IssuePrinter[ParserIssue].stringify(i.issue)
  }

  implicit val wIOIssuePrinter: IssuePrinter[IO] = {
    case i: IO => IssuePrinter[IOIssue].stringify(i.issue)
  }

  implicit val wVerificationIssuePrinter: IssuePrinter[Verification] = {
    case i: Verification => IssuePrinter[VerificationIssue].stringify(i.issue)
  }

  implicit val wTyperIssuePrinter: IssuePrinter[Typer] = {
    case i: Typer => IssuePrinter[TyperIssue].stringify(i.issue)
  }

  implicit val wEvolutionIssuePrinter: IssuePrinter[Evolution] = {
    case i: Evolution => IssuePrinter[EvolutionIssue].stringify(i.issue)
  }

}

package io.septimalmind.baboon.parser.model.issues

import izumi.fundamentals.collections.nonempty.NEList

trait BaboonBug {
  this: IssueGroup =>
}

sealed trait BaboonIssue

object BaboonIssue {
  trait Wrap[-T] {
    def wrap(issue: T): BaboonIssue
  }

  case class Parser(issue: ParserIssue) extends BaboonIssue

  case class IO(issue: IOIssue) extends BaboonIssue

  case class Verification(issue: VerificationIssue) extends BaboonIssue

  case class Typer(issue: TyperIssue) extends BaboonIssue

  case class Translation(issue: TranslationIssue) extends BaboonIssue

  case class Evolution(issue: EvolutionIssue) extends BaboonIssue

  case class RuntimeCodec(issue: RuntimeCodecIssue) extends BaboonIssue

  def of[T](issue: T, more: T*)(implicit w: Wrap[T]): NEList[BaboonIssue] = NEList(w.wrap(issue), more.map(w.wrap))

  implicit object WEvolutionIssue extends Wrap[EvolutionIssue] {
    override def wrap(issue: EvolutionIssue): BaboonIssue = EvolutionIssue.wrap(issue)
  }

  implicit object WParserIssue extends Wrap[ParserIssue] {
    override def wrap(issue: ParserIssue): BaboonIssue = ParserIssue.wrap(issue)
  }

  implicit object WIOIssue extends Wrap[IOIssue] {
    override def wrap(issue: IOIssue): BaboonIssue = IOIssue.wrap(issue)
  }

  implicit object WVerificationIssue extends Wrap[VerificationIssue] {
    override def wrap(issue: VerificationIssue): BaboonIssue = VerificationIssue.wrap(issue)
  }

  implicit object WTyperIssue extends Wrap[TyperIssue] {
    override def wrap(issue: TyperIssue): BaboonIssue = TyperIssue.wrap(issue)
  }

  implicit object WTranslationIssue extends Wrap[TranslationIssue] {
    override def wrap(issue: TranslationIssue): BaboonIssue = TranslationIssue.wrap(issue)
  }

  implicit object WRuntimeCodecIssue extends Wrap[RuntimeCodecIssue] {
    override def wrap(issue: RuntimeCodecIssue): BaboonIssue = RuntimeCodecIssue.wrap(issue)
  }

  implicit val wTranslationIssuePrinter: IssuePrinter[Translation] = {
    case i: Translation => IssuePrinter[TranslationIssue].stringify(i.issue)
  }

  implicit val wRuntimeCodecIssuePrinter: IssuePrinter[RuntimeCodec] = {
    case i: RuntimeCodec => IssuePrinter[RuntimeCodecIssue].stringify(i.issue)
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

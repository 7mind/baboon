package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed

sealed trait BaboonIssue

object BaboonIssue {
  sealed trait ParserIssue extends BaboonIssue
  case class ParserFailed(fail: Parsed.Failure) extends ParserIssue

  sealed trait TyperIssue extends BaboonIssue
  case class TODOIssue() extends TyperIssue

  sealed trait VerificationIssue extends BaboonIssue
  sealed trait TranslationIssue extends BaboonIssue
}

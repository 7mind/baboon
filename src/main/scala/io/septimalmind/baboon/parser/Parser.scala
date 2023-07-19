package io.septimalmind.baboon.parser

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.RawDomain
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait BaboonParser {
  def parse(): Either[NonEmptyList[BaboonIssue.ParserIssue], RawDomain]
}

class Parser(context: ParserContext) extends BaboonParser {
  def parse(): Either[NonEmptyList[BaboonIssue.ParserIssue], RawDomain] = {
    fastparse.parse(context.content, context.defModel.model(_)) match {
      case Parsed.Success(value, _) =>
        Right(value)
      case failure: Parsed.Failure =>
        Left(NonEmptyList(BaboonIssue.ParserFailed(failure)))
    }
  }
}

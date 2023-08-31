package io.septimalmind.baboon.parser

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain}
import izumi.fundamentals.collections.nonempty.NEList

trait BaboonParser {
  def parse(
    input: BaboonParser.Input
  ): Either[NEList[BaboonIssue.ParserIssue], RawDomain]
}

object BaboonParser {
  case class Input(path: FSPath, content: String)

  class BaboonParserImpl() extends BaboonParser {
    def parse(
      input: BaboonParser.Input
    ): Either[NEList[BaboonIssue.ParserIssue], RawDomain] = {
      val context = ParserContext(input.path, input.content)
      fastparse.parse(context.content, context.defModel.model(_)) match {
        case Parsed.Success(value, _) =>
          Right(value)
        case failure: Parsed.Failure =>
          Left(NEList(BaboonIssue.ParserFailed(failure)))
      }
    }
  }

}

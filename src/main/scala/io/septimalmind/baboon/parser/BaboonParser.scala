package io.septimalmind.baboon.parser

import distage.Id
import fastparse.Parsed
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{
  FSPath,
  RawDomain,
  RawInclude,
  RawTLDef
}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.functional.IzEither.*
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path

trait BaboonParser {
  def parse(input: BaboonParser.Input): Either[NEList[BaboonIssue], RawDomain]
}

object BaboonParser {
  case class Input(path: FSPath, content: String)

  class BaboonParserImpl(inputs: Seq[Path] @Id("inputs")) extends BaboonParser {
    def parse(
      input: BaboonParser.Input
    ): Either[NEList[BaboonIssue], RawDomain] = {
      val context = ParserContext(input.path, input.content)

      fastparse.parse(context.content, context.defModel.model(_)) match {
        case Parsed.Success(value, _) =>
          for {
            included <- processIncludes(value.members.includes)
          } yield {
            value.copy(
              members = value.members.copy(
                includes = Seq.empty,
                defs = included ++ value.members.defs
              )
            )
          }

        case failure: Parsed.Failure =>
          Left(NEList(BaboonIssue.ParserFailed(failure)))
      }
    }

    def processIncludes(
      includes: Seq[RawInclude]
    ): Either[NEList[BaboonIssue], Seq[RawTLDef]] = {
      if (includes.nonEmpty) {
        includes.map { inc =>
          val inclusion = inputs
            .map(_.resolve(inc.value).toFile)
            .find(f => f.exists() && f.isFile)
          inclusion match {
            case Some(incFile) =>
              val content = IzFiles.readString(incFile)
              val context = ParserContext(
                FSPath.parse(NEString.unsafeFrom(incFile.getCanonicalPath)),
                content
              )
              fastparse.parse(context.content, context.defModel.content(_)) match {
                case Parsed.Success(value, _) =>
                  for {
                    sub <- processIncludes(value.includes)
                  } yield {
                    sub ++ value.defs
                  }
                case failure: Parsed.Failure =>
                  Left(NEList(BaboonIssue.ParserFailed(failure)))

              }

            case None =>
              Left(NEList(BaboonIssue.IncludeNotFound(inc.value)))
          }
        }.biFlatten
      } else {
        Right(Seq.empty)
      }
    }
  }

}

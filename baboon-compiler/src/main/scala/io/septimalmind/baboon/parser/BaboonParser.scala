package io.septimalmind.baboon.parser

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain, RawInclude, RawTLDef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}

trait BaboonParser[F[+_, +_]] {
  def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain]
}

trait BaboonInclusionResolver[F[+_, +_]] {
  def getIclusionContent(inc: RawInclude): Option[String]
}

object BaboonParser {
  case class Input(path: FSPath, content: String)

  sealed trait ReloadInput {
    def path: FSPath
  }
  object ReloadInput {
    case class Unparsed(path: FSPath, content: String) extends ReloadInput
    case class Parsed(path: FSPath, content: RawDomain) extends ReloadInput
  }

  class BaboonParserImpl[F[+_, +_]: Error2](
    resolver: BaboonInclusionResolver[F]
  ) extends BaboonParser[F] {

    def parse(
      input: BaboonParser.Input
    ): F[NEList[BaboonIssue], RawDomain] = {
      val context = ParserContext(input.path, input.content)

      fastparse.parse(context.content, context.defModel.model(_)) match {
        case Parsed.Success(value, _) =>
          for {
            included <- processIncludes(value.members.includes)
          } yield {
            value.copy(
              members = value.members.copy(
                includes = Seq.empty,
                defs     = included ++ value.members.defs,
              )
            )
          }

        case failure: Parsed.Failure =>
          F.fail(NEList(ParserIssue.ParserFailed(failure, input.path)))
      }
    }

    def processIncludes(
      includes: Seq[RawInclude]
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      if (includes.nonEmpty) {
        F.flatTraverseAccumErrors(includes) {
          inc =>
            // indiv
            val content = resolver.getIclusionContent(inc)
            content match {
              case Some(content) =>
                val context = ParserContext(
                  FSPath.parse(NEString.unsafeFrom(inc.value)),
                  content,
                )
                fastparse.parse(context.content, context.defModel.contentEof(_)) match {
                  case Parsed.Success(value, _) =>
                    for {
                      sub <- processIncludes(value.includes)
                    } yield {
                      sub ++ value.defs
                    }
                  case failure: Parsed.Failure =>
                    F.fail(NEList(ParserIssue.ParserFailed(failure, context.file)))

                }

              case None =>
                F.fail(NEList(ParserIssue.IncludeNotFound(inc.value)))
            }
        }
      } else {
        F.pure(Seq.empty)
      }
    }

  }

}

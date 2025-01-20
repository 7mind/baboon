package io.septimalmind.baboon.parser

import distage.Id
import fastparse.Parsed
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain, RawInclude, RawTLDef}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.Path

trait BaboonParser[F[+_, +_]] {
  def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain]
}

object BaboonParser {
  case class Input(path: FSPath, content: String)

  class BaboonParserImpl[F[+_, +_]: Error2](
    inputs: Seq[Path] @Id("inputs")
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
          F.fail(NEList(BaboonIssue.ParserFailed(failure, input.path)))
      }
    }

    def processIncludes(
      includes: Seq[RawInclude]
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      if (includes.nonEmpty) {
        F.flatTraverseAccumErrors(includes) {
          inc =>
            val inclusion = inputs
              .map(_.resolve(inc.value).toFile)
              .find(f => f.exists() && f.isFile)
            inclusion match {
              case Some(incFile) =>
                val content = IzFiles.readString(incFile)
                val context = ParserContext(
                  FSPath.parse(NEString.unsafeFrom(incFile.getCanonicalPath)),
                  content,
                )
                fastparse.parse(context.content, context.defModel.content(_)) match {
                  case Parsed.Success(value, _) =>
                    for {
                      sub <- processIncludes(value.includes)
                    } yield {
                      sub ++ value.defs
                    }
                  case failure: Parsed.Failure =>
                    F.fail(NEList(BaboonIssue.ParserFailed(failure, context.file)))

                }

              case None =>
                F.fail(NEList(BaboonIssue.IncludeNotFound(inc.value)))
            }
        }
      } else {
        F.pure(Seq.empty)
      }
    }

  }

}

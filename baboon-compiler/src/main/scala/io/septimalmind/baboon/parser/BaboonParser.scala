package io.septimalmind.baboon.parser

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain, RawInclude, RawTLDef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

trait BaboonParser[F[+_, +_]] {
  def parse(input: BaboonParser.Input): F[NEList[BaboonIssue], RawDomain]
}

trait BaboonInclusionResolver[F[+_, +_]] {
  /** `inc`, written in `includedFrom`: relative to that file's directory first, then against the model roots. */
  def resolveInclude(inc: RawInclude, includedFrom: FSPath): Either[ParserIssue, (FSPath, String)]
}

object BaboonInclusionResolver {
  private val Separator = "/"

  /** The include policy shared by every resolver: a file next to the including one wins; otherwise
    * exactly one model root must hold the path, since several would make the choice arbitrary.
    *
    * @param read the file at a candidate location, if there is one
    */
  def resolve[P](inc: RawInclude, relative: Option[P], roots: List[P])(read: P => Option[(FSPath, String)], show: P => String): Either[ParserIssue, (FSPath, String)] = {
    relative.flatMap(read) match {
      case Some(found) => Right(found)
      case None =>
        roots.flatMap(read).distinctBy(_._1) match {
          case List(found) => Right(found)
          case Nil         => Left(ParserIssue.IncludeNotFound(inc, (relative.toList ++ roots).map(show).distinct))
          case several     => Left(ParserIssue.IncludeAmbiguous(inc, several.map(_._1.asString).sorted))
        }
    }
  }

  /** `include` taken against the `/`-separated directory `base` (absolute includes ignore it), with
    * `.` and `..` folded; `None` when `..` climbs above the start of the path.
    */
  def against(base: String, include: String): Option[String] = {
    val joined   = if (include.startsWith(Separator) || base.isEmpty) include else s"$base$Separator$include"
    val absolute = joined.startsWith(Separator)
    joined
      .split(Separator).toList.filterNot(segment => segment.isEmpty || segment == ".")
      .foldLeft(Option(List.empty[String])) {
        case (Some(_ :: parent), "..") => Some(parent)
        case (Some(Nil), "..")         => None
        case (Some(stack), segment)    => Some(segment :: stack)
        case (None, _)                 => None
      }
      .filter(_.nonEmpty)
      .map(segments => (if (absolute) Separator else "") + segments.reverse.mkString(Separator))
  }

  /** The `/`-separated directory holding `file`; empty for a bare name. */
  def directoryOf(file: FSPath): String = file match {
    case FSPath.Name(_) => ""
    case other          => other.asString.stripSuffix(s"$Separator${other.name.theString}")
  }
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
            included <- processIncludes(value.members.includes, input.path)
          } yield {
            value.copy(
              members = value.members.copy(
                includes = Seq.empty,
                defs     = included ++ value.members.defs,
              )
            )
          }

        case failure: Parsed.Failure =>
          // Stacked-doc detection (PR-30.2) sets a side-channel on the
          // ParserContext just before failing the parse. Surface it as a
          // typed `StackedDocComments(pos)` instead of a generic
          // `ParserFailed`.
          context.stackedDocAt match {
            case Some(pos) =>
              F.fail(NEList(ParserIssue.StackedDocComments(pos)))
            case None =>
              F.fail(NEList(ParserIssue.ParserFailed(failure, input.path)))
          }
      }
    }

    def processIncludes(
      includes: Seq[RawInclude],
      includedFrom: FSPath,
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      if (includes.nonEmpty) {
        F.flatTraverseAccumErrors(includes) {
          inc =>
            resolver.resolveInclude(inc, includedFrom) match {
              case Right((file, content)) =>
                val context = ParserContext(
                  file,
                  content,
                )
                fastparse.parse(context.content, context.defModel.contentEof(_)) match {
                  case Parsed.Success(value, _) =>
                    for {
                      sub <- processIncludes(value.includes, file)
                    } yield {
                      sub ++ value.defs
                    }
                  case failure: Parsed.Failure =>
                    context.stackedDocAt match {
                      case Some(pos) =>
                        F.fail(NEList(ParserIssue.StackedDocComments(pos)))
                      case None =>
                        F.fail(NEList(ParserIssue.ParserFailed(failure, context.file)))
                    }
                }

              case Left(issue) =>
                F.fail(NEList(issue))
            }
        }
      } else {
        F.pure(Seq.empty)
      }
    }

  }

}

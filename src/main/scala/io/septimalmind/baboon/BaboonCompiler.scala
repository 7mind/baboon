package io.septimalmind.baboon

import io.circe.{Encoder, Json, KeyEncoder}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler {
  def run(inputs: Set[Path],
          output: Path,
          testOutput: Option[Path]): Either[NEList[BaboonIssue], Unit]
}

object BaboonDomainCodecs {
  import io.septimalmind.baboon.typer.model.*

  implicit lazy val versonKeyEncoder: KeyEncoder[Version] =
    KeyEncoder.encodeKeyString.contramap(_.version)
  implicit lazy val versonEncoder: Encoder[Version] =
    Encoder.encodeString.contramap(_.version)
  implicit lazy val pkgKeyEncoder: KeyEncoder[Pkg] =
    KeyEncoder.encodeKeyString.contramap(_.toString)
  implicit lazy val typeIdKeyEncoder: KeyEncoder[TypeId] =
    KeyEncoder.encodeKeyString.contramap(_.toString)

//  implicit lazy val unmodifiedMetaencoder
//    : Encoder[Map[Version, Map[TypeId, Version]]] = implicitly
}

object BaboonCompiler {
  case class CompilerOptions(debug: Boolean,
                             obsoleteErrors: Boolean,
                             runtime: RuntimeGenOpt,
                             generateConversions: Boolean,
                             disregardImplicitUsings: Boolean,
                             omitMostRecentVersionSuffixFromPaths: Boolean,
                             omitMostRecentVersionSuffixFromNamespaces: Boolean,
                             csUseCompactAdtForm: Boolean,
                             csWrappedAdtBranchCodecs: Boolean,
                             metaWriteEvolutionJsonTo: Option[Path],
                             csWriteEvolutionDict: Boolean,
  )

  class BaboonCompilerImpl(loader: BaboonLoader,
                           translator: CSBaboonTranslator,
                           options: CompilerOptions,
                           logger: BLogger,
  ) extends BaboonCompiler {
    override def run(inputs: Set[Path],
                     output: Path,
                     testOutput: Option[Path],
    ): Either[NEList[BaboonIssue], Unit] = {
      for {
        loaded <- loader.load(inputs.toList)
        _ <- Right(options.metaWriteEvolutionJsonTo.map { path =>
          //val f = path.toFile
          import BaboonDomainCodecs.*
          import io.circe.syntax.*
          import io.circe.generic.auto.*
          path.getParent.toFile.mkdirs()

          val out =
            Json.obj(("unmodified" -> Json.obj(loaded.domains.toSeq.map {
              case (pkg, line) =>
                (pkg.toString, line.evolution.typesUnchangedSince.asJson)
            } *)))
          val result = out.toString()
          Files.writeString(
            path,
            result,
            StandardOpenOption.WRITE,
            StandardOpenOption.TRUNCATE_EXISTING,
            StandardOpenOption.CREATE
          )

        })
        translated <- translator.translate(loaded)
        _ <- translated.files.map {
          case (p, content) =>
            Try {
              (testOutput, content.isTest) match {
                case (Some(value), true) =>
                  val tgt = value.resolve(p)
                  writeFile(content, tgt)
                case (None, true) =>
                  ()
                case (_, false) =>
                  val tgt = output.resolve(p)
                  writeFile(content, tgt)
              }

            }.toEither.left
              .map(t => NEList(BaboonIssue.CantWriteOutput(p, t)))
        }.biSequence_
      } yield {}
    }

    private def writeFile(content: OutputFile, tgt: Path): Unit = {
      tgt.getParent.toFile.mkdirs()

      if (options.debug) {
        logger.message("debug", q"$tgt\n$content")
      }

      Files.writeString(
        tgt,
        content.content,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      ()
    }
  }

}

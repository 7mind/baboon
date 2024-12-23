package io.septimalmind.baboon

import io.circe.{Encoder, Json, KeyEncoder}
import io.septimalmind.baboon.BaboonCompiler.CompilerTargets
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.translator.csharp.{
  CSBaboonTranslator,
  VersionMeta
}
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler {
  def run(inputs: Set[Path],
          targets: CompilerTargets): Either[NEList[BaboonIssue], Unit]
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
}

object BaboonCompiler {
  final case class CompilerTargets(output: Path, testOutput: Option[Path])

  class BaboonCompilerImpl(loader: BaboonLoader,
                           translator: CSBaboonTranslator,
                           options: CompilerOptions,
                           logger: BLogger,
  ) extends BaboonCompiler {
    override def run(
      inputs: Set[Path],
      targets: CompilerTargets
    ): Either[NEList[BaboonIssue], Unit] = {
      for {
        loaded <- loader.load(inputs.toList)
        _ <- Right(options.csOptions.generic.metaWriteEvolutionJsonTo.map {
          maybePath =>
            val path = Option(maybePath.getParent) match {
              case Some(_) => maybePath
              case None    => targets.output.resolve(maybePath)
            }
            import BaboonDomainCodecs.*
            import io.circe.syntax.*
            path.getParent.toFile.mkdirs()

            val data = loaded.domains.toSeq.flatMap {
              case (_, lineage) =>
                lineage.versions.toSeq.map {
                  case (ver, _) =>
                    VersionMeta(lineage.pkg.path.mkString("."), ver.version)
                }

            }

            val out =
              Json.obj(
                "versions" -> data.asJson,
                "unmodified" -> Json.obj(loaded.domains.toSeq.map {
                  case (pkg, line) =>
                    (pkg.toString, line.evolution.typesUnchangedSince.asJson)
                } *)
              )

            val result = out.toString()
            Files.writeString(
              path,
              result,
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.CREATE
            )

        })
        translated <- translator.translate(targets, loaded)
        _ <- translated.files.map {
          case (p, content) =>
            Try {
              if (content.isTest) {
                targets.testOutput.foreach { testTarget =>
                  val tgt = testTarget.resolve(p)
                  writeFile(content, tgt)
                }
              } else {
                val tgt = targets.output.resolve(p)
                writeFile(content, tgt)
              }
            }.toEither.left.map(t => NEList(BaboonIssue.CantWriteOutput(p, t)))
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

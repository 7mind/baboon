package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler {
  def run(inputs: Set[Path]): Either[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  class BaboonCompilerImpl(
    loader: BaboonLoader,
    translator: CSBaboonTranslator,
    options: CompilerOptions,
    logger: BLogger,
    metagen: BaboonMetagen,
  ) extends BaboonCompiler {
    override def run(inputs: Set[Path]): Either[NEList[BaboonIssue], Unit] = {
      for {
        loaded <- loader.load(inputs.toList)
        _ <- Right(options.generic.metaWriteEvolutionJsonTo.map {
          maybePath =>
            val path = Option(maybePath.getParent) match {
              case Some(_) => maybePath
              case None    => options.target.output.resolve(maybePath)
            }

            path.getParent.toFile.mkdirs()

            val result = metagen.meta(loaded).toString()

            Files.writeString(
              path,
              result,
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.CREATE,
            )

        })
        translated <- translator.translate(loaded)
        _ <- translated.files.map {
          case (relativePath, output) =>
            Try {
              options.target.targetPathFor(output).foreach {
                targetDirectory =>
                  val targetPath = targetDirectory.resolve(relativePath)
                  writeFile(output, targetPath)
              }
            }.toEither.left.map(t => NEList(BaboonIssue.CantWriteOutput(relativePath, t)))
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
        StandardOpenOption.TRUNCATE_EXISTING,
      )
      ()
    }
  }

}

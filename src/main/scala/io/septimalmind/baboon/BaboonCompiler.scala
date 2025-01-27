package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler[F[+_, +_]] {
  def run(inputs: Set[Path]): F[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  class BaboonCompilerImpl[F[+_, +_]: Error2](
    loader: BaboonLoader[F],
    translator: CSBaboonTranslator[F],
    options: CompilerOptions,
    logger: BLogger,
    metagen: BaboonMetagen,
  ) extends BaboonCompiler[F] {

    override def run(inputs: Set[Path]): F[NEList[BaboonIssue], Unit] = {
      for {
        loaded <- loader.load(inputs.toList)
        // io
        _ = options.generic.metaWriteEvolutionJsonTo.foreach {
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
        }
        translated <- translator.translate(loaded)
        // io
        _ <- F.traverseAccumErrors_(translated.files.iterator) {
          case (relativePath, output) =>
            F.fromTry(Try {
              options.target.targetPathFor(output).foreach {
                targetDirectory =>
                  val targetPath = targetDirectory.resolve(relativePath)
                  writeFile(output, targetPath)
              }
            }).leftMap(t => NEList(BaboonIssue.CantWriteOutput(relativePath, t)))
        }
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

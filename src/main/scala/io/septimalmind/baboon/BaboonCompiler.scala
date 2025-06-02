package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.CantCleanupTarget
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, OutputFile}
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.{BLogger, BaboonMetagen}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.strings.TextTree.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.Try

trait BaboonCompiler[F[+_, +_]] {
  def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  class BaboonCompilerImpl[F[+_, +_]: Error2]( // dirty, I/O happens there
    translator: BaboonAbstractTranslator[F],
    options: CompilerOptions,
    logger: BLogger,
    metagen: BaboonMetagen,
  ) extends BaboonCompiler[F] {

    override def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {

      for {
        _ <- cleanupTargetPaths(target.output)
        _ <- writeEvolutionJson(target, model)

        _           = logger.message( s"${target.id}: generating output...")
        translated <- translator.translate(model)

        _ <- F.traverseAccumErrors_(translated.files.iterator) {
          case (relativePath, output) =>
            F.traverse_(target.output.targetPathFor(output)) {
              targetDirectory =>
                val targetPath = targetDirectory.resolve(relativePath)
                writeFile(output, targetPath)
            }
        }
        _ = logger.message( s"${target.id}: done")

      } yield {}
    }

    private def writeFile(content: OutputFile, tgt: Path): F[NEList[BaboonIssue], Unit] = {
      F.fromTry(Try {
        tgt.getParent.toFile.mkdirs()

        if (options.debug) {
          logger.message("debug", s"$tgt\n$content")
        }

        Files.writeString(
          tgt,
          content.content.replace("\r", ""),
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
        ()
      }).leftMap(t => NEList(BaboonIssue.CantWriteOutput(tgt.toString, t)))
    }

    private def cleanupTargetPaths(targetOptions: OutputOptions): F[NEList[BaboonIssue], Unit] = {
      for {
        targetPaths <- F.pure(targetOptions.targetPaths.values.toList.distinct.filter(_.toFile.exists()))
        unexpectedFiles <- F
          .fromTry(Try(targetPaths.flatMap {
            IzFiles.walk(_).filter {
              p =>
                val f = p.toFile
                !f.isDirectory && !(
                  f.getName.startsWith(".") ||
                  targetOptions.safeToRemoveExtensions.exists(ext => f.getName.endsWith(s".$ext"))
                )
            }
          }))
          .catchAll(t => F.fail(NEList(CantCleanupTarget(Seq.empty, targetOptions.safeToRemoveExtensions.toSeq, Some(t)))))
        _ <- F
          .ifThenElse(unexpectedFiles.isEmpty)(
            F.fromTry(Try(targetPaths.foreach(path => IzFiles.erase(path))))
              .catchAll(t => F.fail(NEList(CantCleanupTarget(unexpectedFiles.map(_.toString), targetOptions.safeToRemoveExtensions.toSeq, Some(t))))),
            F.fail(NEList(CantCleanupTarget(unexpectedFiles.map(_.toString), targetOptions.safeToRemoveExtensions.toSeq, None))),
          )

      } yield {}
    }

    private def writeEvolutionJson(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {
      F.traverse_(target.generic.metaWriteEvolutionJsonTo) {
        maybePath =>
          val path = Option(maybePath.getParent) match {
            case Some(_) => maybePath
            case None    => target.output.output.resolve(maybePath)
          }

          val result = metagen.meta(model).toString()

          writeFile(OutputFile(result.replace("\r", ""), CompilerProduct.CustomMeta), path)
      }
    }
  }

}

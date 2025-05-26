package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.CantCleanupTarget
import io.septimalmind.baboon.translator.OutputFile
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
//  def run(inputs: Set[Path]): F[NEList[BaboonIssue], Unit]
  def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit]
}

object BaboonCompiler {
  class BaboonCompilerImpl[F[+_, +_]: Error2](
    translator: CSBaboonTranslator[F],
    options: CompilerOptions,
    logger: BLogger,
    metagen: BaboonMetagen,
  ) extends BaboonCompiler[F] {

    override def run(target: CompilerTarget, model: BaboonFamily): F[NEList[BaboonIssue], Unit] = {

      for {
        _ <- cleanupTargetPaths(target.output)
//          .catchAll {
//          value =>
//            System.err.println(s"Refusing to remove target directory, there are unexpected files: ${value.niceList()}")
//            System.err.println(s"Extensions allowed for removal: ${options.safeToRemoveExtensions.mkString(", ")}")
//            sys.exit(2)
//        }
        _ = target.generic.metaWriteEvolutionJsonTo.foreach {
          maybePath =>
            val path = Option(maybePath.getParent) match {
              case Some(_) => maybePath
              case None    => target.output.output.resolve(maybePath)
            }

            path.getParent.toFile.mkdirs()

            val result = metagen.meta(model).toString()

            Files.writeString(
              path,
              result.replace("\r", ""),
              StandardOpenOption.WRITE,
              StandardOpenOption.TRUNCATE_EXISTING,
              StandardOpenOption.CREATE,
            )
        }
        translated <- translator.translate(model)
        // io
        _ <- F.traverseAccumErrors_(translated.files.iterator) {
          case (relativePath, output) =>
            F.fromTry(Try {
              target.output.targetPathFor(output).foreach {
                targetDirectory =>
                  val targetPath = targetDirectory.resolve(relativePath)
                  writeFile(output, targetPath)
              }
            }).leftMap(t => NEList(BaboonIssue.CantWriteOutput(relativePath, t)))
        }

//        _ <- compiler.run(inputModels).catchAll {
//          value =>
//            System.err.println("Compiler failed")
//            System.err.println(value.toList.stringifyIssues)
//            sys.exit(3)
//        }
      } yield {}

    }
//
//    override def run(inputs: Set[Path]): F[NEList[BaboonIssue], Unit] = {
//      for {
//        loaded <- loader.load(inputs.toList)
//        // io
//
//
//      } yield {}
//    }

    private def writeFile(content: OutputFile, tgt: Path): Unit = {
      tgt.getParent.toFile.mkdirs()

      if (options.debug) {
        logger.message("debug", q"$tgt\n$content")
      }

      Files.writeString(
        tgt,
        content.content.replace("\r", ""),
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
      )
      ()
    }

    private def cleanupTargetPaths(targetOptions: OutputOptions): F[NEList[BaboonIssue], Unit] = {
      val targetPaths = targetOptions.targetPaths.values.toList.distinct.filter(_.toFile.exists())
      val unexpectedFiles = targetPaths.flatMap {
        IzFiles.walk(_).filter {
          p =>
            val f = p.toFile
            !f.isDirectory && !(
              f.getName.startsWith(".") ||
              targetOptions.safeToRemoveExtensions.exists(ext => f.getName.endsWith(s".$ext"))
            )
        }
      }

      if (unexpectedFiles.isEmpty) {
        targetPaths.foreach(path => IzFiles.erase(path))
        F.pure(())
      } else {
        F.fail(NEList(CantCleanupTarget(unexpectedFiles.map(_.toString), targetOptions.safeToRemoveExtensions.toSeq)))
      }
    }

  }

}

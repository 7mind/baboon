package io.septimalmind.baboon

import caseapp.*
import distage.Injector
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzArtifactMaterializer
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.{Path, Paths}

case class Options(
                    model: List[String],
                    modelDir: List[String],
                    output: String,
                    testOutput: Option[String],
                    debug: Option[Boolean],
                    @HelpMessage(
                      "generate shared runtime classes and evolution registrations, default is `with`"
                    )
                    @ValueDescription("with|only|without")
                    runtime: Option[String],
                    @HelpMessage("disable conversions (default is `false`)")
                    disableConversions: Option[Boolean],
                    @HelpMessage(
                      "generate obsolete errors instead of deprecations (default is `false`)"
                    )
                    csObsoleteErrors: Option[Boolean],
                    @HelpMessage(
                      "do not generate usings for System, System.Collections.Generic and System.Linq (see ImplicitUsings)"
                    )
                    csExcludeGlobalUsings: Option[Boolean],
                    omitMostRecentVersionSuffixFromPaths: Option[Boolean],
                    omitMostRecentVersionSuffixFromNamespaces: Option[Boolean],
                    csUseCompactAdtForm: Option[Boolean],
                    @HelpMessage(
                      "Every ADT branch will encode ADT metadata and expect it in the decoder"
                    )
                    csWrappedAdtBranchCodecs: Option[Boolean],
                  )

sealed trait RuntimeGenOpt

object RuntimeGenOpt {
  case object Only extends RuntimeGenOpt

  case object With extends RuntimeGenOpt

  case object Without extends RuntimeGenOpt
}

object Baboon {
  def main(args: Array[String]): Unit = {
    val artifact = implicitly[IzArtifactMaterializer]
    println(s"Baboon ${artifact.get.shortInfo}")

    CaseApp.parse[Options](args.toSeq) match {
      case Left(value) =>
        System.err.println(value.message)
        CaseApp.printHelp[Options]()
        System.exit(1)
      case Right(value) =>
        val opts = value._1
        val inputPaths = opts.modelDir.map(s => Paths.get(s))
        val testOutDir = opts.testOutput.map(o => Paths.get(o))

        val rtOpt = opts.runtime match {
          case Some("only") => RuntimeGenOpt.Only
          case Some("without") => RuntimeGenOpt.Without
          case _ => RuntimeGenOpt.With
        }

        val options = CompilerOptions(
          opts.debug.getOrElse(false),
          opts.csObsoleteErrors.getOrElse(false),
          rtOpt,
          !opts.disableConversions.getOrElse(false),
          !opts.csExcludeGlobalUsings.getOrElse(false),
          opts.omitMostRecentVersionSuffixFromPaths.getOrElse(true),
          opts.omitMostRecentVersionSuffixFromNamespaces.getOrElse(true),
          opts.csUseCompactAdtForm.getOrElse(true),
          opts.csWrappedAdtBranchCodecs.getOrElse(false),
        )
        Injector
          .NoCycles()
          .produceRun(new BaboonModule(options, inputPaths, testOutDir)) {
            (compiler: BaboonCompiler) =>
              val inputModels = opts.model
                .map(s => Paths.get(s))
                .toSet ++ inputPaths.flatMap { dir =>
                IzFiles
                  .walk(dir.toFile)
                  .filter(_.toFile.getName.endsWith(".baboon"))
              }
              val outDir = Paths.get(opts.output)
              println(
                s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}"
              )
              println(s"Target: ${outDir.toFile.getCanonicalPath}")
              testOutDir.foreach { t =>
                println(s"Test target: ${t.toFile.getCanonicalPath}")
              }

              cleanupTargetDir(outDir) match {
                case Left(value) =>
                  System.err.println(
                    s"Refusing to remove target directory, there are unexpected files: ${value.niceList()}"
                  )
                  System.exit(2)
                case Right(_) =>
                  compiler.run(inputModels, outDir, testOutDir) match {
                    case Left(value) =>
                      System.err.println("Compiler failed")
                      System.err.println(value.toList.stringifyIssues)
                      System.exit(3)
                    case Right(_) =>
                      println("Done")
                      System.exit(0)
                  }
              }

          }

    }
  }

  private def cleanupTargetDir(outDir: Path): Either[Seq[Path], Unit] = {
    if (outDir.toFile.exists()) {
      val unexpectedFiles = IzFiles
        .walk(outDir.toFile)
        .filter { p =>
          val f = p.toFile
          !f.isDirectory && !(f.getName.endsWith(".cs") ||
            f.getName.endsWith(".json") ||
            f.getName.startsWith("."))
        }
        .toSeq

      if (unexpectedFiles.isEmpty) {
        IzFiles.erase(outDir)
        Right(())
      } else {
        Left(unexpectedFiles)
      }
    } else {
      Right(())
    }
  }
}


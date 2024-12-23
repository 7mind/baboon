package io.septimalmind.baboon

import caseapp.*
import distage.Injector
import io.septimalmind.baboon.BaboonCompiler.{CompilerTargets}
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzArtifactMaterializer
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.{Path, Paths}

object Baboon {
  def main(args: Array[String]): Unit = {
    val artifact = implicitly[IzArtifactMaterializer]
    println(s"Baboon ${artifact.get.shortInfo}")

    CaseApp.parse[CLIOptions](args.toSeq) match {
      case Left(value) =>
        System.err.println(value.message)
        CaseApp.printHelp[CLIOptions]()
        System.exit(1)
      case Right(value) =>
        val opts = value._1
        val inputPaths = opts.modelDir.map(s => Paths.get(s))
        val testOutDir =
          opts.csOptions.generic.testOutput.map(o => Paths.get(o))

        val rtOpt = opts.csOptions.generic.runtime match {
          case Some("only")    => RuntimeGenOpt.Only
          case Some("without") => RuntimeGenOpt.Without
          case _               => RuntimeGenOpt.With
        }

        val options = CompilerOptions(
          debug = opts.debug.getOrElse(false),
          csOptions = CSOptions(
            GenericOptions(
              obsoleteErrors = opts.csOptions.csObsoleteErrors.getOrElse(false),
              runtime = rtOpt,
              generateConversions =
                !opts.csOptions.generic.disableConversions.getOrElse(false),
              metaWriteEvolutionJsonTo =
                opts.csOptions.generic.metaWriteEvolutionJson
                  .map(s => Paths.get(s)),
              codecTestIterations =
                opts.csOptions.generic.codecTestIterations.getOrElse(500),
            ),
            omitMostRecentVersionSuffixFromPaths =
              opts.csOptions.generic.omitMostRecentVersionSuffixFromPaths
                .getOrElse(true),
            omitMostRecentVersionSuffixFromNamespaces =
              opts.csOptions.generic.omitMostRecentVersionSuffixFromNamespaces
                .getOrElse(true),
            disregardImplicitUsings =
              !opts.csOptions.csExcludeGlobalUsings.getOrElse(false),
            csUseCompactAdtForm =
              opts.csOptions.csUseCompactAdtForm.getOrElse(true),
            csWrappedAdtBranchCodecs =
              opts.csOptions.csWrappedAdtBranchCodecs.getOrElse(false),
            csWriteEvolutionDict =
              opts.csOptions.csWriteEvolutionDict.getOrElse(false),
          )
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
              val outDir = Paths.get(opts.csOptions.generic.output)

              println(
                s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}"
              )
              println(s"Target: ${outDir.toFile.getCanonicalPath}")
              testOutDir.foreach(
                t => println(s"Test target: ${t.toFile.getCanonicalPath}")
              )

              cleanupTargetDir(outDir) match {
                case Left(value) =>
                  System.err.println(
                    s"Refusing to remove target directory, there are unexpected files: ${value.niceList()}"
                  )
                  System.exit(2)

                case Right(_) =>
                  val targets = CompilerTargets(outDir, testOutDir)
                  compiler.run(inputModels, targets) match {
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
            f.getName.endsWith(".meta") || // TODO: CLI parameter
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

package io.septimalmind.baboon

import caseapp.*
import distage.Injector
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
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
        val opts       = value._1
        val inputPaths = opts.modelDir.map(s => Paths.get(s))

        val rtOpt = opts.csOptions.generic.runtime match {
          case Some("only")    => RuntimeGenOpt.Only
          case Some("without") => RuntimeGenOpt.Without
          case _               => RuntimeGenOpt.With
        }

        val outDir         = Paths.get(opts.csOptions.generic.output)
        val testOutDir     = opts.csOptions.generic.testOutput.map(o => Paths.get(o))
        val fixturesOutDir = opts.csOptions.generic.fixtureOutput.map(o => Paths.get(o)).orElse(testOutDir)

        val options = CompilerOptions(
          debug = opts.debug.getOrElse(false),
          target = TargetOptions(
            runtime             = rtOpt,
            generateConversions = !opts.csOptions.generic.disableConversions.getOrElse(false),
            output              = outDir,
            fixturesOutput      = fixturesOutDir,
            testsOutput         = testOutDir,
          ),
          generic = GenericOptions(
            obsoleteErrors           = opts.csOptions.csObsoleteErrors.getOrElse(false),
            metaWriteEvolutionJsonTo = opts.csOptions.generic.metaWriteEvolutionJson.map(s => Paths.get(s)),
            codecTestIterations      = opts.csOptions.generic.codecTestIterations.getOrElse(500),
          ),
          csOptions = CSOptions(
            omitMostRecentVersionSuffixFromPaths      = opts.csOptions.generic.omitMostRecentVersionSuffixFromPaths.getOrElse(true),
            omitMostRecentVersionSuffixFromNamespaces = opts.csOptions.generic.omitMostRecentVersionSuffixFromNamespaces.getOrElse(true),
            disregardImplicitUsings                   = !opts.csOptions.csExcludeGlobalUsings.getOrElse(false),
            useCompactAdtForm                         = opts.csOptions.csUseCompactAdtForm.getOrElse(true),
            wrappedAdtBranchCodecs                    = opts.csOptions.csWrappedAdtBranchCodecs.getOrElse(false),
            writeEvolutionDict                        = opts.csOptions.csWriteEvolutionDict.getOrElse(false),
          ),
        )

        val safeToRemove = NEList.from(opts.extAllowCleanup) match {
          case Some(value) => value.toSet
          case None        => Set("meta", "cs", "json")
        }

        Injector.NoCycles().produceRun(new BaboonModule[Either](options, inputPaths, ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither)) {
          (compiler: BaboonCompiler[Either]) =>
            val inputModels = opts.model.map(s => Paths.get(s)).toSet ++ inputPaths.flatMap {
              dir =>
                IzFiles
                  .walk(dir.toFile)
                  .filter(_.toFile.getName.endsWith(".baboon"))
            }

            println(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}")
            println(s"Targets: ${options.target.targetPaths.map { case (t, p) => s"$t: $p" }.toList.sorted.niceList()}")

            val res: Either[Nothing, Unit] = for {
              _ <- cleanupTargetPaths[Either](options.target, safeToRemove).catchAll {
                value =>
                  System.err.println(s"Refusing to remove target directory, there are unexpected files: ${value.niceList()}")
                  System.err.println(s"Extensions allowed for removal: ${safeToRemove.mkString(", ")}")
                  sys.exit(2)
              }
              _ <- compiler.run(inputModels).catchAll {
                value =>
                  System.err.println("Compiler failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(3)
              }
            } yield ()
            res.merge
        }
    }
  }

  private def cleanupTargetPaths[F[+_, +_]: Error2](targetOptions: TargetOptions, safeToRemove: Set[String]): F[Seq[Path], Unit] = {
    val targetPaths = targetOptions.targetPaths.values.toList.distinct.filter(_.toFile.exists())
    val unexpectedFiles = targetPaths.flatMap {
      IzFiles.walk(_).filter {
        p =>
          val f = p.toFile
          !f.isDirectory && !(
            f.getName.startsWith(".") ||
            safeToRemove.exists(ext => f.getName.endsWith(s".$ext"))
          )
      }
    }

    if (unexpectedFiles.isEmpty) {
      targetPaths.foreach(path => IzFiles.erase(path))
      F.pure(())
    } else {
      F.fail(unexpectedFiles)
    }
  }
}

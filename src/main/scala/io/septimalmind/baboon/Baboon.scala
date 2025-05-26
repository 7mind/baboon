package io.septimalmind.baboon

import caseapp.*
import distage.{DIKey, Injector, Locator, Roots}
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.functional.IzEither.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzArtifactMaterializer
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.Paths

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
        entrypoint(convertOptions(value))
    }
  }

  private def convertOptions(value: (CLIOptions, Seq[String])) = {
    val opts = value._1

    val directoryInputs  = opts.modelDir.map(s => Paths.get(s)).toSet
    val individualInputs = opts.model.map(s => Paths.get(s)).toSet

    val rtOpt = opts.csOptions.generic.runtime match {
      case Some("only")    => RuntimeGenOpt.Only
      case Some("without") => RuntimeGenOpt.Without
      case _               => RuntimeGenOpt.With
    }

    val outDir         = Paths.get(opts.csOptions.generic.output)
    val testOutDir     = opts.csOptions.generic.testOutput.map(o => Paths.get(o))
    val fixturesOutDir = opts.csOptions.generic.fixtureOutput.map(o => Paths.get(o)).orElse(testOutDir)

    val safeToRemove = NEList.from(opts.extAllowCleanup) match {
      case Some(value) => value.toSet
      case None        => Set("meta", "cs", "json")
    }

    val options = CompilerOptions(
      debug            = opts.debug.getOrElse(false),
      individualInputs = individualInputs,
      directoryInputs  = directoryInputs,
      targets = Seq(
        CompilerTarget.CSTarget(
          id = "C#",
          output = OutputOptions(
            safeToRemoveExtensions = safeToRemove,
            runtime                = rtOpt,
            generateConversions    = !opts.csOptions.generic.disableConversions.getOrElse(false),
            output                 = outDir,
            fixturesOutput         = fixturesOutDir,
            testsOutput            = testOutDir,
          ),
          generic = GenericOptions(
            obsoleteErrors           = opts.csOptions.csObsoleteErrors.getOrElse(false),
            metaWriteEvolutionJsonTo = opts.csOptions.generic.metaWriteEvolutionJson.map(s => Paths.get(s)),
            codecTestIterations      = opts.csOptions.generic.codecTestIterations.getOrElse(500),
          ),
          language = CSOptions(
            omitMostRecentVersionSuffixFromPaths      = opts.csOptions.generic.omitMostRecentVersionSuffixFromPaths.getOrElse(true),
            omitMostRecentVersionSuffixFromNamespaces = opts.csOptions.generic.omitMostRecentVersionSuffixFromNamespaces.getOrElse(true),
            disregardImplicitUsings                   = !opts.csOptions.csExcludeGlobalUsings.getOrElse(false),
            useCompactAdtForm                         = opts.csOptions.csUseCompactAdtForm.getOrElse(true),
            wrappedAdtBranchCodecs                    = opts.csOptions.csWrappedAdtBranchCodecs.getOrElse(false),
            writeEvolutionDict                        = opts.csOptions.csWriteEvolutionDict.getOrElse(false),
            enableDeprecatedEncoders                  = opts.csOptions.enableDeprecatedEncoders.getOrElse(false),
          ),
        )
      ),
    )
    options
  }

  private def processTarget(loc: Locator, model: BaboonFamily, target: CompilerTarget): Either[Nothing, Unit] = {
    val module = target match {
      case t: CompilerTarget.CSTarget =>
        new BaboonCSModule[Either](t)
    }

    Injector
      .NoCycles(parent = Some(loc))
      .produceRun(module) {
        compiler: BaboonCompiler[Either] =>
          val res: Either[Nothing, Unit] = for {
            _ <- Right(println(s"Working on ${target.id}..."))
            _ <- Right(println(s"Outputs: ${target.output.targetPaths.map { case (t, p) => s"$t: $p" }.toList.sorted.niceList()}"))
            _ <- compiler.run(target, model).catchAll {
              value =>
                System.err.println("Compiler failed")
                System.err.println(value.toList.stringifyIssues)
                sys.exit(3)
            }
            _ <- Right(println(s"Done: ${target.id}"))

          } yield ()
          res.merge
      }
    Right(())

  }

  private def entrypoint(options: CompilerOptions): Unit = {
    val m = new BaboonModule[Either](options, ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither)

    Injector
      .NoCycles()
      .produce(m, Roots(DIKey.get[BaboonLoader[Either]]))
      .use(
        loc =>
          for {
            inputModels <- Right(options.individualInputs ++ options.directoryInputs.flatMap {
              dir =>
                IzFiles
                  .walk(dir.toFile)
                  .filter(_.toFile.getName.endsWith(".baboon"))
            })
            _ <- Right(println(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}"))

            loader = loc.get[BaboonLoader[Either]]

            loadedModels <- loader.load(inputModels.toList).catchAll {
              value =>
                System.err.println("Loader failed")
                System.err.println(value.toList.stringifyIssues)
                sys.exit(4)
            }

            out <- options.targets.map(processTarget(loc, loadedModels, _)).biSequenceScalar

          } yield {}
      )
    ()

//      .produceRun() {
//        (loader: BaboonLoader[Either]) =>
//      }

//    ???
//    Injector.NoCycles().produceRun(new BaboonModule[Either](options, ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither)) {
//      (compiler: BaboonCompiler[Either]) =>
//        val inputModels = options.individualInputs ++ options.directoryInputs.flatMap {
//          dir =>
//            IzFiles
//              .walk(dir.toFile)
//              .filter(_.toFile.getName.endsWith(".baboon"))
//        }
//
//        println(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}")
//        println(s"Targets: ${options.target.targetPaths.map { case (t, p) => s"$t: $p" }.toList.sorted.niceList()}")
//
//        val res: Either[Nothing, Unit] = for {
//          _ <- cleanupTargetPaths[Either](options.target, options.safeToRemoveExtensions).catchAll {
//            value =>
//              System.err.println(s"Refusing to remove target directory, there are unexpected files: ${value.niceList()}")
//              System.err.println(s"Extensions allowed for removal: ${options.safeToRemoveExtensions.mkString(", ")}")
//              sys.exit(2)
//          }
//          _ <- compiler.run(inputModels).catchAll {
//            value =>
//              System.err.println("Compiler failed")
//              System.err.println(value.toList.stringifyIssues)
//              sys.exit(3)
//          }
//        } yield ()
//        res.merge
//    }
  }

}

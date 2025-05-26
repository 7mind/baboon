package io.septimalmind.baboon

import caseapp.*
import distage.{DIKey, DefaultModule, Injector, Locator, Roots, TagKK}
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.functional.IzEither.*
import izumi.functional.bio.{Error2, F}
import izumi.functional.quasi.QuasiIO
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.functional.Identity
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

  private def processTarget[F[+_, +_]: Error2: TagKK](
    loc: Locator,
    model: BaboonFamily,
    target: CompilerTarget,
  )(implicit
    q: QuasiIO[F[Throwable, _]],
    m: DefaultModule[F[Throwable, _]],
  ): F[Throwable, Unit] = { // dirty, I/O happens there
    val module = target match {
      case t: CompilerTarget.CSTarget =>
        new BaboonCSModule[F](t)
    }

    val logger = loc.get[BLogger]

    Injector
      .NoCycles[F[Throwable, _]](parent = Some(loc))
      .produceRun(module) {
        (compiler: BaboonCompiler[F]) =>
          for {
            _ <- F.pure(())
            _  = logger.message(s"${target.id}: output configuration: ${target.output.targetPaths.map { case (t, p) => s"$t: $p" }.toList.sorted.niceList()}")

            _ <- compiler.run(target, model).catchAll {
              value =>
                System.err.println("Compiler failed")
                System.err.println(value.toList.stringifyIssues)
                sys.exit(3)
            }
          } yield ()
      }
  }

  private def entrypoint(options: CompilerOptions): Unit = {
    val m = new BaboonModule[Either](options, ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither)

    import QuasiIO.*
    import QuasiIOEither.*

    Injector
      .NoCycles()
      .produce(m, Roots(DIKey.get[BaboonLoader[Either]], DIKey.get[BLogger]))
      .use(
        loc =>
          for {
            loader <- F.pure(loc.get[BaboonLoader[Either]])
            logger <- F.pure(loc.get[BLogger])

            inputModels <- Right(options.individualInputs ++ options.directoryInputs.flatMap {
              dir =>
                IzFiles
                  .walk(dir.toFile)
                  .filter(_.toFile.getName.endsWith(".baboon"))
            })
            _ = logger.message(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}")

            loadedModels <- loader.load(inputModels.toList).catchAll {
              value =>
                System.err.println("Loader failed")
                System.err.println(value.toList.stringifyIssues)
                sys.exit(4)
            }

            _ <- options.targets.map(processTarget[Either](loc, loadedModels, _)).biSequenceScalar
          } yield {}
      )
    ()
  }

}

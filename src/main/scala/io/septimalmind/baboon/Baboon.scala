package io.septimalmind.baboon

import caseapp.*
import distage.*
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.functional.IzEither.*
import izumi.functional.bio.{Error2, F}
import izumi.functional.quasi.QuasiIO
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.cli.CLIParserImpl
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzArtifactMaterializer
import izumi.fundamentals.platform.strings.IzString.*

import java.nio.file.Paths

object Baboon {
  def main(args: Array[String]): Unit = {
    val artifact = implicitly[IzArtifactMaterializer]
    println(s"Baboon ${artifact.get.shortInfo}")

    new CLIParserImpl().parse(args) match {
      case Left(value) =>
        System.err.println("Cannot parse multimodal commandline ([ARGS] [:role [role-args]] [:role1 [role1-args]] ...")
        System.err.println(value.toString)
        System.exit(1)

      case Right(value) =>
        val generalArgs = value.globalParameters.values.flatMap(v => Seq(s"--${v.name}", v.value)) ++
          value.globalParameters.flags.map(f => s"${f.name}")

        import izumi.functional.IzEither.*

        for {
          generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => s"Can't parse generic CLI: $e")
          launchArgs <- value.roles.map {
            r =>
              val roleArgs = r.roleParameters.values.flatMap(v => Seq(s"--${v.name}", v.value)) ++
                r.roleParameters.flags.map(f => s"${f.name}") ++ r.freeArgs

              r.role match {
                case "cs" =>
                  CaseApp.parse[CsCLIOptions](roleArgs).leftMap(e => s"Can't parse cs CLI: $e").map {
                    case (opts, _) =>
                      val shopts = mkGenericOpts(opts)

                      CompilerTarget.CSTarget(
                        id      = "C#",
                        output  = shopts.outOpts,
                        generic = shopts.genericOpts,
                        language = CSOptions(
                          obsoleteErrors                            = opts.csObsoleteErrors.getOrElse(false),
                          omitMostRecentVersionSuffixFromPaths      = opts.generic.omitMostRecentVersionSuffixFromPaths.getOrElse(true),
                          omitMostRecentVersionSuffixFromNamespaces = opts.generic.omitMostRecentVersionSuffixFromNamespaces.getOrElse(true),
                          disregardImplicitUsings                   = !opts.csExcludeGlobalUsings.getOrElse(false),
                          useCompactAdtForm                         = opts.csUseCompactAdtForm.getOrElse(true),
                          wrappedAdtBranchCodecs                    = opts.csWrappedAdtBranchCodecs.getOrElse(false),
                          writeEvolutionDict                        = opts.csWriteEvolutionDict.getOrElse(false),
                          enableDeprecatedEncoders                  = opts.enableDeprecatedEncoders.getOrElse(false),
                        ),
                      )
                  }
                case "scala" =>
                  CaseApp.parse[ScCLIOptions](roleArgs).leftMap(e => s"Can't parse cs CLI: $e").map {
                    case (opts, _) =>
                      val shopts = mkGenericOpts(opts)

                      CompilerTarget.ScTarget(
                        id       = "Scala",
                        output   = shopts.outOpts,
                        generic  = shopts.genericOpts,
                        language = ScOptions(),
                      )
                  }
                case r => Left(s"Unknown role id: $r")
              }
          }.biSequenceScalar
        } yield {
          val directoryInputs  = generalOptions._1.modelDir.map(s => Paths.get(s)).toSet
          val individualInputs = generalOptions._1.model.map(s => Paths.get(s)).toSet

          val options = CompilerOptions(
            debug            = generalOptions._1.debug.getOrElse(false),
            individualInputs = individualInputs,
            directoryInputs  = directoryInputs,
            targets          = launchArgs,
          )

          entrypoint(options)
        }
    }
  }

  case class SharedOpts(outOpts: OutputOptions, genericOpts: GenericOptions)

  private def mkGenericOpts(opts: SharedCLIOptions): SharedOpts = {
    val rtOpt = opts.generic.runtime match {
      case Some("only")    => RuntimeGenOpt.Only
      case Some("without") => RuntimeGenOpt.Without
      case _               => RuntimeGenOpt.With
    }

    val outDir         = Paths.get(opts.generic.output)
    val testOutDir     = opts.generic.testOutput.map(o => Paths.get(o))
    val fixturesOutDir = opts.generic.fixtureOutput.map(o => Paths.get(o)).orElse(testOutDir)

    val safeToRemove = NEList.from(opts.extAllowCleanup) match {
      case Some(value) => value.toSet
      case None        => Set("meta", "cs", "json", "scala")
    }

    val outOpts = OutputOptions(
      safeToRemoveExtensions = safeToRemove,
      runtime                = rtOpt,
      generateConversions    = !opts.generic.disableConversions.getOrElse(false),
      output                 = outDir,
      fixturesOutput         = fixturesOutDir,
      testsOutput            = testOutDir,
    )
    val genericOpts = GenericOptions(
      metaWriteEvolutionJsonTo = opts.generic.metaWriteEvolutionJson.map(s => Paths.get(s)),
      codecTestIterations      = opts.generic.codecTestIterations.getOrElse(500),
    )
    SharedOpts(outOpts, genericOpts)
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
      case t: CompilerTarget.ScTarget =>
        new BaboonScModule[F](t)
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

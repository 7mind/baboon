package io.septimalmind.baboon

import caseapp.*
import distage.*
import io.septimalmind.baboon.explore.{ExploreContext, ExploreInputs, ExploreShell}
import io.septimalmind.baboon.lsp._
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.BaboonModeAxis
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.impl.BioEither
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.functional.quasi.{QuasiIO, QuasiIORunner}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.cli.MultiModalArgsParserImpl
import izumi.fundamentals.platform.cli.model.{ModalityArgs, MultiModalArgs}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzArtifactMaterializer
import izumi.fundamentals.platform.strings.IzString.*
import izumi.distage.model.definition.Activation

import java.nio.file.Paths

object Baboon {
  private type EitherF[+e, +a] = Either[e, a]

  private val helpText: String =
    """Usage: baboon [options] [:cs [cs-options] | :scala [scala-options] | :lsp [lsp-options] | :explore]
      |
      |Global options:
      |  --model <file>           A *.baboon file to process (can be repeated)
      |  --model-dir <dir>        A directory to recursively read *.baboon files from (can be repeated)
      |  --lock-file <file>       A file used to track model signatures
      |  --meta-write-evolution-json <file>  Write evolution metadata as JSON
      |  --debug                  Enable debug output
      |  --help                   Show this help message
      |
      |Modalities:
      |  :cs                      Generate C# code
      |  :scala                   Generate Scala code
      |  :rust                    Generate Rust code
      |  :typescript              Generate TypeScript code
      |  :lsp                     Start LSP server
      |  :explore                 Start interactive explorer
      |
      |C# options (:cs):
      |  --output <dir>           Output directory for generated code
      |  --fixture-output <dir>   Output directory for generated fixtures
      |  --runtime <only|with|without>  Runtime generation mode
      |  --cs-obsolete-errors     Generate obsolete errors instead of deprecations
      |  --cs-wrapped-adt-branch-codecs  ADT branches encode/expect metadata
      |  --deduplicate            Apply code deduplication (default: true)
      |
      |Scala options (:scala):
      |  --output <dir>           Output directory for generated code
      |  --fixture-output <dir>   Output directory for generated fixtures
      |  --runtime <only|with|without>  Runtime generation mode
      |  --sc-write-evolution-dict  Add evolution metadata as Scala dictionary
      |  --sc-wrapped-adt-branch-codecs  ADT branches encode/expect metadata
      |
      |Rust options (:rust):
      |  --output <dir>           Output directory for generated code
      |  --fixture-output <dir>   Output directory for generated fixtures
      |  --runtime <only|with|without>  Runtime generation mode
      |  --rs-write-evolution-dict  Add evolution metadata as Rust dictionary
      |  --rs-wrapped-adt-branch-codecs  ADT branches encode/expect metadata
      |
      |LSP options (:lsp):
      |  --port <port>            TCP port to listen on (default: stdio)
      |
      |Examples:
      |  baboon --model-dir ./models :cs --output ./out/cs :scala --output ./out/scala
      |  baboon --model-dir ./models :lsp
      |  baboon --model-dir ./models :lsp --port 5007
      |  baboon --model-dir ./models :explore
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val artifact  = implicitly[IzArtifactMaterializer]
    val isLspMode = args.contains(":lsp") || args.contains("lsp")

    // Print banner to stderr in LSP mode to avoid breaking the protocol
    val bannerOut = if (isLspMode) System.err else System.out
    bannerOut.println(s"Baboon ${artifact.get.shortInfo}")

    if (args.contains("--help") || args.contains("-h")) {
      println(helpText)
      System.exit(0)
    }

    new MultiModalArgsParserImpl().parse(args).merge match {
      case MultiModalArgs(generalArgs, modalities) =>
        val isExploreMode = modalities.exists { case ModalityArgs(id, _) => id == "explore" }
        val _             = isLspMode || modalities.exists { case ModalityArgs(id, _) => id == "lsp" }

        if (isLspMode) {
          val lspModality = modalities.find(_.id == "lsp")
          val lspPort = lspModality.flatMap {
            m =>
              val args = m.args.toSeq
              args
                .sliding(2).collectFirst {
                  case Seq("--port", p) => scala.util.Try(p.toInt).toOption
                }.flatten
          }

          val out = for {
            generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => NEList(s"Can't parse generic CLI: $e"))
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
            import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

            lspEntrypoint(directoryInputs, individualInputs, lspPort)
          }
          out match {
            case Left(value) =>
              System.err.println(value.toList.niceList())
              System.exit(1)
              ()
            case Right(_) =>
              System.exit(0)
              ()
          }
        } else if (isExploreMode) {
          val out = for {
            generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => NEList(s"Can't parse generic CLI: $e"))
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
            import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

            exploreEntrypoint(directoryInputs, individualInputs)
          }
          out match {
            case Left(value) =>
              System.err.println(value.toList.niceList())
              System.exit(1)
              ()
            case Right(_) =>
              System.exit(0)
              ()
          }
        } else {
          val out = for {
            generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => NEList(s"Can't parse generic CLI: $e"))
            launchArgs <- BioEither.traverseAccumErrorsNEList(modalities) {
              case ModalityArgs(roleId, roleArgs) =>
                roleId match {
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
                            wrappedAdtBranchCodecs                    = opts.csWrappedAdtBranchCodecs.getOrElse(false),
                            writeEvolutionDict                        = opts.csWriteEvolutionDict.getOrElse(false),
                            enableDeprecatedEncoders                  = opts.enableDeprecatedEncoders.getOrElse(false),
                            generateIndexWriters                      = opts.generateIndexWriters.getOrElse(true),
                            generateJsonCodecs                        = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs                        = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault               = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault               = opts.generateUebaCodecsByDefault.getOrElse(false),
                            deduplicate                               = opts.deduplicate.getOrElse(true),
                            serviceResult                             = mkServiceResult(opts, ServiceResultConfig.csDefault),
                            serviceContext                            = mkServiceContext(opts),
                            pragmas                                   = parsePragmas(opts.pragma),
                          ),
                        )
                    }
                  case "scala" =>
                    CaseApp.parse[ScCLIOptions](roleArgs).leftMap(e => s"Can't parse cs CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.ScTarget(
                          id      = "Scala",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = ScOptions(
                            writeEvolutionDict          = opts.scWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.scWrappedAdtBranchCodecs.getOrElse(false),
                            enableDeprecatedEncoders    = opts.enableDeprecatedEncoders.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.scalaDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                          ),
                        )
                    }
                  case "python" =>
                    CaseApp.parse[PyCLIOptions](roleArgs).leftMap(e => s"Can't parse python CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.PyTarget(
                          id      = "Python",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = PyOptions(
                            writeEvolutionDict          = opts.pyWriteEvolutionDict.getOrElse(true),
                            wrappedAdtBranchCodecs      = opts.pyWrappedAdtBranchCodecs.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            enableDeprecatedEncoders    = opts.enableDeprecatedEncoders.getOrElse(false),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.pythonDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                          ),
                        )
                    }
                  case "rust" =>
                    CaseApp.parse[RsCLIOptions](roleArgs).leftMap(e => s"Can't parse rust CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.RsTarget(
                          id      = "Rust",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = RsOptions(
                            writeEvolutionDict          = opts.rsWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.rsWrappedAdtBranchCodecs.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.rustDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                          ),
                        )
                    }
                  case "typescript" =>
                    CaseApp.parse[TsCLIOptions](roleArgs).leftMap(e => s"Can't parse typescript CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.TsTarget(
                          id      = "TypeScript",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = TsOptions(
                            writeEvolutionDict          = opts.tsWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.tsWrappedAdtBranchCodecs.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.typescriptDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                          ),
                        )
                    }
                  case r => Left(s"Unknown role id: $r")
                }
            }
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            val options = CompilerOptions(
              debug                    = generalOptions._1.debug.getOrElse(false),
              individualInputs         = individualInputs,
              directoryInputs          = directoryInputs,
              targets                  = launchArgs,
              metaWriteEvolutionJsonTo = generalOptions._1.metaWriteEvolutionJson.map(s => FSPath.parse(NEString.unsafeFrom(s))),
              lockFile                 = generalOptions._1.lockFile.map(s => FSPath.parse(NEString.unsafeFrom(s))),
            )

            import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
            import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

            entrypoint(options)
          }
          out match {
            case Left(value) =>
              System.err.println(value.toList.niceList())
              System.exit(1)
              ()
            case Right(_) =>
              System.exit(0)
              ()
          }
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

    val outDir         = FSPath.parse(NEString.unsafeFrom(opts.generic.output))
    val testOutDir     = opts.generic.testOutput.map(o => FSPath.parse(NEString.unsafeFrom(o)))
    val fixturesOutDir = opts.generic.fixtureOutput.map(o => FSPath.parse(NEString.unsafeFrom(o))).orElse(testOutDir)

    val safeToRemove = NEList.from(opts.extAllowCleanup) match {
      case Some(value) => value.toSet
      case None        => Set("meta", "cs", "json", "scala", "py", "pyc", "rs", "ts")
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
      codecTestIterations = opts.generic.codecTestIterations.getOrElse(500)
    )
    SharedOpts(outOpts, genericOpts)
  }

  private def parsePragmas(raw: List[String]): Map[String, String] = {
    raw.flatMap { s =>
      val idx = s.indexOf('=')
      if (idx > 0) Some(s.substring(0, idx).trim -> s.substring(idx + 1).trim)
      else None
    }.toMap
  }

  private def mkServiceContext(opts: SharedCLIOptions): ServiceContextConfig = {
    ServiceContextConfig(
      mode          = opts.serviceContextMode.getOrElse("none"),
      typeName      = opts.serviceContextType.getOrElse("Ctx"),
      parameterName = opts.serviceContextParameterName.getOrElse("ctx"),
    )
  }

  private def mkServiceResult(opts: SharedCLIOptions, default: ServiceResultConfig): ServiceResultConfig = {
    val hkt = opts match {
      case sc: ScalaHktCLIOptions if sc.serviceResultHkt.getOrElse(false) =>
        Some(HktConfig(
          name      = sc.serviceResultHktName.getOrElse("F"),
          signature = sc.serviceResultHktSignature.getOrElse("[+_, +_]"),
        ))
      case _ => default.hkt
    }
    ServiceResultConfig(
      noErrors   = opts.serviceResultNoErrors.getOrElse(default.noErrors),
      resultType = opts.serviceResultType.orElse(default.resultType),
      pattern    = opts.serviceResultPattern.orElse(default.pattern),
      hkt        = hkt,
    )
  }

  private def processTarget[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
    loc: Locator,
    logger: BLogger,
    model: BaboonFamily,
    target: CompilerTarget,
  )(implicit
    q: QuasiIO[F[Throwable, _]],
    m: DefaultModule[F[Throwable, _]],
  ): F[Throwable, Unit] = { // dirty, I/O happens there
    val module = target match {
      case t: CompilerTarget.CSTarget =>
        new BaboonJvmCSModule[F](t)
      case t: CompilerTarget.ScTarget =>
        new BaboonJvmScModule[F](t)
      case t: CompilerTarget.PyTarget =>
        new BaboonJvmPyModule[F](t)
      case t: CompilerTarget.RsTarget =>
        new BaboonJvmRsModule[F](t)
      case t: CompilerTarget.TsTarget =>
        new BaboonJvmTsModule[F](t)
    }

    Injector
      .NoCycles(parent = Some(loc))
      .produceRun(module) {
        (compiler: BaboonCompiler[F]) =>
          for {
            _ <- F.maybeSuspend {
              logger.message(s"${target.id}: output configuration: ${target.output.targetPaths.map { case (t, p) => s"$t: $p" }.toList.sorted.niceList()}")
            }

            _ <- compiler.run(target, model).catchAll {
              value =>
                System.err.println("Compiler failed")
                System.err.println(value.toList.stringifyIssues)
                sys.exit(3)
            }
          } yield ()
      }
  }

  private def entrypoint[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    options: CompilerOptions
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Unit = {
    val m = new BaboonModuleJvm[F](options, ParallelErrorAccumulatingOps2[F])
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[F[Throwable, _]]()
        .produceRun(m, Activation(BaboonModeAxis.Compiler)) {
          (loader: BaboonLoader[F], logger: BLogger, loc: Locator) =>
            for {
              inputModels <- F.maybeSuspend(options.individualInputs.map(_.toPath) ++ options.directoryInputs.flatMap {
                dir =>
                  IzFiles
                    .walk(dir.toFile)
                    .filter(_.toFile.getName.endsWith(".baboon"))
              })
              _ <- F.maybeSuspend {
                logger.message(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}")
              }

              loadedModels <- loader.load(inputModels.toList).catchAll {
                value =>
                  System.err.println("Loader failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(4)
              }

              _ <- F.traverse_(options.targets)(processTarget[F](loc, logger, loadedModels, _))
            } yield {}
        }
    }
  }

  private def exploreEntrypoint(
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
  )(implicit
    quasiIO: QuasiIO[Either[Throwable, _]],
    runner: QuasiIORunner[Either[Throwable, _]],
    error2: Error2[EitherF],
    maybeSuspend2: MaybeSuspend2[EitherF],
    parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[EitherF],
    tag: TagKK[EitherF],
    defaultModule2: DefaultModule2[EitherF],
  ): Unit = {
    val options = CompilerOptions(
      debug                    = false,
      individualInputs         = individualInputs,
      directoryInputs          = directoryInputs,
      targets                  = Seq.empty,
      metaWriteEvolutionJsonTo = None,
      lockFile                 = None,
    )
    val m = new BaboonModuleJvm[EitherF](options, parallelAccumulatingOps2)
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[EitherF[Throwable, _]]()
        .produceRun(m, Activation(BaboonModeAxis.Explorer)) {
          (loader: BaboonLoader[EitherF], logger: BLogger, exploreContext: Subcontext[ExploreContext[EitherF]]) =>
            for {
              inputModels <- F.maybeSuspend(individualInputs.map(_.toPath) ++ directoryInputs.flatMap {
                dir =>
                  IzFiles
                    .walk(dir.toFile)
                    .filter(_.toFile.getName.endsWith(".baboon"))
              })
              _ <- F.maybeSuspend {
                logger.message(s"Inputs: ${inputModels.map(_.toFile.getCanonicalPath).toList.sorted.niceList()}")
              }

              loadedModels <- loader.load(inputModels.toList).catchAll {
                value =>
                  System.err.println("Loader failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(4)
              }

              _ <- exploreContext
                .provide(loadedModels)
                .provide(ExploreInputs(directoryInputs, individualInputs))
                .produce()
                .use {
                  ctx =>
                    F.maybeSuspend {
                      val shell = new ExploreShell(ctx)
                      shell.run()
                    }
                }
            } yield {}
        }
    }
  }

  private def lspEntrypoint[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
    port: Option[Int],
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Unit = {
    val options = CompilerOptions(
      debug                    = false,
      individualInputs         = individualInputs,
      directoryInputs          = directoryInputs,
      targets                  = Seq.empty,
      metaWriteEvolutionJsonTo = None,
      lockFile                 = None,
    )
    val m = new BaboonModuleJvm[F](options, ParallelErrorAccumulatingOps2[F])

    runner.run {
      val cliModelDirs = directoryInputs.map(dir => Paths.get(dir.asString))
      val lspModule = new BaboonLspModuleJvm[F](
        modelDirs    = cliModelDirs,
        port         = port,
        runner       = runner,
        exitCallback = () => System.exit(0),
      )

      Injector
        .NoCycles[F[Throwable, _]]()
        .produceRun(new ModuleDef {
          include(m)
          include(lspModule)
        }, Activation(BaboonModeAxis.Lsp)) {
          (launcher: LspLauncher, logger: BLogger) =>
            F.maybeSuspend {
              logger.message(LspLogging.Context, "Starting Baboon LSP server...")
              launcher.launch()
            }
        }
    }
  }

}

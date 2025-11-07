package io.septimalmind.baboon

import caseapp.*
import distage.*
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
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

import java.nio.file.Paths

object Baboon {
  def main(args: Array[String]): Unit = {
    val artifact = implicitly[IzArtifactMaterializer]
    println(s"Baboon ${artifact.get.shortInfo}")

    new MultiModalArgsParserImpl().parse(args).merge match {
      case MultiModalArgs(generalArgs, modalities) =>
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
                          writeEvolutionDict     = opts.scWriteEvolutionDict.getOrElse(false),
                          wrappedAdtBranchCodecs = opts.scWrappedAdtBranchCodecs.getOrElse(false),
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
          case Right(value) =>
            System.exit(0)
            ()
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
      codecTestIterations = opts.generic.codecTestIterations.getOrElse(500)
    )
    SharedOpts(outOpts, genericOpts)
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
        new BaboonCSModule[F](t)
      case t: CompilerTarget.ScTarget =>
        new BaboonScModule[F](t)
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
    val m = new BaboonModule[F](options, ParallelErrorAccumulatingOps2[F])
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[F[Throwable, _]]()
        .produceRun(m) {
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

}

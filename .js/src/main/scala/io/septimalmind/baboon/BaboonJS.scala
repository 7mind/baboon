package io.septimalmind.baboon

import distage.*
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.{BLogger, BLoggerJS}
import izumi.functional.bio.unsafe.MaybeSuspend2
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.functional.quasi.{QuasiIO, QuasiIORunner}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.JSConverters.*

/**
  * JavaScript API for Baboon compiler
  */
@JSExportTopLevel("BaboonCompiler")
object BaboonJS {

  /**
    * Input file representation for JS
    */
  @js.native
  trait JSInputFile extends js.Object {
    val path: String
    val content: String
  }

  /**
    * Output file representation for JS
    */
  trait JSOutputFile extends js.Object {
    val path: String
    val content: String
    val product: String
  }

  def createJSOutputFile(path: String, content: String, product: String): JSOutputFile = {
    js.Dynamic
      .literal(
        path    = path,
        content = content,
        product = product,
      ).asInstanceOf[JSOutputFile]
  }

  /**
    * Compilation result
    */
  trait JSCompilationResult extends js.Object {
    val success: Boolean
    val files: js.UndefOr[js.Array[JSOutputFile]]
    val errors: js.UndefOr[js.Array[String]]
  }

  object JSCompilationResult {
    def success(files: js.Array[JSOutputFile]): JSCompilationResult = {
      js.Dynamic
        .literal(
          success = true,
          files   = files,
        ).asInstanceOf[JSCompilationResult]
    }

    def failure(errors: js.Array[String]): JSCompilationResult = {
      js.Dynamic
        .literal(
          success = false,
          errors  = errors,
        ).asInstanceOf[JSCompilationResult]
    }
  }

  /**
    * Language-specific options
    */
  @js.native
  trait JSCSOptions extends js.Object {
    val obsoleteErrors: js.UndefOr[Boolean]
    val omitMostRecentVersionSuffixFromPaths: js.UndefOr[Boolean]
    val omitMostRecentVersionSuffixFromNamespaces: js.UndefOr[Boolean]
    val wrappedAdtBranchCodecs: js.UndefOr[Boolean]
    val writeEvolutionDict: js.UndefOr[Boolean]
    val disregardImplicitUsings: js.UndefOr[Boolean]
    val enableDeprecatedEncoders: js.UndefOr[Boolean]
    val generateIndexWriters: js.UndefOr[Boolean]
    val generateJsonCodecs: js.UndefOr[Boolean]
    val generateUebaCodecs: js.UndefOr[Boolean]
    val generateUebaCodecsByDefault: js.UndefOr[Boolean]
    val generateJsonCodecsByDefault: js.UndefOr[Boolean]
    val deduplicate: js.UndefOr[Boolean]
  }

  @js.native
  trait JSScOptions extends js.Object {
    val writeEvolutionDict: js.UndefOr[Boolean]
    val wrappedAdtBranchCodecs: js.UndefOr[Boolean]
  }

  @js.native
  trait JSGenericOptions extends js.Object {
    val codecTestIterations: js.UndefOr[Int]
    val omitMostRecentVersionSuffixFromPaths: js.UndefOr[Boolean]
    val omitMostRecentVersionSuffixFromNamespaces: js.UndefOr[Boolean]
    val runtime: js.UndefOr[String] // "with", "only", "without"
    val disableConversions: js.UndefOr[Boolean]
    val generateTests: js.UndefOr[Boolean]
    val generateFixtures: js.UndefOr[Boolean]
  }

  @js.native
  trait JSCompilerTarget extends js.Object {
    val language: String // "cs" or "scala"
    val generic: js.UndefOr[JSGenericOptions]
    val cs: js.UndefOr[JSCSOptions]
    val scala: js.UndefOr[JSScOptions]
  }

  @js.native
  trait JSCompilerOptions extends js.Object {
    val inputs: js.Array[JSInputFile]
    val targets: js.Array[JSCompilerTarget]
    val debug: js.UndefOr[Boolean]
  }

  private def parseRuntimeOpt(opt: js.UndefOr[String]): RuntimeGenOpt = {
    opt.toOption match {
      case Some("only")    => RuntimeGenOpt.Only
      case Some("without") => RuntimeGenOpt.Without
      case _               => RuntimeGenOpt.With
    }
  }

  private def createOutputOptions(generic: js.UndefOr[JSGenericOptions]): OutputOptionsJS = {
    val g = generic.getOrElse(js.Dynamic.literal().asInstanceOf[JSGenericOptions])
    OutputOptionsJS(
      safeToRemoveExtensions = Set("meta", "cs", "json", "scala"),
      runtime                = parseRuntimeOpt(g.runtime),
      generateConversions    = !g.disableConversions.getOrElse(false),
      generateTests          = g.generateTests.getOrElse(false),
      generateFixtures       = g.generateFixtures.getOrElse(false),
    )
  }

  private def createGenericOptions(generic: js.UndefOr[JSGenericOptions]): GenericOptions = {
    val g = generic.getOrElse(js.Dynamic.literal().asInstanceOf[JSGenericOptions])
    GenericOptions(
      codecTestIterations = g.codecTestIterations.getOrElse(500)
    )
  }

  private def createTargets(jsTargets: js.Array[JSCompilerTarget]): Seq[CompilerTargetJS] = {
    jsTargets.toSeq.map {
      target =>
        target.language match {
          case "cs" =>
            val opts    = target.cs.getOrElse(js.Dynamic.literal().asInstanceOf[JSCSOptions])
            val generic = target.generic.getOrElse(js.Dynamic.literal().asInstanceOf[JSGenericOptions])

            CompilerTargetJS.CSTarget(
              id      = "C#",
              output  = createOutputOptions(target.generic),
              generic = createGenericOptions(target.generic),
              language = CSOptions(
                obsoleteErrors                            = opts.obsoleteErrors.getOrElse(false),
                omitMostRecentVersionSuffixFromPaths      = generic.omitMostRecentVersionSuffixFromPaths.getOrElse(true),
                omitMostRecentVersionSuffixFromNamespaces = generic.omitMostRecentVersionSuffixFromNamespaces.getOrElse(true),
                wrappedAdtBranchCodecs                    = opts.wrappedAdtBranchCodecs.getOrElse(false),
                writeEvolutionDict                        = opts.writeEvolutionDict.getOrElse(false),
                disregardImplicitUsings                   = !opts.disregardImplicitUsings.getOrElse(false),
                enableDeprecatedEncoders                  = opts.enableDeprecatedEncoders.getOrElse(false),
                generateIndexWriters                      = opts.generateIndexWriters.getOrElse(true),
                generateJsonCodecs                        = opts.generateJsonCodecs.getOrElse(true),
                generateUebaCodecs                        = opts.generateUebaCodecs.getOrElse(true),
                generateJsonCodecsByDefault               = opts.generateJsonCodecsByDefault.getOrElse(false),
                generateUebaCodecsByDefault               = opts.generateUebaCodecsByDefault.getOrElse(false),
                deduplicate                               = opts.deduplicate.getOrElse(true),
              ),
            )
          case "scala" =>
            val opts = target.scala.getOrElse(js.Dynamic.literal().asInstanceOf[JSScOptions])
            CompilerTargetJS.ScTarget(
              id      = "Scala",
              output  = createOutputOptions(target.generic),
              generic = createGenericOptions(target.generic),
              language = ScOptions(
                writeEvolutionDict     = opts.writeEvolutionDict.getOrElse(false),
                wrappedAdtBranchCodecs = opts.wrappedAdtBranchCodecs.getOrElse(false),
              ),
            )
          case other => throw new IllegalArgumentException(s"Unknown target language: $other")
        }
    }
  }

  /**
    * Compile Baboon models (async)
    *
    * @param options Compilation options as JS object
    * @return JS Promise that resolves to compilation result with generated files or errors
    */
  @JSExport
  def compile(options: JSCompilerOptions): js.Promise[JSCompilationResult] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

    try {
      val inputs = options.inputs.toSeq.map {
        input =>
          BaboonParser.Input(
            FSPath.parse(NEString.unsafeFrom(input.path)),
            input.content,
          )
      }

      val targets = createTargets(options.targets)
      val debug   = options.debug.getOrElse(false)

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val resultFuture = compileInternal[F](inputs, targets, debug)

      resultFuture.map {
        files =>
          val jsFiles = js.Array(files.map {
            file =>
              createJSOutputFile(file.path, file.content, file.product.toString)
          }*)
          JSCompilationResult.success(jsFiles)
      }.recover {
        case e: Throwable =>
          JSCompilationResult.failure(js.Array(s"Compilation failed: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}"))
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future
          .successful(
            JSCompilationResult.failure(js.Array(s"Compilation failed: ${e.getMessage}\n${e.getStackTrace.mkString("\n")}"))
          ).toJSPromise
    }
  }

  case class OutputFileWithPath(path: String, content: String, product: CompilerProduct)

  private def compileInternal[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    inputs: Seq[BaboonParser.Input],
    targets: Seq[CompilerTargetJS],
    debug: Boolean,
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Future[Seq[OutputFileWithPath]] = {
    val logger = new BLoggerJS(debug)
    val m      = new BaboonModuleJS[F](targets, logger, ParallelErrorAccumulatingOps2[F])

    runner.runFuture(
      Injector
        .NoCycles[F[Throwable, _]]()
        .produceRun(m) {
          (loader: BaboonLoaderJS[F], logger: BLogger, loc: Locator) =>
            (for {
              loadedModels <- loader.load(inputs.toList)
              files <- F.traverse(targets) {
                target =>
                  processTarget[F](loc, logger, loadedModels, target)
              }
            } yield files.flatten).leftMap(issues => new RuntimeException(s"Failure: $issues"))
        }
    )
  }

  private def processTarget[F[+_, +_]: Error2: MaybeSuspend2: TagKK](
    loc: Locator,
    logger: BLogger,
    model: BaboonFamily,
    target: CompilerTargetJS,
  )(implicit
    q: QuasiIO[F[Throwable, _]],
    m: DefaultModule[F[Throwable, _]],
  ): F[Throwable, Seq[OutputFileWithPath]] = {
    val module = target match {
      case t: CompilerTargetJS.CSTarget =>
        new BaboonCSModuleJS[F](t)
      case t: CompilerTargetJS.ScTarget =>
        new BaboonScModuleJS[F](t)
    }

    Injector
      .NoCycles(parent = Some(loc))
      .produceRun(module) {
        (translator: BaboonAbstractTranslator[F]) =>
          (for {
            _ <- F.maybeSuspend {
              logger.message(s"${target.id}: generating output...")
            }
            sources <- translator.translate(model)
            files = sources.files.filter { case (_, file) => target.output.products.contains(file.product) }.map {
              case (path, file) => OutputFileWithPath(path, file.content, file.product)
            }.toSeq
            _ <- F.maybeSuspend {
              logger.message(s"${target.id}: done, ${files.size} files generated")
            }
          } yield files).leftMap(issues => new RuntimeException(s"Failure: $issues"))
      }

  }
}

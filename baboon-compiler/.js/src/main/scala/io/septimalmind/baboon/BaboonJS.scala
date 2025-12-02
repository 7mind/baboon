package io.septimalmind.baboon

import distage.*
import io.circe.Json
import io.circe.parser.parse as parseJson
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.BaboonRuntimeCodec
import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, Version}
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
    * Opaque handle for loaded Baboon model
    */
  trait BaboonLoadedModel extends js.Object

  class BaboonLoadedModelImpl(val family: BaboonFamily) extends BaboonLoadedModel

  /**
    * Codec encode result
    */
  trait JSEncodeResult extends js.Object {
    val success: Boolean
    val data: js.UndefOr[js.typedarray.Uint8Array]
    val error: js.UndefOr[String]
  }

  object JSEncodeResult {
    def success(data: js.typedarray.Uint8Array): JSEncodeResult = {
      js.Dynamic
        .literal(
          success = true,
          data    = data,
        ).asInstanceOf[JSEncodeResult]
    }

    def failure(error: String): JSEncodeResult = {
      js.Dynamic
        .literal(
          success = false,
          error   = error,
        ).asInstanceOf[JSEncodeResult]
    }
  }

  /**
    * Codec decode result
    */
  trait JSDecodeResult extends js.Object {
    val success: Boolean
    val json: js.UndefOr[String]
    val error: js.UndefOr[String]
  }

  object JSDecodeResult {
    def success(json: String): JSDecodeResult = {
      js.Dynamic
        .literal(
          success = true,
          json    = json,
        ).asInstanceOf[JSDecodeResult]
    }

    def failure(error: String): JSDecodeResult = {
      js.Dynamic
        .literal(
          success = false,
          error   = error,
        ).asInstanceOf[JSDecodeResult]
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

  private def collectInputDirectories(inputs: Seq[BaboonParser.Input]): Set[FSPath] = {
    inputs.flatMap {
      input =>
        val parentSegments = input.path.segments.dropRight(1)
        if (parentSegments.nonEmpty) {
          Some(FSPath(parentSegments))
        } else {
          None
        }
    }.toSet
  }

  private def buildOutputPaths(output: OutputOptionsJS): (FSPath, Option[FSPath], Option[FSPath]) = {
    val mainOut     = FSPath.parse(NEString.unsafeFrom("generated"))
    val fixturesOut = if (output.generateFixtures) Some(FSPath.parse(NEString.unsafeFrom("generated-fixtures"))) else None
    val testsOut    = if (output.generateTests) Some(FSPath.parse(NEString.unsafeFrom("generated-tests"))) else None
    (mainOut, fixturesOut, testsOut)
  }

  private def toOutputOptions(output: OutputOptionsJS): OutputOptions = {
    val (mainOut, fixturesOut, testsOut) = buildOutputPaths(output)
    OutputOptions(
      safeToRemoveExtensions = output.safeToRemoveExtensions,
      runtime = output.runtime,
      generateConversions = output.generateConversions,
      output = mainOut,
      fixturesOutput = fixturesOut,
      testsOutput = testsOut,
    )
  }

  private def toCSTarget(target: CompilerTargetJS.CSTarget): CompilerTarget.CSTarget = {
    CompilerTarget.CSTarget(
      id      = target.id,
      output  = toOutputOptions(target.output),
      generic = target.generic,
      language = target.language,
    )
  }

  private def toScTarget(target: CompilerTargetJS.ScTarget): CompilerTarget.ScTarget = {
    CompilerTarget.ScTarget(
      id      = target.id,
      output  = toOutputOptions(target.output),
      generic = target.generic,
      language = target.language,
    )
  }

  private def toCompilerTargets(targets: Seq[CompilerTargetJS]): Seq[CompilerTarget] = {
    targets.map {
      case t: CompilerTargetJS.CSTarget => toCSTarget(t)
      case t: CompilerTargetJS.ScTarget => toScTarget(t)
    }
  }

  private def createCompilerOptions(
    inputs: Seq[BaboonParser.Input],
    targets: Seq[CompilerTargetJS],
    debug: Boolean,
  ): CompilerOptions = {
    CompilerOptions(
      individualInputs = inputs.map(_.path).toSet,
      directoryInputs = collectInputDirectories(inputs),
      lockFile = None,
      debug = debug,
      targets = toCompilerTargets(targets),
      metaWriteEvolutionJsonTo = None,
    )
  }

  /**
    * Load Baboon model from files (async)
    */
  @JSExport
  def load(files: js.Dictionary[String]): js.Promise[BaboonLoadedModel] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue
    try {
      val inputs = files.toSeq.map {
        case (path, content) =>
          BaboonParser.Input(
            FSPath.parse(NEString.unsafeFrom(path)),
            content,
          )
      }

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val resultFuture = loadInternal[F](inputs)

      resultFuture.map {
        family =>
          new BaboonLoadedModelImpl(family)
      }.recover {
        case e: Throwable =>
          throw new RuntimeException(s"Loading failed: ${e.getMessage}")
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future.failed(new RuntimeException(s"Loading failed: ${e.getMessage}")).toJSPromise
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

  private def loadInternal[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    inputs: Seq[BaboonParser.Input]
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Future[BaboonFamily] = {
    val logger = new BLoggerJS(false)
    val compilerOptions = createCompilerOptions(inputs, Seq.empty, debug = false)
    val m = new BaboonModuleJS[F](inputs.toList, logger, ParallelErrorAccumulatingOps2[F], compilerOptions)

    runner.runFuture(
      Injector
        .NoCycles[F[Throwable, _]]()
        .produceRun(m) {
          (loader: BaboonLoaderJS[F]) =>
            (for {
              family <- loader.load(inputs.toList)
            } yield family).leftMap(issues => new RuntimeException(s"Loading failure: $issues"))
        }
    )
  }

  private def compileInternal[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    inputs: Seq[BaboonParser.Input],
    targets: Seq[CompilerTargetJS],
    debug: Boolean,
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Future[Seq[OutputFileWithPath]] = {
    val logger = new BLoggerJS(debug)
    val compilerOptions = createCompilerOptions(inputs, targets, debug)
    val m = new BaboonModuleJS[F](inputs, logger, ParallelErrorAccumulatingOps2[F], compilerOptions)

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
        new BaboonJsCSModule[F](toCSTarget(t))
      case t: CompilerTargetJS.ScTarget =>
        new BaboonJsScModule[F](toScTarget(t))
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

  /**
    * Encode JSON to binary format using runtime codec (async)
    *
    * @param files Map of baboon files (path -> content)
    * @param pkg Package name (e.g., "io.example.models")
    * @param version Version string (e.g., "1.0.0")
    * @param idString Type identifier
    * @param json JSON string to encode
    * @param indexed Whether to use indexed encoding
    * @return JS Promise that resolves to encode result with binary data or error
    */
  @JSExport
  def encode(
    files: js.Dictionary[String],
    pkg: String,
    version: String,
    idString: String,
    json: String,
    indexed: Boolean,
  ): js.Promise[JSEncodeResult] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

    try {
      val inputs = files.toSeq.map {
        case (path, content) =>
          BaboonParser.Input(
            FSPath.parse(NEString.unsafeFrom(path)),
            content,
          )
      }

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val resultFuture = encodeInternal[F](Left(inputs), pkg, version, idString, json, indexed)

      resultFuture.map {
        data =>
          val jsArray    = js.Array[Short](data.map(b => (b & 0xFF).toShort)*)
          val uint8Array = new js.typedarray.Uint8Array(jsArray.length)
          jsArray.indices.foreach(i => uint8Array(i) = jsArray(i))
          JSEncodeResult.success(uint8Array)
      }.recover {
        case e: Throwable =>
          JSEncodeResult.failure(s"Encoding failed: ${e.getMessage}")
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future
          .successful(
            JSEncodeResult.failure(s"Encoding failed: ${e.getMessage}")
          ).toJSPromise
    }
  }

  /**
    * Encode JSON to binary format using runtime codec with loaded model (async)
    *
    * @param model Loaded Baboon model
    * @param pkg Package name (e.g., "io.example.models")
    * @param version Version string (e.g., "1.0.0")
    * @param idString Type identifier
    * @param json JSON string to encode
    * @param indexed Whether to use indexed encoding
    * @return JS Promise that resolves to encode result with binary data or error
    */
  @JSExport
  def encodeLoaded(
    model: BaboonLoadedModel,
    pkg: String,
    version: String,
    idString: String,
    json: String,
    indexed: Boolean,
  ): js.Promise[JSEncodeResult] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

    try {
      val family = model.asInstanceOf[BaboonLoadedModelImpl].family

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val resultFuture = encodeInternal[F](Right(family), pkg, version, idString, json, indexed)

      resultFuture.map {
        data =>
          val jsArray    = js.Array[Short](data.map(b => (b & 0xFF).toShort)*)
          val uint8Array = new js.typedarray.Uint8Array(jsArray.length)
          jsArray.indices.foreach(i => uint8Array(i) = jsArray(i))
          JSEncodeResult.success(uint8Array)
      }.recover {
        case e: Throwable =>
          JSEncodeResult.failure(s"Encoding failed: ${e.getMessage}")
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future
          .successful(
            JSEncodeResult.failure(s"Encoding failed: ${e.getMessage}")
          ).toJSPromise
    }
  }

  /**
    * Decode binary data to JSON using runtime codec (async)
    *
    * @param files Map of baboon files (path -> content)
    * @param pkg Package name (e.g., "io.example.models")
    * @param version Version string (e.g., "1.0.0")
    * @param idString Type identifier
    * @param data Binary data to decode
    * @return JS Promise that resolves to decode result with JSON string or error
    */
  @JSExport
  def decode(
    files: js.Dictionary[String],
    pkg: String,
    version: String,
    idString: String,
    data: js.typedarray.Uint8Array,
  ): js.Promise[JSDecodeResult] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

    try {
      val inputs = files.toSeq.map {
        case (path, content) =>
          BaboonParser.Input(
            FSPath.parse(NEString.unsafeFrom(path)),
            content,
          )
      }

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val dataVector   = Vector.from((0 until data.length).map(i => data(i).toByte))
      val resultFuture = decodeInternal[F](Left(inputs), pkg, version, idString, dataVector)

      resultFuture.map {
        json =>
          JSDecodeResult.success(json.noSpaces)
      }.recover {
        case e: Throwable =>
          JSDecodeResult.failure(s"Decoding failed: ${e.getMessage}")
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future
          .successful(
            JSDecodeResult.failure(s"Decoding failed: ${e.getMessage}")
          ).toJSPromise
    }
  }

  /**
    * Decode binary data to JSON using runtime codec with loaded model (async)
    *
    * @param model Loaded Baboon model
    * @param pkg Package name (e.g., "io.example.models")
    * @param version Version string (e.g., "1.0.0")
    * @param idString Type identifier
    * @param data Binary data to decode
    * @return JS Promise that resolves to decode result with JSON string or error
    */
  @JSExport
  def decodeLoaded(
    model: BaboonLoadedModel,
    pkg: String,
    version: String,
    idString: String,
    data: js.typedarray.Uint8Array,
  ): js.Promise[JSDecodeResult] = {
    implicit val ec: ExecutionContext = scala.scalajs.concurrent.JSExecutionContext.queue

    try {
      val family = model.asInstanceOf[BaboonLoadedModelImpl].family

      import izumi.distage.modules.support.unsafe.EitherSupport.*
      import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

      type F[+E, +A] = Either[E, A]

      val dataVector   = Vector.from((0 until data.length).map(i => data(i).toByte))
      val resultFuture = decodeInternal[F](Right(family), pkg, version, idString, dataVector)

      resultFuture.map {
        json =>
          JSDecodeResult.success(json.noSpaces)
      }.recover {
        case e: Throwable =>
          JSDecodeResult.failure(s"Decoding failed: ${e.getMessage}")
      }.toJSPromise
    } catch {
      case e: Throwable =>
        Future
          .successful(
            JSDecodeResult.failure(s"Decoding failed: ${e.getMessage}")
          ).toJSPromise
    }
  }

  private def encodeInternal[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    input: Either[Seq[BaboonParser.Input], BaboonFamily],
    pkgString: String,
    versionString: String,
    idString: String,
    jsonString: String,
    indexed: Boolean,
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Future[Vector[Byte]] = {
    val pkg     = parsePkg(pkgString)
    val version = Version.parse(versionString)

    input match {
      case Left(inputs) =>
        val logger          = new BLoggerJS(false)
        val compilerOptions = createCompilerOptions(inputs, Seq.empty, debug = false)
        val m               = new BaboonModuleJS[F](inputs.toList, logger, ParallelErrorAccumulatingOps2[F], compilerOptions)
        runner.runFuture(
          Injector
            .NoCycles[F[Throwable, _]]()
            .produceRun(m) {
              (loader: BaboonLoaderJS[F], codec: BaboonRuntimeCodec[F]) =>
                (for {
                  family <- loader.load(inputs.toList)
                  json   <- Error2[F].fromEither(parseJson(jsonString).left.map(e => new RuntimeException(s"Invalid JSON: ${e.getMessage}")))
                  result <- codec.encode(family, pkg, version, idString, json, indexed)
                } yield result).leftMap(issues => new RuntimeException(s"Encoding failure: $issues"))
            }
        )
      case Right(family) =>
        val m = new BaboonCodecModuleJS[F](ParallelErrorAccumulatingOps2[F])
        runner.runFuture(
          Injector
            .NoCycles[F[Throwable, _]]()
            .produceRun(m) {
              (codec: BaboonRuntimeCodec[F]) =>
                (for {
                  json   <- Error2[F].fromEither(parseJson(jsonString).left.map(e => new RuntimeException(s"Invalid JSON: ${e.getMessage}")))
                  result <- codec.encode(family, pkg, version, idString, json, indexed)
                } yield result).leftMap(issues => new RuntimeException(s"Encoding failure: $issues"))
            }
        )
    }
  }

  private def decodeInternal[F[+_, +_]: Error2: MaybeSuspend2: ParallelErrorAccumulatingOps2: TagKK: DefaultModule2](
    input: Either[Seq[BaboonParser.Input], BaboonFamily],
    pkgString: String,
    versionString: String,
    idString: String,
    data: Vector[Byte],
  )(implicit
    quasiIO: QuasiIO[F[Throwable, _]],
    runner: QuasiIORunner[F[Throwable, _]],
  ): Future[Json] = {
    val pkg     = parsePkg(pkgString)
    val version = Version.parse(versionString)

    input match {
      case Left(inputs) =>
        val logger          = new BLoggerJS(false)
        val compilerOptions = createCompilerOptions(inputs, Seq.empty, debug = false)
        val m               = new BaboonModuleJS[F](inputs.toList, logger, ParallelErrorAccumulatingOps2[F], compilerOptions)
        runner.runFuture(
          Injector
            .NoCycles[F[Throwable, _]]()
            .produceRun(m) {
              (loader: BaboonLoaderJS[F], codec: BaboonRuntimeCodec[F]) =>
                (for {
                  family <- loader.load(inputs.toList)
                  result <- codec.decode(family, pkg, version, idString, data)
                } yield result).leftMap(issues => new RuntimeException(s"Decoding failure: $issues"))
            }
        )
      case Right(family) =>
        val m = new BaboonCodecModuleJS[F](ParallelErrorAccumulatingOps2[F])
        runner.runFuture(
          Injector
            .NoCycles[F[Throwable, _]]()
            .produceRun(m) {
              (codec: BaboonRuntimeCodec[F]) =>
                (for {
                  result <- codec.decode(family, pkg, version, idString, data)
                } yield result).leftMap(issues => new RuntimeException(s"Decoding failure: $issues"))
            }
        )
    }
  }

  private def parsePkg(pkgString: String): Pkg = {
    val parts = pkgString.split("\\.").toList
    Pkg(NEList.unsafeFrom(parts))
  }
}

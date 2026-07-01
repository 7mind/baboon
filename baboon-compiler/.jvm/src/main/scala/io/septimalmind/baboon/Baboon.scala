package io.septimalmind.baboon

import caseapp.*
import distage.*
import io.septimalmind.baboon.explore.{ExploreContext, ExploreInputs, ExploreShell}
import io.septimalmind.baboon.lsp._
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps
import io.septimalmind.baboon.BaboonModeAxis
import io.septimalmind.baboon.diff.BaboonDiffRenderer
import io.septimalmind.baboon.scheme.BaboonSchemeRenderer
import io.septimalmind.baboon.typer.{BaboonComparator, BaboonEnquiries}
import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, Version}
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

  /** Names of args inherited from GenericTranspilerCLIOptions, SharedCLIOptions, and ScalaHktCLIOptions
    * (camelCase, matching case-app's Arg.name). These are documented in the "Common transpiler options" /
    * "Service generation options" / "HKT options" sections of the help text.
    */
  private val sharedArgNames: Set[String] = Set(
    // GenericTranspilerCLIOptions
    "output",
    "fixtureOutput",
    "testOutput",
    "runtime",
    "disableConversions",
    "omitMostRecentVersionSuffixFromPaths",
    "omitMostRecentVersionSuffixFromNamespaces",
    "codecTestIterations",
    // SharedCLIOptions
    "extAllowCleanup",
    "serviceResultNoErrors",
    "serviceResultType",
    "serviceResultPattern",
    "serviceContextMode",
    "serviceContextType",
    "serviceContextParameterName",
    "pragma",
    "generateJsonCodecs",
    "generateUebaCodecs",
    "generateJsonCodecsByDefault",
    "generateUebaCodecsByDefault",
    "enableDeprecatedEncoders",
    // ScalaHktCLIOptions
    "serviceResultHkt",
    "serviceResultHktName",
    "serviceResultHktSignature",
  )

  private def camelToKebab(s: String): String = {
    val sb = new StringBuilder
    s.foreach {
      c =>
        if (c.isUpper && sb.nonEmpty) sb.append('-')
        sb.append(c.toLower)
    }
    sb.toString
  }

  private def formatLanguageHelp[T](label: String, modality: String)(implicit help: caseapp.core.help.Help[T]): String = {
    val languageArgs = help.args
      .filterNot(_.noHelp)
      .filterNot(a => sharedArgNames.contains(a.name.name))

    if (languageArgs.isEmpty) return s"$label options (:$modality):\n  (no language-specific options)\n"

    val lines = languageArgs.map {
      arg =>
        val flag = s"--${camelToKebab(arg.name.name)}"
        val value = arg.valueDescription
          .map(_.description)
          .filterNot(d => d == "true/false" || d == "bool?" || d == "boolean")
          .fold("")(v => s" <$v>")
        val desc = arg.helpMessage.fold("")(_.message)
        s"  $flag$value  $desc"
    }
    s"$label options (:$modality):\n${lines.mkString("\n")}\n"
  }

  private val helpText: String = {
    import caseapp.core.help.Help

    val languageSections = Seq(
      formatLanguageHelp[CsCLIOptions]("C#", "cs"),
      formatLanguageHelp[ScCLIOptions]("Scala", "scala"),
      formatLanguageHelp[PyCLIOptions]("Python", "python"),
      formatLanguageHelp[RsCLIOptions]("Rust", "rust"),
      formatLanguageHelp[TsCLIOptions]("TypeScript", "typescript"),
      formatLanguageHelp[KtCLIOptions]("Kotlin", "kotlin"),
      formatLanguageHelp[JvCLIOptions]("Java", "java"),
      formatLanguageHelp[DtCLIOptions]("Dart", "dart"),
      formatLanguageHelp[SwCLIOptions]("Swift", "swift"),
      formatLanguageHelp[GqlCLIOptions]("GraphQL", "graphql"),
      formatLanguageHelp[OasCLIOptions]("OpenAPI", "openapi"),
    ).mkString("\n")

    s"""Usage: baboon [options] <modality> [modality-options] [<modality> [modality-options] ...]
       |
       |Global options:
       |  --model <file>           A *.baboon file to process (can be repeated)
       |  --model-dir <dir>        A directory to recursively read *.baboon files from (can be repeated)
       |  --lock-file <file>       A file used to track model signatures
       |  --lockfile-update {create-only|force}                   Lockfile update policy (default: create-only; requires --lock-file)
       |  --lockfile-enforcement {none|legacy-versions|all-versions}  Lockfile enforcement scope (default: legacy-versions; requires --lock-file)
       |  --meta-write-evolution-json <file>  Write evolution metadata as JSON
       |  --emit-only <domains>    Comma-separated list of domain names to generate code for (all are still parsed/typed)
       |  --debug                  Enable debug output
       |  --help                   Show this help message
       |
       |Modalities:
       |  :cs                      Generate C# code
       |  :scala                   Generate Scala code
       |  :python                  Generate Python code
       |  :rust                    Generate Rust code
       |  :typescript              Generate TypeScript code
       |  :kotlin                  Generate Kotlin code
       |  :java                    Generate Java 21 code
       |  :dart                    Generate Dart 3+ code
       |  :swift                   Generate Swift 5.9+ code
       |  :graphql                 Generate GraphQL SDL schemas
       |  :openapi                 Generate OpenAPI 3.1 component schemas
       |  :lsp                     Start LSP server
       |  :explore                 Start interactive explorer
       |  :scheme                  Emit a cleaned-up single .baboon file for a domain version
       |  :diff                    Report the schema diff between two versions of a domain
       |
       |Common transpiler options (apply to all language modalities):
       |  --output <dir>           Output directory for generated code (required)
       |  --fixture-output <dir>   Output directory for generated fixtures
       |  --test-output <dir>      Output directory for generated tests
       |  --runtime <with|only|without>  Runtime generation mode (default: with)
       |  --disable-conversions    Do not generate conversions (default: false)
       |  --omit-most-recent-version-suffix-from-paths       Remove version segment from paths for most recent version
       |  --omit-most-recent-version-suffix-from-namespaces  Remove version segment from namespaces for most recent version
       |  --codec-test-iterations <n>  Number of iterations for generated codec tests (default: 500)
       |  --ext-allow-cleanup <ext>    File extensions safe to delete during cleanup (can be repeated)
       |  --generate-json-codecs          Generate JSON codecs (default: true)
       |  --generate-ueba-codecs          Generate UEBA binary codecs (default: true)
       |  --generate-json-codecs-by-default  Generate JSON codecs even for types without derived[json]
       |  --generate-ueba-codecs-by-default  Generate UEBA codecs even for types without derived[ueba]
       |  --enable-deprecated-encoders    Generate encoders for deprecated versions (CS, Scala, Python, Kotlin, Java)
       |  --pragma <key=value>     Set a pragma value (can be repeated)
       |
       |Service generation options (apply to all language modalities):
       |  --service-result-no-errors           Methods return success type only, no error wrapping
       |  --service-result-type <type>         Wrapper type for service results (e.g. 'Either')
       |  --service-result-pattern <pattern>   Pattern for result type (e.g. '<$$error, $$success>')
       |  --service-context-mode <mode>        Context parameter mode: none, abstract, type (default: none)
       |  --service-context-type <name>        Context type name (default: Ctx)
       |  --service-context-parameter-name <name>  Context parameter name (default: ctx)
       |
       |HKT options (Scala, Kotlin only):
       |  --service-result-hkt               Use HKT type parameter for service result
       |  --service-result-hkt-name <name>   HKT type parameter name (e.g. 'F')
       |  --service-result-hkt-signature <sig>  HKT type parameter signature (e.g. '[+_, +_]')
       |
       |$languageSections
       |Scheme options (:scheme):
       |  --domain <name>          Domain name (e.g., 'my.domain.name')
       |  --version <version>      Version string (e.g., '1.0.0')
       |  --target <file>          Target output file path
       |
       |Diff options (:diff):
       |  --domain <name>          Domain name (e.g., 'my.domain.name')
       |  --from <version>         Older version (e.g., '1.0.0')
       |  --to <version>           Newer version (e.g., '2.0.0')
       |  --target <file>          Target output file path (when absent, the diff is printed to stdout)
       |  --format {text|json}     Output format (default: text)
       |
       |LSP options (:lsp):
       |  --port <port>            TCP port to listen on (default: stdio)
       |
       |Examples:
       |  baboon --model-dir ./models :cs --output ./out/cs :scala --output ./out/scala
       |  baboon --model-dir ./models :cs --output ./out/cs --generate-json-codecs-by-default=true
       |  baboon --model-dir ./models :lsp
       |  baboon --model-dir ./models :lsp --port 5007
       |  baboon --model-dir ./models :explore
       |  baboon --model-dir ./models :scheme --domain=my.pkg --version=1.0.0 --target=./cleaned.baboon
       |  baboon --model-dir ./models :diff --domain=my.pkg --from=1.0.0 --to=2.0.0 --format=json
       |""".stripMargin
  }

  /** Maps the parsed `--lockfile-update` / `--lockfile-enforcement` strings to their enums and
    * enforces the two CLI invariants:
    *   - Q37b: either flag supplied without `--lock-file` is a hard CLI error (fail-fast).
    *   - Q37c: an unrecognized enum value is a hard CLI error, surfaced via the parse helper's Left.
    * Absent flags fall back to the production defaults (CreateOnly / LegacyVersions).
    */
  private[baboon] def parseLockfileOptions(
    lockFile: Option[String],
    lockfileUpdate: Option[String],
    lockfileEnforcement: Option[String],
  ): Either[NEList[String], (LockfileUpdate, LockfileEnforcement)] = {
    val errors = scala.collection.mutable.ListBuffer.empty[String]

    if (lockFile.isEmpty && (lockfileUpdate.isDefined || lockfileEnforcement.isDefined)) {
      errors += "--lockfile-update and --lockfile-enforcement require --lock-file to be set"
    }

    val update = lockfileUpdate match {
      case None    => LockfileUpdate.CreateOnly
      case Some(s) =>
        LockfileUpdate.parse(s) match {
          case Right(v)  => v
          case Left(msg) => errors += msg; LockfileUpdate.CreateOnly
        }
    }

    val enforcement = lockfileEnforcement match {
      case None    => LockfileEnforcement.LegacyVersions
      case Some(s) =>
        LockfileEnforcement.parse(s) match {
          case Right(v)  => v
          case Left(msg) => errors += msg; LockfileEnforcement.LegacyVersions
        }
    }

    NEList.from(errors.toList) match {
      case Some(nel) => Left(nel)
      case None      => Right((update, enforcement))
    }
  }

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
        val isSchemeMode  = modalities.exists { case ModalityArgs(id, _) => id == "scheme" }
        val isDiffMode    = modalities.exists { case ModalityArgs(id, _) => id == "diff" }
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
        } else if (isSchemeMode) {
          val schemeModality = modalities.find(_.id == "scheme").get
          val out = for {
            generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => NEList(s"Can't parse generic CLI: $e"))
            schemeOptions  <- CaseApp.parse[SchemeCLIOptions](schemeModality.args).leftMap(e => NEList(s"Can't parse scheme CLI: $e"))
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
            import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

            schemeEntrypoint(directoryInputs, individualInputs, schemeOptions._1)
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
        } else if (isDiffMode) {
          val diffModality = modalities.find(_.id == "diff").get
          val out = for {
            generalOptions <- CaseApp.parse[CLIOptions](generalArgs).leftMap(e => NEList(s"Can't parse generic CLI: $e"))
            diffOptions    <- CaseApp.parse[DiffCLIOptions](diffModality.args).leftMap(e => NEList(s"Can't parse diff CLI: $e"))
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            import izumi.distage.modules.support.unsafe.EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}
            import izumi.functional.bio.unsafe.UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither

            diffEntrypoint(directoryInputs, individualInputs, diffOptions._1)
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
                            generateDomainFacade                      = opts.generateDomainFacade.getOrElse(true),
                            serviceResult                             = mkServiceResult(opts, ServiceResultConfig.csDefault),
                            serviceContext                            = mkServiceContext(opts),
                            pragmas                                   = parsePragmas(opts.pragma),
                            asyncServices                             = opts.csAsyncServices.getOrElse(false),
                            generateMcpServer                         = opts.csGenerateMcpServer.getOrElse(false),
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
                            // MFACADE-PR-6: scaffold default false; cs is the proven-good pilot.
                            // Per-backend codegen has rough edges that need follow-up — opt-in only.
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.scalaDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            generateMcpServer           = opts.scalaGenerateMcpServer.getOrElse(false),
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
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.pythonDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.pyAsyncServices.getOrElse(false),
                            generateMcpServer           = opts.pyGenerateMcpServer.getOrElse(false),
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
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.rustDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.rsAsyncServices.getOrElse(false),
                            cratePrefix                 = opts.rsCratePrefix.getOrElse("crate"),
                            reexportMode                = opts.rsReexportMode.getOrElse("selective"),
                            edition                     = opts.rsEdition.getOrElse("2024"),
                            generateMcpServer           = opts.rsGenerateMcpServer.getOrElse(false),
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
                            importSuffix                = opts.tsImportSuffix.getOrElse(""),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.typescriptDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.tsAsyncServices.getOrElse(false),
                            bareServiceSymbols          = opts.tsBareServiceSymbols.getOrElse(false),
                            mapsAsRecords               = opts.tsMapsAsRecords.getOrElse(false),
                            timestampsUtcMode =
                              if (opts.tsTimestampsAsStrings.getOrElse(false)) "string"
                              else if (opts.tsTimestampsAsDates.getOrElse(false)) "date"
                              else "wrapper",
                            timestampsOffsetMode =
                              if (opts.tsTimestampsAsStrings.getOrElse(false)) "string"
                              else if (opts.tsTimestampsAsDates.getOrElse(false)) "date"
                              else "wrapper",
                            enumLowercaseValues = opts.tsEnumLowercaseValues.getOrElse(false),
                            generateMcpServer   = opts.tsGenerateMcpServer.getOrElse(false),
                          ),
                        )
                    }
                  case "kotlin" =>
                    CaseApp.parse[KtCLIOptions](roleArgs).leftMap(e => s"Can't parse kotlin CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.KtTarget(
                          id      = "Kotlin",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = KtOptions(
                            writeEvolutionDict          = opts.ktWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.ktWrappedAdtBranchCodecs.getOrElse(false),
                            enableDeprecatedEncoders    = opts.enableDeprecatedEncoders.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.kotlinDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.ktAsyncServices.getOrElse(false),
                            multiplatform               = opts.ktMultiplatform.getOrElse(false),
                            generateMcpServer           = opts.ktGenerateMcpServer.getOrElse(false),
                          ),
                        )
                    }
                  case "java" =>
                    CaseApp.parse[JvCLIOptions](roleArgs).leftMap(e => s"Can't parse java CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.JvTarget(
                          id      = "Java",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = JvOptions(
                            writeEvolutionDict          = opts.jvWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.jvWrappedAdtBranchCodecs.getOrElse(false),
                            enableDeprecatedEncoders    = opts.enableDeprecatedEncoders.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.javaDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.jvAsyncServices.getOrElse(false),
                            generateMcpServer           = opts.jvGenerateMcpServer.getOrElse(false),
                          ),
                        )
                    }
                  case "dart" =>
                    CaseApp.parse[DtCLIOptions](roleArgs).leftMap(e => s"Can't parse dart CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.DtTarget(
                          id      = "Dart",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = DtOptions(
                            writeEvolutionDict          = opts.dtWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.dtWrappedAdtBranchCodecs.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.dartDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.dtAsyncServices.getOrElse(false),
                            generateMcpServer           = opts.dtGenerateMcpServer.getOrElse(false),
                          ),
                        )
                    }
                  case "swift" =>
                    CaseApp.parse[SwCLIOptions](roleArgs).leftMap(e => s"Can't parse swift CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.SwTarget(
                          id      = "Swift",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = SwOptions(
                            writeEvolutionDict          = opts.swWriteEvolutionDict.getOrElse(false),
                            wrappedAdtBranchCodecs      = opts.swWrappedAdtBranchCodecs.getOrElse(false),
                            generateJsonCodecs          = opts.generateJsonCodecs.getOrElse(true),
                            generateUebaCodecs          = opts.generateUebaCodecs.getOrElse(true),
                            generateJsonCodecsByDefault = opts.generateJsonCodecsByDefault.getOrElse(false),
                            generateUebaCodecsByDefault = opts.generateUebaCodecsByDefault.getOrElse(false),
                            generateDomainFacade        = opts.generateDomainFacade.getOrElse(true),
                            serviceResult               = mkServiceResult(opts, ServiceResultConfig.swiftDefault),
                            serviceContext              = mkServiceContext(opts),
                            pragmas                     = parsePragmas(opts.pragma),
                            asyncServices               = opts.swAsyncServices.getOrElse(false),
                            generateMcpServer           = opts.swGenerateMcpServer.getOrElse(false),
                          ),
                        )
                    }
                  case "graphql" =>
                    CaseApp.parse[GqlCLIOptions](roleArgs).leftMap(e => s"Can't parse graphql CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.GqlTarget(
                          id      = "GraphQL",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = GqlOptions(
                            pragmas = parsePragmas(opts.pragma)
                          ),
                        )
                    }
                  case "openapi" =>
                    CaseApp.parse[OasCLIOptions](roleArgs).leftMap(e => s"Can't parse openapi CLI: $e").map {
                      case (opts, _) =>
                        val shopts = mkGenericOpts(opts)

                        CompilerTarget.OasTarget(
                          id      = "OpenAPI",
                          output  = shopts.outOpts,
                          generic = shopts.genericOpts,
                          language = OasOptions(
                            pragmas = parsePragmas(opts.pragma)
                          ),
                        )
                    }
                  case r => Left(s"Unknown role id: $r")
                }
            }
            lockfileOptions <- parseLockfileOptions(
              generalOptions._1.lockFile,
              generalOptions._1.lockfileUpdate,
              generalOptions._1.lockfileEnforcement,
            )
          } yield {
            val directoryInputs  = generalOptions._1.modelDir.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet
            val individualInputs = generalOptions._1.model.map(s => FSPath.parse(NEString.unsafeFrom(s))).toSet

            val emitOnly = generalOptions._1.emitOnly.map {
              raw =>
                raw
                  .split(',').map(_.trim).filter(_.nonEmpty).map {
                    s => Pkg(NEList.unsafeFrom(s.split('.').toList))
                  }.toSet
            }

            val options = CompilerOptions(
              debug                    = generalOptions._1.debug.getOrElse(false),
              individualInputs         = individualInputs,
              directoryInputs          = directoryInputs,
              targets                  = launchArgs,
              metaWriteEvolutionJsonTo = generalOptions._1.metaWriteEvolutionJson.map(s => FSPath.parse(NEString.unsafeFrom(s))),
              lockFile                 = generalOptions._1.lockFile.map(s => FSPath.parse(NEString.unsafeFrom(s))),
              emitOnly                 = emitOnly,
              lockfileUpdate           = lockfileOptions._1,
              lockfileEnforcement      = lockfileOptions._2,
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
      case None        => Set("meta", "cs", "json", "scala", "py", "pyc", "rs", "ts", "kt", "java", "dart", "swift", "toml", "graphql")
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
    raw.flatMap {
      s =>
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
        Some(
          HktConfig(
            name      = sc.serviceResultHktName.getOrElse("F"),
            signature = sc.serviceResultHktSignature.getOrElse("[+_, +_]"),
          )
        )
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
      case t: CompilerTarget.KtTarget =>
        new BaboonJvmKtModule[F](t)
      case t: CompilerTarget.JvTarget =>
        new BaboonJvmJvModule[F](t)
      case t: CompilerTarget.DtTarget =>
        new BaboonJvmDtModule[F](t)
      case t: CompilerTarget.SwTarget =>
        new BaboonJvmSwModule[F](t)
      case t: CompilerTarget.GqlTarget =>
        new BaboonJvmGqlModule[F](t)
      case t: CompilerTarget.OasTarget =>
        new BaboonJvmOasModule[F](t)
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

              // The lockfile is a global, target-independent artifact: enforce/update it EXACTLY ONCE per
              // invocation, BEFORE the per-target loop, so an enforcement failure short-circuits before any
              // target generates and a Force rewrite happens once, not once per target.
              lockfileManager = new LockfileManagerImpl[F](options, loc.get[BaboonEnquiries])
              _ <- lockfileManager.validateLock(loadedModels).catchAll {
                value =>
                  System.err.println("Lockfile validation failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(3)
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
      emitOnly                 = None,
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

  private def schemeEntrypoint(
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
    schemeOptions: SchemeCLIOptions,
  )(implicit
    quasiIO: QuasiIO[Either[Throwable, _]],
    runner: QuasiIORunner[Either[Throwable, _]],
    error2: Error2[EitherF],
    maybeSuspend2: MaybeSuspend2[EitherF],
    parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[EitherF],
    tag: TagKK[EitherF],
    defaultModule2: DefaultModule2[EitherF],
  ): Unit = {
    val pkg     = Pkg(NEList.unsafeFrom(schemeOptions.domain.split("\\.").toList))
    val version = Version.parse(schemeOptions.version)

    schemeOptions.target match {
      case Some(target) =>
        // TARGET PRESENT: Compiler axis (BLoggerImpl => stdout diagnostics + file write).
        schemeEntrypointToFile(directoryInputs, individualInputs, pkg, version, target)
      case None =>
        // TARGET ABSENT: Explorer axis (Noop BLogger) + scheme printed to Console.out.
        schemeEntrypointToStdout(directoryInputs, individualInputs, pkg, version)
    }
  }

  /** `:diff` seam, modeled on [[schemeEntrypoint]]. Loads the family, resolves the
    * domain lineage + both endpoint versions, compares newer-vs-older
    * (`compare(last = to, prev = from)` per Q47 from->to), and renders the whole-domain
    * diff via [[BaboonDiffRenderer]] (text default; JSON when `--format json`).
    * Writes to `--target` file if present (mkdirs + writeString, as schemeEntrypointToFile)
    * else prints to stdout. Missing domain/version exits nonzero with an actionable message
    * that lists what IS available.
    */
  private def diffEntrypoint(
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
    diffOptions: DiffCLIOptions,
  )(implicit
    quasiIO: QuasiIO[Either[Throwable, _]],
    runner: QuasiIORunner[Either[Throwable, _]],
    error2: Error2[EitherF],
    maybeSuspend2: MaybeSuspend2[EitherF],
    parallelAccumulatingOps2: ParallelErrorAccumulatingOps2[EitherF],
    tag: TagKK[EitherF],
    defaultModule2: DefaultModule2[EitherF],
  ): Unit = {
    val pkg         = Pkg(NEList.unsafeFrom(diffOptions.domain.split("\\.").toList))
    val fromVersion = Version.parse(diffOptions.from)
    val toVersion   = Version.parse(diffOptions.to)
    val emitJson    = diffOptions.format.contains("json")

    val options = CompilerOptions(
      debug                    = false,
      individualInputs         = individualInputs,
      directoryInputs          = directoryInputs,
      targets                  = Seq.empty,
      metaWriteEvolutionJsonTo = None,
      lockFile                 = None,
      emitOnly                 = None,
    )
    val m = new BaboonModuleJvm[EitherF](options, parallelAccumulatingOps2)
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[EitherF[Throwable, _]]()
        .produceRun(m, Activation(BaboonModeAxis.Compiler)) {
          (loader: BaboonLoader[EitherF], logger: BLogger, comparator: BaboonComparator[EitherF]) =>
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

              family <- loader.load(inputModels.toList).catchAll {
                value =>
                  System.err.println("Loader failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(4)
              }

              lineage <- F.maybeSuspend {
                family.domains.toMap.get(pkg) match {
                  case Some(l) => l
                  case None =>
                    val available = family.domains.toMap.keys.map(_.path.mkString(".")).toList.sorted
                    System.err.println(s"Domain not found: ${diffOptions.domain}")
                    System.err.println(s"Available domains: ${available.niceList()}")
                    sys.exit(5)
                }
              }

              versions = lineage.versions.toMap

              fromDomain <- F.maybeSuspend {
                versions.get(fromVersion) match {
                  case Some(d) => d
                  case None =>
                    val available = versions.keys.map(_.toString).toList.sorted
                    System.err.println(s"Version not found in domain ${diffOptions.domain}: ${diffOptions.from}")
                    System.err.println(s"Available versions: ${available.niceList()}")
                    sys.exit(5)
                }
              }

              toDomain <- F.maybeSuspend {
                versions.get(toVersion) match {
                  case Some(d) => d
                  case None =>
                    val available = versions.keys.map(_.toString).toList.sorted
                    System.err.println(s"Version not found in domain ${diffOptions.domain}: ${diffOptions.to}")
                    System.err.println(s"Available versions: ${available.niceList()}")
                    sys.exit(5)
                }
              }

              // Q47 from->to: the newer version is `last`, the older is `prev`.
              diff <- comparator.compare(last = toDomain, prev = fromDomain).catchAll {
                value =>
                  System.err.println("Comparison failed")
                  System.err.println(value.toList.stringifyIssues)
                  sys.exit(6)
              }

              renderer = new BaboonDiffRenderer
              result   = if (emitJson) renderer.renderJson(diff) else renderer.renderText(diff)

              _ <- F.maybeSuspend {
                diffOptions.target match {
                  case Some(target) =>
                    val targetPath = Paths.get(target)
                    val parent     = targetPath.getParent
                    if (parent != null) {
                      java.nio.file.Files.createDirectories(parent)
                    }
                    java.nio.file.Files.writeString(targetPath, result)
                    logger.message(s"Diff written to: ${targetPath.toAbsolutePath}")
                  case None =>
                    println(result)
                }
              }
            } yield {}
        }
    }
  }

  private def schemeEntrypointToFile(
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
    pkg: Pkg,
    version: Version,
    target: String,
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
      emitOnly                 = None,
    )
    val m = new BaboonModuleJvm[EitherF](options, parallelAccumulatingOps2)
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[EitherF[Throwable, _]]()
        .produceRun(m, Activation(BaboonModeAxis.Compiler)) {
          (loader: BaboonLoader[EitherF], logger: BLogger, renderer: BaboonSchemeRenderer) =>
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

              result <- F.fromEither(renderer.render(loadedModels, pkg, version).left.map(e => new RuntimeException(e)))

              _ <- F.maybeSuspend {
                val targetPath = Paths.get(target)
                val parent     = targetPath.getParent
                if (parent != null) {
                  java.nio.file.Files.createDirectories(parent)
                }
                java.nio.file.Files.writeString(targetPath, result)
                logger.message(s"Scheme written to: ${targetPath.toAbsolutePath}")
              }
            } yield {}
        }
    }
  }

  /** Absent-target scheme seam, factored out (T175) so an in-process test can drive the REAL
    * `produceRun` (Explorer axis) WITHOUT launching an OS process, and capture BOTH the logger's
    * output and the rendered scheme in ONE buffer.
    *
    * The rendered scheme is printed via `Console.println`, i.e. the dynamically-rebindable
    * `scala.Console.out` — the SAME stream `BLoggerImpl.writeLine` writes to (BLogger.scala). This is
    * deliberate and load-bearing: a test wrapping the whole call in
    * `Console.withOut(new ByteArrayOutputStream){ schemeEntrypointToStdout(...) }` observes, in that
    * single captured buffer, any logger lines AND the scheme text. That makes the "no log line
    * precedes the scheme on stdout" purity assertion falsifiable — reverting this seam to the
    * Compiler axis (live BLoggerImpl) would leak the 'Inputs:' line into the same buffer and the
    * assertion would fail. A private scheme-only PrintStream/sink here would NOT capture the logger
    * and would make that assertion vacuously true, so it is intentionally avoided.
    *
    * Single-thread contract: the `Either` runner is synchronous on the caller thread, so
    * `Console.withOut`'s thread-local `DynamicVariable` intercepts the logger's `Console.println`.
    * It would NOT intercept output emitted from a forked thread.
    */
  private[baboon] def schemeEntrypointToStdout(
    directoryInputs: Set[FSPath],
    individualInputs: Set[FSPath],
    pkg: Pkg,
    version: Version,
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
      emitOnly                 = None,
    )
    val m = new BaboonModuleJvm[EitherF](options, parallelAccumulatingOps2)
    import PathTools.*

    runner.run {
      Injector
        .NoCycles[EitherF[Throwable, _]]()
        .produceRun(m, Activation(BaboonModeAxis.Explorer)) {
          (loader: BaboonLoader[EitherF], logger: BLogger, renderer: BaboonSchemeRenderer) =>
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

              result <- F.fromEither(renderer.render(loadedModels, pkg, version).left.map(e => new RuntimeException(e)))

              _ <- F.maybeSuspend {
                Console.println(result)
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
      emitOnly                 = None,
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
        .produceRun(
          new ModuleDef {
            include(m)
            include(lspModule)
          },
          Activation(BaboonModeAxis.Lsp),
        ) {
          (launcher: LspLauncher, logger: BLogger) =>
            F.maybeSuspend {
              logger.message(LspLogging.Context, "Starting Baboon LSP server...")
              launcher.launch()
            }
        }
    }
  }

}

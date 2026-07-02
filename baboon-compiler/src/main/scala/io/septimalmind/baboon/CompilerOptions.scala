package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.translator.OutputFile
import io.septimalmind.baboon.typer.model.Pkg

sealed trait CompilerTarget {
  def id: String
  def output: OutputOptions
  def generic: GenericOptions

}

object CompilerTarget {
  case class CSTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: CSOptions,
  ) extends CompilerTarget

  case class ScTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: ScOptions,
  ) extends CompilerTarget

  case class PyTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: PyOptions,
  ) extends CompilerTarget

  case class RsTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: RsOptions,
  ) extends CompilerTarget

  case class TsTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: TsOptions,
  ) extends CompilerTarget

  case class KtTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: KtOptions,
  ) extends CompilerTarget

  case class JvTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: JvOptions,
  ) extends CompilerTarget

  case class DtTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: DtOptions,
  ) extends CompilerTarget

  case class SwTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: SwOptions,
  ) extends CompilerTarget

  case class GqlTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: GqlOptions,
  ) extends CompilerTarget

  case class OasTarget(
    id: String,
    output: OutputOptions,
    generic: GenericOptions,
    language: OasOptions,
  ) extends CompilerTarget
}

final case class HktConfig(
  name: String,
  signature: String,
)

final case class ServiceResultConfig(
  noErrors: Boolean,
  resultType: Option[String],
  pattern: Option[String],
  hkt: Option[HktConfig],
)

object ServiceResultConfig {
  val scalaDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = false,
    resultType = Some("scala.util.Either"),
    pattern    = Some("[$error, $success]"),
    hkt        = None,
  )
  val rustDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = false,
    resultType = Some("Result"),
    pattern    = Some("<$success, $error>"),
    hkt        = None,
  )
  val csDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
  val pythonDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
  val typescriptDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
  val kotlinDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = false,
    resultType = Some("Either"),
    pattern    = Some("<$error, $success>"),
    hkt        = None,
  )
  val javaDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
  val dartDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
  val swiftDefault: ServiceResultConfig = ServiceResultConfig(
    noErrors   = true,
    resultType = None,
    pattern    = None,
    hkt        = None,
  )
}

final case class ServiceContextConfig(
  mode: String,
  typeName: String,
  parameterName: String,
)

object ServiceContextConfig {
  val default: ServiceContextConfig = ServiceContextConfig(
    mode          = "none",
    typeName      = "Ctx",
    parameterName = "ctx",
  )
}

final case class PyOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  generateMcpServer: Boolean,
)

final case class ScOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  generateMcpServer: Boolean,
)

final case class CSOptions(
  obsoleteErrors: Boolean,
  omitMostRecentVersionSuffixFromPaths: Boolean,
  omitMostRecentVersionSuffixFromNamespaces: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  writeEvolutionDict: Boolean,
  disregardImplicitUsings: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateIndexWriters: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  deduplicate: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  generateMcpServer: Boolean,
)

final case class RsOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  cratePrefix: String,
  reexportMode: String,
  edition: String,
  generateMcpServer: Boolean,
)

final case class TsOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  importSuffix: String,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  // When true, service symbols are emitted bare (Service/Client/invokeJson/invokeUeba/
  // JsonService/UebaService) and cross-file references are aliased to the prefixed form; the
  // per-service directory + barrel namespace disambiguates. When false (default), the legacy
  // service-name-prefixed symbols are emitted.
  bareServiceSymbols: Boolean,
  mapsAsRecords: Boolean,
  timestampsUtcMode: String,
  timestampsOffsetMode: String,
  /** TS-only backward-compat escape hatch.
    *
    * When `true`, TypeScript emits and decodes lowercase enum wire values (the pre-PR-35 form).
    * Default `false`. Not honored by other backends; using this flag with cross-language deployment
    * will break wire-format compatibility.
    *
    * See `docs/drafts/20260428-1700-enum-wire-format-spec.md`.
    */
  enumLowercaseValues: Boolean,
  generateMcpServer: Boolean,
)

final case class KtOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  multiplatform: Boolean,
  generateMcpServer: Boolean,
)

final case class JvOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  generateMcpServer: Boolean,
)

final case class DtOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  generateMcpServer: Boolean,
)

final case class SwOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  generateDomainFacade: Boolean,
  serviceResult: ServiceResultConfig,
  serviceContext: ServiceContextConfig,
  pragmas: Map[String, String],
  asyncServices: Boolean,
  generateMcpServer: Boolean,
)

final case class GqlOptions(
  pragmas: Map[String, String]
)

final case class OasOptions(
  pragmas: Map[String, String]
)

final case class GenericOptions(
  codecTestIterations: Int
)

final case class OutputOptions(
  safeToRemoveExtensions: Set[String],
  runtime: RuntimeGenOpt,
  generateConversions: Boolean,
  output: FSPath,
  fixturesOutput: Option[FSPath],
  testsOutput: Option[FSPath],
) {
  lazy val products: Set[CompilerProduct] = {
    val defaultProducts = runtime match {
      case RuntimeGenOpt.Only    => Set(CompilerProduct.Runtime)
      case RuntimeGenOpt.Without => Set(CompilerProduct.Definition)
      case RuntimeGenOpt.With    => Set(CompilerProduct.Runtime, CompilerProduct.Definition)
    }
    val conversionProducts = if (generateConversions) Set(CompilerProduct.Conversion) else Set.empty
    val fixturesProducts   = fixturesOutput.map(_ => CompilerProduct.Fixture).toSet
    val testProducts       = testsOutput.map(_ => Set(CompilerProduct.Test, CompilerProduct.FixtureRuntime)).getOrElse(Set.empty)

    defaultProducts ++ conversionProducts ++ fixturesProducts ++ testProducts
  }

  def targetPathFor(out: OutputFile): Option[FSPath] = {
    targetPathFor(out.product)
  }

  def targetPathFor(product: CompilerProduct): Option[FSPath] = {
    if (products.contains(product)) {
      product match {
        case CompilerProduct.Definition     => Some(output)
        case CompilerProduct.Runtime        => Some(output)
        case CompilerProduct.Conversion     => Some(output)
        case CompilerProduct.Fixture        => fixturesOutput
        case CompilerProduct.FixtureRuntime => fixturesOutput
        case CompilerProduct.Test           => testsOutput
        case CompilerProduct.CustomMeta     => None
      }
    } else {
      None
    }
  }

  def targetPaths: Map[CompilerProduct, FSPath] = {
    products.flatMap(t => targetPathFor(t).map(p => (t, p))).toMap
  }
}

sealed trait LockfileUpdate
object LockfileUpdate {
  case object CreateOnly extends LockfileUpdate
  case object Force extends LockfileUpdate

  def parse(s: String): Either[String, LockfileUpdate] = s match {
    case "create-only" => Right(CreateOnly)
    case "force"       => Right(Force)
    case other         => Left(s"Unrecognized lockfile-update value: '$other'. Expected one of: create-only, force")
  }
}

sealed trait LockfileEnforcement
object LockfileEnforcement {
  case object None extends LockfileEnforcement
  case object LegacyVersions extends LockfileEnforcement
  case object AllVersions extends LockfileEnforcement

  def parse(s: String): Either[String, LockfileEnforcement] = s match {
    case "none"            => Right(None)
    case "legacy-versions" => Right(LegacyVersions)
    case "all-versions"    => Right(AllVersions)
    case other             => Left(s"Unrecognized lockfile-enforcement value: '$other'. Expected one of: none, legacy-versions, all-versions")
  }
}

final case class CompilerOptions(
  individualInputs: Set[FSPath],
  directoryInputs: Set[FSPath],
  lockfile: Option[FSPath],
  debug: Boolean,
  targets: Seq[CompilerTarget],
  metaWriteEvolutionJsonTo: Option[FSPath],
  emitOnly: Option[Set[Pkg]],
  lockfileUpdate: LockfileUpdate = LockfileUpdate.CreateOnly,
  lockfileEnforcement: LockfileEnforcement = LockfileEnforcement.LegacyVersions,
)

sealed trait CompilerProduct
object CompilerProduct {
  case object Definition extends CompilerProduct
  case object Runtime extends CompilerProduct
  case object FixtureRuntime extends CompilerProduct
  case object Conversion extends CompilerProduct
  case object Fixture extends CompilerProduct
  case object Test extends CompilerProduct
  case object CustomMeta extends CompilerProduct
}

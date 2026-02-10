package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.translator.OutputFile

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
}

final case class PyOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  enableDeprecatedEncoders: Boolean,
  serviceResult: ServiceResultConfig,
  pragmas: Map[String, String],
)

final case class ScOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  serviceResult: ServiceResultConfig,
  pragmas: Map[String, String],
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
  serviceResult: ServiceResultConfig,
  pragmas: Map[String, String],
)

final case class RsOptions(
  writeEvolutionDict: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  serviceResult: ServiceResultConfig,
  pragmas: Map[String, String],
)

final case class TsOptions(
  writeEvolutionDict: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
  serviceResult: ServiceResultConfig,
  pragmas: Map[String, String],
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

final case class CompilerOptions(
  individualInputs: Set[FSPath],
  directoryInputs: Set[FSPath],
  lockFile: Option[FSPath],
  debug: Boolean,
  targets: Seq[CompilerTarget],
  metaWriteEvolutionJsonTo: Option[FSPath],
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

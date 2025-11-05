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
}

final case class PyOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
)

final case class ScOptions(
  writeEvolutionDict: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  enableDeprecatedEncoders: Boolean,
  generateJsonCodecs: Boolean,
  generateUebaCodecs: Boolean,
  generateUebaCodecsByDefault: Boolean,
  generateJsonCodecsByDefault: Boolean,
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

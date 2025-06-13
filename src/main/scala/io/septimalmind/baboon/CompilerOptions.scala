package io.septimalmind.baboon

import io.septimalmind.baboon.translator.OutputFile

import java.nio.file.Path

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
}

final case class ScOptions(
  writeEvolutionDict: Boolean
)

final case class CSOptions(
  obsoleteErrors: Boolean,
  omitMostRecentVersionSuffixFromPaths: Boolean,
  omitMostRecentVersionSuffixFromNamespaces: Boolean,
  useCompactAdtForm: Boolean,
  wrappedAdtBranchCodecs: Boolean,
  writeEvolutionDict: Boolean,
  disregardImplicitUsings: Boolean,
  enableDeprecatedEncoders: Boolean,
)

final case class GenericOptions(
  metaWriteEvolutionJsonTo: Option[Path],
  codecTestIterations: Int,
)

final case class OutputOptions(
  safeToRemoveExtensions: Set[String],
  runtime: RuntimeGenOpt,
  generateConversions: Boolean,
  output: Path,
  fixturesOutput: Option[Path],
  testsOutput: Option[Path],
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

  def targetPathFor(out: OutputFile): Option[Path] = {
    targetPathFor(out.product)
  }

  def targetPathFor(product: CompilerProduct): Option[Path] = {
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

  def targetPaths: Map[CompilerProduct, Path] = {
    products.flatMap(t => targetPathFor(t).map(p => (t, p))).toMap
  }
}

final case class CompilerOptions(
  individualInputs: Set[Path],
  directoryInputs: Set[Path],
  debug: Boolean,
  targets: Seq[CompilerTarget],
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

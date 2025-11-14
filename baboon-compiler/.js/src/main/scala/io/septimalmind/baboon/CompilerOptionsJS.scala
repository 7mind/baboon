package io.septimalmind.baboon

sealed trait CompilerTargetJS {
  def id: String
  def output: OutputOptionsJS
  def generic: GenericOptions
}

object CompilerTargetJS {
  case class CSTarget(
    id: String,
    output: OutputOptionsJS,
    generic: GenericOptions,
    language: CSOptions,
  ) extends CompilerTargetJS

  case class ScTarget(
    id: String,
    output: OutputOptionsJS,
    generic: GenericOptions,
    language: ScOptions,
  ) extends CompilerTargetJS
}

final case class OutputOptionsJS(
  safeToRemoveExtensions: Set[String],
  runtime: RuntimeGenOpt,
  generateConversions: Boolean,
  generateTests: Boolean,
  generateFixtures: Boolean,
) {
  lazy val products: Set[CompilerProduct] = {
    val defaultProducts = runtime match {
      case RuntimeGenOpt.Only    => Set(CompilerProduct.Runtime)
      case RuntimeGenOpt.Without => Set(CompilerProduct.Definition)
      case RuntimeGenOpt.With    => Set(CompilerProduct.Runtime, CompilerProduct.Definition)
    }
    val conversionProducts = if (generateConversions) Set(CompilerProduct.Conversion) else Set.empty
    val fixturesProducts   = if (generateFixtures) Set(CompilerProduct.Fixture, CompilerProduct.FixtureRuntime) else Set.empty
    val testProducts       = if (generateTests) Set(CompilerProduct.Test) else Set.empty

    defaultProducts ++ conversionProducts ++ fixturesProducts ++ testProducts
  }
}

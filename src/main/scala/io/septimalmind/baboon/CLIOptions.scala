package io.septimalmind.baboon

import caseapp.*

sealed trait RuntimeGenOpt

object RuntimeGenOpt {
  case object Only extends RuntimeGenOpt

  case object With extends RuntimeGenOpt

  case object Without extends RuntimeGenOpt
}

case class GenericTranspilerCLIOptions(
  @HelpMessage("Path to directory where generated code should be placed")
  output: String,
  @HelpMessage("Path to directory where generated fixtures should be placed")
  fixtureOutput: Option[String],
  @HelpMessage(
    "Path to directory where generated tests should be placed " +
    "(requires generated fixtures, if no '--fixtures-output' is specified, fixtures would be generated automatically into tests directory)"
  )
  testOutput: Option[String],
  @HelpMessage("Generate shared runtime classes and evolution registrations, default is `with`")
  @ValueDescription("with|only|without")
  runtime: Option[String],
  @HelpMessage("Do not generate conversions (default is `false`)")
  disableConversions: Option[Boolean],
  @HelpMessage("Most recent versions will not have version segment in its path")
  omitMostRecentVersionSuffixFromPaths: Option[Boolean],
  @HelpMessage("Most recent versions will not have version segment in its namespace")
  omitMostRecentVersionSuffixFromNamespaces: Option[Boolean],
  @HelpMessage("How many iterations the generated codec tests should perform")
  codecTestIterations: Option[Int],
)

trait SharedCLIOptions {
  def generic: GenericTranspilerCLIOptions
  def extAllowCleanup: List[String]
}

case class CsCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Generate obsolete errors instead of deprecations (default is `false`)")
  csObsoleteErrors: Option[Boolean],
  @HelpMessage("Do not generate usings for System, System.Collections.Generic and System.Linq (see ImplicitUsings)")
  csExcludeGlobalUsings: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  csWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Adds evolution metadata as a C# dictionary")
  csWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Do not generate encoders for deprecated versions")
  enableDeprecatedEncoders: Option[Boolean],
  @HelpMessage("Generate UEBA index writers")
  generateIndexWriters: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Apply code deduplication")
  deduplicate: Option[Boolean],
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: cs,json,meta")
  extAllowCleanup: List[String],
) extends SharedCLIOptions

case class ScCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: cs,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Scala dictionary")
  scWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  scWrappedAdtBranchCodecs: Option[Boolean],
) extends SharedCLIOptions

case class CLIOptions(
  @HelpMessage("A list of *.baboon files to process (can be combined with --model-dir)")
  model: List[String],
  @HelpMessage("A file used to track model signatures")
  lockFile: Option[String],
  @HelpMessage("A directory to recursively read all the *.baboon files from")
  modelDir: List[String],
  @HelpMessage("Produces additional debug messages. Do not use.")
  debug: Option[Boolean],
  @HelpMessage("Path to a file to which the compiler will write evolution metadata as a JSON")
  metaWriteEvolutionJson: Option[String],
)

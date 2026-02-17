package io.septimalmind.baboon

import caseapp.*

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
  def serviceResultNoErrors: Option[Boolean]
  def serviceResultType: Option[String]
  def serviceResultPattern: Option[String]
  def serviceContextMode: Option[String]
  def serviceContextType: Option[String]
  def serviceContextParameterName: Option[String]
  def pragma: List[String]
}

trait ScalaHktCLIOptions {
  def serviceResultHkt: Option[Boolean]
  def serviceResultHktName: Option[String]
  def serviceResultHktSignature: Option[String]
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
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for C#)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results (e.g. 'Either')")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type (e.g. '<$error, $success>')")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
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
  @HelpMessage("Do not generate encoders for deprecated versions")
  enableDeprecatedEncoders: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: false for Scala)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results (e.g. 'scala.util.Either')")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type (e.g. '[$error, $success]')")
  serviceResultPattern: Option[String],
  @HelpMessage("Use HKT type parameter for service result")
  serviceResultHkt: Option[Boolean],
  @HelpMessage("HKT type parameter name (e.g. 'F')")
  serviceResultHktName: Option[String],
  @HelpMessage("HKT type parameter signature (e.g. '[+_, +_]')")
  serviceResultHktSignature: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions
  with ScalaHktCLIOptions

case class PyCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: cs,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Scala dictionary")
  pyWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  pyWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Do not generate encoders for deprecated versions")
  enableDeprecatedEncoders: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for Python)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions

case class RsCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: rs,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Rust dictionary")
  rsWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  rsWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: false for Rust)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results (e.g. 'Result')")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type (e.g. '<$success, $error>')")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions

case class TsCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: ts,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a TypeScript dictionary")
  tsWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  tsWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for TypeScript)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions

case class KtCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: kt,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Kotlin dictionary")
  ktWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  ktWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Do not generate encoders for deprecated versions")
  enableDeprecatedEncoders: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: false for Kotlin)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results (e.g. 'Result')")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type (e.g. '<$success, $error>')")
  serviceResultPattern: Option[String],
  @HelpMessage("Use HKT type parameter for service result")
  serviceResultHkt: Option[Boolean],
  @HelpMessage("HKT type parameter name (e.g. 'F')")
  serviceResultHktName: Option[String],
  @HelpMessage("HKT type parameter signature (e.g. '<*, *>')")
  serviceResultHktSignature: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions
  with ScalaHktCLIOptions

case class JvCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: java,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Java dictionary")
  jvWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  jvWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Do not generate encoders for deprecated versions")
  enableDeprecatedEncoders: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for Java)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions

case class DtCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: dart,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Dart dictionary")
  dtWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  dtWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for Dart)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
) extends SharedCLIOptions

case class SchemeCLIOptions(
  @HelpMessage("Domain name (e.g., 'my.domain.name')")
  domain: String,
  @HelpMessage("Version (e.g., '1.0.0')")
  version: String,
  @HelpMessage("Target output file path")
  target: String,
)

case class SwCLIOptions(
  @Recurse
  generic: GenericTranspilerCLIOptions,
  @HelpMessage("Allow to erase target directory even if files with these extensions exist there. Default: swift,json,meta")
  extAllowCleanup: List[String],
  @HelpMessage("Adds evolution metadata as a Swift dictionary")
  swWriteEvolutionDict: Option[Boolean],
  @HelpMessage("Every ADT branch will encode ADT metadata and expect it in the decoder")
  swWrappedAdtBranchCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs")
  generateJsonCodecs: Option[Boolean],
  @HelpMessage("Generate UEBA codecs")
  generateUebaCodecs: Option[Boolean],
  @HelpMessage("Generate JSON codecs even for types without derived[json]")
  generateJsonCodecsByDefault: Option[Boolean],
  @HelpMessage("Generate UEBA codecs even for types without derived[ueba]")
  generateUebaCodecsByDefault: Option[Boolean],
  @HelpMessage("Service methods return only success type, no error wrapping (default: true for Swift)")
  serviceResultNoErrors: Option[Boolean],
  @HelpMessage("Wrapper type for service results")
  serviceResultType: Option[String],
  @HelpMessage("Pattern for service result type")
  serviceResultPattern: Option[String],
  @HelpMessage("Service method context parameter mode: none, abstract, type (default: none)")
  serviceContextMode: Option[String],
  @HelpMessage("Context type name (default: Ctx)")
  serviceContextType: Option[String],
  @HelpMessage("Context parameter name (default: ctx)")
  serviceContextParameterName: Option[String],
  @HelpMessage("Set a pragma value (key=value, repeatable)")
  pragma: List[String],
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

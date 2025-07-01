package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.Quote

object CSTypes {
  // Baboon packages
  val baboonRuntimePkg: CSPackageId = parseCsPkg("Baboon.Runtime.Shared")
  val baboonFixturePkg: CSPackageId = parseCsPkg("Baboon.Fixture")
  val baboonTimePkg: CSPackageId    = parseCsPkg("Baboon.Time")

  // System packages
  val csSystemPkg: CSPackageId               = parseCsPkg("System")
  val csGlobalizationPkg: CSPackageId        = parseCsPkg("System.Globalization")
  val csCollectionsGenericPkg: CSPackageId   = parseCsPkg("System.Collections.Generic")
  val csCollectionsImmutablePkg: CSPackageId = parseCsPkg("System.Collections.Immutable")
  val csLinqPkg: CSPackageId                 = parseCsPkg("System.Linq")
  val csIoPkg: CSPackageId                   = parseCsPkg("System.IO")
  val csTextPkg: CSPackageId                 = parseCsPkg("System.Text")
  val csDiagnosticsPkg: CSPackageId          = parseCsPkg("System.Diagnostics")

  // Newtonsoft packages
  val nsPkg: CSPackageId     = parseCsPkg("Newtonsoft.Json")
  val nsLinqPkg: CSPackageId = parseCsPkg("Newtonsoft.Json.Linq")
  val nunitPkg: CSPackageId  = parseCsPkg("NUnit.Framework")

  // Nunit types
  val nunitTestFixture: CSType  = CSType(nunitPkg, "TestFixture", fq = false, None)
  val nunitOneTimeSetUp: CSType = CSType(nunitPkg, "OneTimeSetUp", fq = false, None)

  // Baboon conversions' types
  val abstractConversion: CSType        = CSType(baboonRuntimePkg, "AbstractConversion", fq = false, None)
  val abstractBaboonConversions: CSType = CSType(baboonRuntimePkg, "AbstractBaboonConversions", fq = false, None)
  val iBaboonGenerated: CSType          = CSType(baboonRuntimePkg, "IBaboonGenerated", fq = false, None)
  val iBaboonAdtMemberMeta: CSType      = CSType(baboonRuntimePkg, "IBaboonAdtMemberMeta", fq = false, None)
  val iBaboonGeneratedLatest: CSType    = CSType(baboonRuntimePkg, "IBaboonGeneratedLatest", fq = false, None)
  val BaboonTools: CSType               = CSType(baboonRuntimePkg, "BaboonTools", fq = false, None)
  val BaboonTestTools: CSType           = CSType(baboonRuntimePkg, "BaboonTestTools", fq = false, None)

  // Baboon codec types
  val iBaboonCodecData: CSType       = CSType(baboonRuntimePkg, "IBaboonCodecData", fq = false, None)
  val iBaboonBinCodecIndexed: CSType = CSType(baboonRuntimePkg, "IBaboonBinCodecIndexed", fq = false, None)
  val baboonCodecContext: CSType     = CSType(baboonRuntimePkg, "BaboonCodecContext", fq = false, None)

  val iBaboonJsonCodec: CSType                 = CSType(baboonRuntimePkg, "IBaboonJsonCodec", fq = false, None)
  val iBaboonBinCodec: CSType                  = CSType(baboonRuntimePkg, "IBaboonBinCodec", fq = false, None)
  def abstractBaboonCodecs(id: String): CSType = CSType(baboonRuntimePkg, s"AbstractBaboon${id.capitalize}Codecs", fq = false, None)

  val baboonJsonCodecBase: CSType                  = CSType(baboonRuntimePkg, "IBaboonJsonCodec.Base", fq = false, None)
  val baboonJsonCodecBaseGenerated: CSType         = CSType(baboonRuntimePkg, "IBaboonJsonCodec.BaseGenerated", fq = false, None)
  val baboonJsonCodecBaseGeneratedAdt: CSType      = CSType(baboonRuntimePkg, "IBaboonJsonCodec.BaseGeneratedAdt", fq = false, None)
  val baboonJsonCodecNoEncoder: CSType             = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoder", fq = false, None)
  val baboonJsonCodecNoEncoderGenerated: CSType    = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoderGenerated", fq = false, None)
  val baboonJsonCodecNoEncoderGeneratedAdt: CSType = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoderGeneratedAdt", fq = false, None)

  val baboonBinCodecBase: CSType                  = CSType(baboonRuntimePkg, "IBaboonBinCodec.Base", fq = false, None)
  val baboonBinCodecBaseGenerated: CSType         = CSType(baboonRuntimePkg, "IBaboonBinCodec.BaseGenerated", fq = false, None)
  val baboonBinCodecBaseGeneratedAdt: CSType      = CSType(baboonRuntimePkg, "IBaboonBinCodec.BaseGeneratedAdt", fq = false, None)
  val baboonBinCodecNoEncoder: CSType             = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoder", fq = false, None)
  val baboonBinCodecNoEncoderGenerated: CSType    = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoderGenerated", fq = false, None)
  val baboonBinCodecNoEncoderGeneratedAdt: CSType = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoderGeneratedAdt", fq = false, None)

  val either: CSType = CSType(baboonRuntimePkg, "Either", fq = false, None)
  val unit: CSType   = CSType(baboonRuntimePkg, "Unit", fq = false, None)

  val baboonTimeFormats: CSType = CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false, None)

  val iBaboonMeta: CSType = CSType(baboonRuntimePkg, "IBaboonMeta", fq = false, None)

  // Baboon type
  val rpDateTime: CSType = CSType(baboonTimePkg, "RpDateTime", fq = false, None)

  // Baboon fixture
  val baboonFixture: CSType = CSType(baboonFixturePkg, "BaboonFixture", fq = false, None)

  // Newtonsoft types
  val nsJsonWriter: CSType     = CSType(nsPkg, "JsonWriter", fq = false, None)
  val nsJsonReader: CSType     = CSType(nsPkg, "JsonReader", fq = false, None)
  val nsJsonSerializer: CSType = CSType(nsPkg, "JsonSerializer", fq = false, None)
  val nsJsonConverter: CSType  = CSType(nsPkg, "JsonConverter", fq = false, None)
  val nsJsonConvert: CSType    = CSType(nsPkg, "JsonConvert", fq = false, None)
  val nsFormatting: CSType     = CSType(nsPkg, "Formatting", fq = false, None)
  val nsJToken: CSType         = CSType(nsLinqPkg, "JToken", fq = false, None)
  val nsJValue: CSType         = CSType(nsLinqPkg, "JValue", fq = false, None)
  val nsJArray: CSType         = CSType(nsLinqPkg, "JArray", fq = false, None)
  val nsJObject: CSType        = CSType(nsLinqPkg, "JObject", fq = false, None)
  val nsJProperty: CSType      = CSType(nsLinqPkg, "JProperty", fq = false, None)
  val nsJTokenType: CSType     = CSType(nsLinqPkg, "JTokenType", fq = false, None)

  val binaryReader: CSType = CSType(csIoPkg, "BinaryReader", fq = false, None)
  val binaryWriter: CSType = CSType(csIoPkg, "BinaryWriter", fq = false, None)
  val memoryStream: CSType = CSType(csIoPkg, "MemoryStream", fq = false, None)

  // C# types
  val csString: CSType        = CSType(csSystemPkg, "String", fq = false, None)
  val csGuid: CSType          = CSType(csSystemPkg, "Guid", fq = false, None)
  val csBoolean: CSType       = CSType(csSystemPkg, "Boolean", fq = false, None)
  val csStringBuilder: CSType = CSType(csTextPkg, "StringBuilder", fq = false, None)
  val csEncoding: CSType      = CSType(csTextPkg, "Encoding", fq = false, None)

  val csSByte: CSType = CSType(csSystemPkg, "sbyte", fq = false, None)
  val csInt16: CSType = CSType(csSystemPkg, "Int16", fq = false, None)
  val csInt32: CSType = CSType(csSystemPkg, "Int32", fq = false, None)
  val csInt64: CSType = CSType(csSystemPkg, "Int64", fq = false, None)

  val csByte: CSType   = CSType(csSystemPkg, "byte", fq = false, None)
  val csUInt16: CSType = CSType(csSystemPkg, "UInt16", fq = false, None)
  val csUInt32: CSType = CSType(csSystemPkg, "UInt32", fq = false, None)
  val csUInt64: CSType = CSType(csSystemPkg, "UInt64", fq = false, None)

  val csSingle: CSType  = CSType(csSystemPkg, "Single", fq = false, None)
  val csDouble: CSType  = CSType(csSystemPkg, "Double", fq = false, None)
  val csDecimal: CSType = CSType(csSystemPkg, "Decimal", fq = false, None)

  val csTpe: CSType  = CSType(csSystemPkg, "Type", fq = false, None)
  val csLazy: CSType = CSType(csSystemPkg, "Lazy", fq = false, None)

  val csEnum: CSType              = CSType(csSystemPkg, "Enum", fq = false, None)
  val csDateTime: CSType          = CSType(csSystemPkg, "DateTime", fq = false, None)
  val csTimeSpan: CSType          = CSType(csSystemPkg, "TimeSpan", fq = false, None)
  val csDayOfWeek: CSType         = CSType(csSystemPkg, "DayOfWeek", fq = false, None)
  val csArgumentException: CSType = CSType(csSystemPkg, "ArgumentException", fq = false, None)
  val csEnumerable: CSType        = CSType(csLinqPkg, "Enumerable", fq = false, None)
  val csRandom: CSType            = CSType(csSystemPkg, "Random", fq = false, None)
  val csIComparable: CSType       = CSType(csSystemPkg, "IComparable", fq = false, None)
  val csIEquatable: CSType        = CSType(csSystemPkg, "IEquatable", fq = false, None)

  val csIReadOnlyDictionary: CSType = CSType(csCollectionsGenericPkg, "IReadOnlyDictionary", fq = false, None)
  val csIReadOnlyList: CSType       = CSType(csCollectionsGenericPkg, "IReadOnlyList", fq = false, None)
  val csIReadOnlySet: CSType        = CSType(csCollectionsGenericPkg, "IReadOnlySet", fq = false, None)

  val csIDictionary: CSType = CSType(csCollectionsGenericPkg, "IDictionary", fq = false, None)
  val csIList: CSType       = CSType(csCollectionsGenericPkg, "IList", fq = false, None)
  val csISet: CSType        = CSType(csCollectionsGenericPkg, "ISet", fq = false, None)

  val csList: CSType       = CSType(csCollectionsGenericPkg, "List", fq = false, None)
  val csSet: CSType        = CSType(csCollectionsGenericPkg, "HashSet", fq = false, None)
  val csDictionary: CSType = CSType(csCollectionsGenericPkg, "Dictionary", fq = false, None)

  val csImmutableDictionary: CSType = CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false, None)
  val csImmutableList: CSType       = CSType(csCollectionsImmutablePkg, "ImmutableList", fq = false, None)
  val csImmutableHashSet: CSType    = CSType(csCollectionsImmutablePkg, "ImmutableHashSet", fq = false, None)

  val csKeyValuePair: CSType = CSType(csCollectionsGenericPkg, "KeyValuePair", fq = false, None)

  val csInvariantCulture: CSType = CSType(csGlobalizationPkg, "CultureInfo", fq = false, None)
  val csDateTimeStyles: CSType   = CSType(csGlobalizationPkg, "DateTimeStyles", fq = false, None)
  val csDateTimeKind: CSType     = CSType(csSystemPkg, "DateTimeKind", fq = false, None)
  val csTimeZoneInfo: CSType     = CSType(csSystemPkg, "TimeZoneInfo", fq = false, None)

  val debug: CSType = CSType(csDiagnosticsPkg, "Debug", fq = false, None)

  val mkDict = q"BbnToDictionary()"
  val mkList = q"BbnToList()"
  val mkSet  = q"ToImmutableHashSet()"

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseCsPkg(pkg: String): CSPackageId  = CSPackageId(parsePkg(pkg))
}

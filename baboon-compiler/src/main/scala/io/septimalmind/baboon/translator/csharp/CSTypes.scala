package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType, CSTypeOrigin}
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
  val nunitTestFixture: CSType  = CSType(nunitPkg, "TestFixture", fq = false, CSTypeOrigin.Other)
  val nunitOneTimeSetUp: CSType = CSType(nunitPkg, "OneTimeSetUp", fq = false, CSTypeOrigin.Other)

  // Baboon conversions' types
  val abstractConversion: CSType        = CSType(baboonRuntimePkg, "AbstractConversion", fq = false, CSTypeOrigin.Other)
  val abstractBaboonConversions: CSType = CSType(baboonRuntimePkg, "AbstractBaboonConversions", fq = false, CSTypeOrigin.Other)
  val iBaboonGenerated: CSType          = CSType(baboonRuntimePkg, "IBaboonGenerated", fq = false, CSTypeOrigin.Other)
  val iBaboonAdtMemberMeta: CSType      = CSType(baboonRuntimePkg, "IBaboonAdtMemberMeta", fq = false, CSTypeOrigin.Other)
  val iBaboonGeneratedLatest: CSType    = CSType(baboonRuntimePkg, "IBaboonGeneratedLatest", fq = false, CSTypeOrigin.Other)
  val BaboonTools: CSType               = CSType(baboonRuntimePkg, "BaboonTools", fq = false, CSTypeOrigin.Other)
  val BaboonTestTools: CSType           = CSType(baboonRuntimePkg, "BaboonTestTools", fq = false, CSTypeOrigin.Other)

  // Baboon codec types
  val iBaboonCodecData: CSType       = CSType(baboonRuntimePkg, "IBaboonCodecData", fq = false, CSTypeOrigin.Other)
  val iBaboonBinCodecIndexed: CSType = CSType(baboonRuntimePkg, "IBaboonBinCodecIndexed", fq = false, CSTypeOrigin.Other)
  val baboonCodecContext: CSType     = CSType(baboonRuntimePkg, "BaboonCodecContext", fq = false, CSTypeOrigin.Other)

  def abstractBaboonCodecs(id: String): CSType = CSType(baboonRuntimePkg, s"AbstractBaboon${id.capitalize}Codecs", fq = false, CSTypeOrigin.Other)

  val iBaboonJsonCodec: CSType = CSType(baboonRuntimePkg, "IBaboonJsonCodec", fq = false, CSTypeOrigin.Other)
  val iBaboonBinCodec: CSType  = CSType(baboonRuntimePkg, "IBaboonBinCodec", fq = false, CSTypeOrigin.Other)

  val baboonJsonCodecBase: CSType                  = CSType(baboonRuntimePkg, "IBaboonJsonCodec.Base", fq = false, CSTypeOrigin.Other)
  val baboonJsonCodecBaseGenerated: CSType         = CSType(baboonRuntimePkg, "IBaboonJsonCodec.BaseGenerated", fq = false, CSTypeOrigin.Other)
  val baboonJsonCodecBaseGeneratedAdt: CSType      = CSType(baboonRuntimePkg, "IBaboonJsonCodec.BaseGeneratedAdt", fq = false, CSTypeOrigin.Other)
  val baboonJsonCodecNoEncoder: CSType             = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoder", fq = false, CSTypeOrigin.Other)
  val baboonJsonCodecNoEncoderGenerated: CSType    = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoderGenerated", fq = false, CSTypeOrigin.Other)
  val baboonJsonCodecNoEncoderGeneratedAdt: CSType = CSType(baboonRuntimePkg, "IBaboonJsonCodec.NoEncoderGeneratedAdt", fq = false, CSTypeOrigin.Other)

  val baboonBinCodecBase: CSType                  = CSType(baboonRuntimePkg, "IBaboonBinCodec.Base", fq = false, CSTypeOrigin.Other)
  val baboonBinCodecBaseGenerated: CSType         = CSType(baboonRuntimePkg, "IBaboonBinCodec.BaseGenerated", fq = false, CSTypeOrigin.Other)
  val baboonBinCodecBaseGeneratedAdt: CSType      = CSType(baboonRuntimePkg, "IBaboonBinCodec.BaseGeneratedAdt", fq = false, CSTypeOrigin.Other)
  val baboonBinCodecNoEncoder: CSType             = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoder", fq = false, CSTypeOrigin.Other)
  val baboonBinCodecNoEncoderGenerated: CSType    = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoderGenerated", fq = false, CSTypeOrigin.Other)
  val baboonBinCodecNoEncoderGeneratedAdt: CSType = CSType(baboonRuntimePkg, "IBaboonBinCodec.NoEncoderGeneratedAdt", fq = false, CSTypeOrigin.Other)
  val csByteString: CSType                        = CSType(baboonRuntimePkg, "ByteString", fq = false, CSTypeOrigin.Other)

  val either: CSType = CSType(baboonRuntimePkg, "Either", fq = false, CSTypeOrigin.Other)
  val unit: CSType   = CSType(baboonRuntimePkg, "Unit", fq = false, CSTypeOrigin.Other)

  // Service wiring types
  val baboonMethodId: CSType        = CSType(baboonRuntimePkg, "BaboonMethodId", fq = false, CSTypeOrigin.Other)
  val baboonWiringError: CSType     = CSType(baboonRuntimePkg, "BaboonWiringError", fq = false, CSTypeOrigin.Other)
  val baboonWiringException: CSType = CSType(baboonRuntimePkg, "BaboonWiringException", fq = false, CSTypeOrigin.Other)

  val csException: CSType = CSType(csSystemPkg, "Exception", fq = false, CSTypeOrigin.Other)

  val baboonTimeFormats: CSType = CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false, CSTypeOrigin.Other)

  val iBaboonMeta: CSType = CSType(baboonRuntimePkg, "IBaboonMeta", fq = false, CSTypeOrigin.Other)

  // Baboon type
  val rpDateTime: CSType = CSType(baboonTimePkg, "RpDateTime", fq = false, CSTypeOrigin.Other)

  // Baboon fixture
  val baboonFixture: CSType = CSType(baboonFixturePkg, "BaboonFixture", fq = false, CSTypeOrigin.Other)

  // Newtonsoft types
  val nsJsonWriter: CSType     = CSType(nsPkg, "JsonWriter", fq = false, CSTypeOrigin.Other)
  val nsJsonReader: CSType     = CSType(nsPkg, "JsonReader", fq = false, CSTypeOrigin.Other)
  val nsJsonSerializer: CSType = CSType(nsPkg, "JsonSerializer", fq = false, CSTypeOrigin.Other)
  val nsJsonConverter: CSType  = CSType(nsPkg, "JsonConverter", fq = false, CSTypeOrigin.Other)
  val nsJsonConvert: CSType    = CSType(nsPkg, "JsonConvert", fq = false, CSTypeOrigin.Other)
  val nsFormatting: CSType     = CSType(nsPkg, "Formatting", fq = false, CSTypeOrigin.Other)
  val nsJToken: CSType         = CSType(nsLinqPkg, "JToken", fq = false, CSTypeOrigin.Other)
  val nsJValue: CSType         = CSType(nsLinqPkg, "JValue", fq = false, CSTypeOrigin.Other)
  val nsJArray: CSType         = CSType(nsLinqPkg, "JArray", fq = false, CSTypeOrigin.Other)
  val nsJObject: CSType        = CSType(nsLinqPkg, "JObject", fq = false, CSTypeOrigin.Other)
  val nsJProperty: CSType      = CSType(nsLinqPkg, "JProperty", fq = false, CSTypeOrigin.Other)
  val nsJTokenType: CSType     = CSType(nsLinqPkg, "JTokenType", fq = false, CSTypeOrigin.Other)

  val binaryReader: CSType = CSType(csIoPkg, "BinaryReader", fq = false, CSTypeOrigin.Other)
  val binaryWriter: CSType = CSType(csIoPkg, "BinaryWriter", fq = false, CSTypeOrigin.Other)
  val memoryStream: CSType = CSType(csIoPkg, "MemoryStream", fq = false, CSTypeOrigin.Other)

  // C# types
  val csString: CSType        = CSType(csSystemPkg, "String", fq = false, CSTypeOrigin.Other)
  val csGuid: CSType          = CSType(csSystemPkg, "Guid", fq = false, CSTypeOrigin.Other)
  val csBoolean: CSType       = CSType(csSystemPkg, "Boolean", fq = false, CSTypeOrigin.Other)
  val csStringBuilder: CSType = CSType(csTextPkg, "StringBuilder", fq = false, CSTypeOrigin.Other)
  val csEncoding: CSType      = CSType(csTextPkg, "Encoding", fq = false, CSTypeOrigin.Other)

  val csSByte: CSType = CSType(csSystemPkg, "sbyte", fq = false, CSTypeOrigin.Other)
  val csInt16: CSType = CSType(csSystemPkg, "Int16", fq = false, CSTypeOrigin.Other)
  val csInt32: CSType = CSType(csSystemPkg, "Int32", fq = false, CSTypeOrigin.Other)
  val csInt64: CSType = CSType(csSystemPkg, "Int64", fq = false, CSTypeOrigin.Other)

  val csByte: CSType   = CSType(csSystemPkg, "byte", fq = false, CSTypeOrigin.Other)
  val csUInt16: CSType = CSType(csSystemPkg, "UInt16", fq = false, CSTypeOrigin.Other)
  val csUInt32: CSType = CSType(csSystemPkg, "UInt32", fq = false, CSTypeOrigin.Other)
  val csUInt64: CSType = CSType(csSystemPkg, "UInt64", fq = false, CSTypeOrigin.Other)

  val csSingle: CSType  = CSType(csSystemPkg, "Single", fq = false, CSTypeOrigin.Other)
  val csDouble: CSType  = CSType(csSystemPkg, "Double", fq = false, CSTypeOrigin.Other)
  val csDecimal: CSType = CSType(csSystemPkg, "Decimal", fq = false, CSTypeOrigin.Other)

  val csTpe: CSType  = CSType(csSystemPkg, "Type", fq = false, CSTypeOrigin.Other)
  val csLazy: CSType = CSType(csSystemPkg, "Lazy", fq = false, CSTypeOrigin.Other)

  val csEnum: CSType              = CSType(csSystemPkg, "Enum", fq = false, CSTypeOrigin.Other)
  val csDateTime: CSType          = CSType(csSystemPkg, "DateTime", fq = false, CSTypeOrigin.Other)
  val csTimeSpan: CSType          = CSType(csSystemPkg, "TimeSpan", fq = false, CSTypeOrigin.Other)
  val csDayOfWeek: CSType         = CSType(csSystemPkg, "DayOfWeek", fq = false, CSTypeOrigin.Other)
  val csArgumentException: CSType = CSType(csSystemPkg, "ArgumentException", fq = false, CSTypeOrigin.Other)
  val csEnumerable: CSType        = CSType(csLinqPkg, "Enumerable", fq = false, CSTypeOrigin.Other)
  val csRandom: CSType            = CSType(csSystemPkg, "Random", fq = false, CSTypeOrigin.Other)
  val csIComparable: CSType       = CSType(csSystemPkg, "IComparable", fq = false, CSTypeOrigin.Other)
  val csIEquatable: CSType        = CSType(csSystemPkg, "IEquatable", fq = false, CSTypeOrigin.Other)

  val csIReadOnlyDictionary: CSType = CSType(csCollectionsGenericPkg, "IReadOnlyDictionary", fq = false, CSTypeOrigin.Other)
  val csIReadOnlyList: CSType       = CSType(csCollectionsGenericPkg, "IReadOnlyList", fq = false, CSTypeOrigin.Other)
  val csIReadOnlySet: CSType        = CSType(csCollectionsGenericPkg, "IReadOnlySet", fq = false, CSTypeOrigin.Other)

  val csIDictionary: CSType = CSType(csCollectionsGenericPkg, "IDictionary", fq = false, CSTypeOrigin.Other)
  val csIList: CSType       = CSType(csCollectionsGenericPkg, "IList", fq = false, CSTypeOrigin.Other)
  val csISet: CSType        = CSType(csCollectionsGenericPkg, "ISet", fq = false, CSTypeOrigin.Other)

  val csList: CSType       = CSType(csCollectionsGenericPkg, "List", fq = false, CSTypeOrigin.Other)
  val csSet: CSType        = CSType(csCollectionsGenericPkg, "HashSet", fq = false, CSTypeOrigin.Other)
  val csDictionary: CSType = CSType(csCollectionsGenericPkg, "Dictionary", fq = false, CSTypeOrigin.Other)

  val csImmutableDictionary: CSType = CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false, CSTypeOrigin.Other)
  val csImmutableList: CSType       = CSType(csCollectionsImmutablePkg, "ImmutableList", fq = false, CSTypeOrigin.Other)
  val csImmutableHashSet: CSType    = CSType(csCollectionsImmutablePkg, "ImmutableHashSet", fq = false, CSTypeOrigin.Other)

  val csKeyValuePair: CSType = CSType(csCollectionsGenericPkg, "KeyValuePair", fq = false, CSTypeOrigin.Other)

  val csInvariantCulture: CSType = CSType(csGlobalizationPkg, "CultureInfo", fq = false, CSTypeOrigin.Other)
  val csDateTimeStyles: CSType   = CSType(csGlobalizationPkg, "DateTimeStyles", fq = false, CSTypeOrigin.Other)
  val csDateTimeKind: CSType     = CSType(csSystemPkg, "DateTimeKind", fq = false, CSTypeOrigin.Other)
  val csTimeZoneInfo: CSType     = CSType(csSystemPkg, "TimeZoneInfo", fq = false, CSTypeOrigin.Other)

  val debug: CSType = CSType(csDiagnosticsPkg, "Debug", fq = false, CSTypeOrigin.Other)

  val mkDict = q"BbnToDictionary()"
  val mkList = q"BbnToList()"
  val mkSet  = q"ToImmutableHashSet()"

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseCsPkg(pkg: String): CSPackageId  = CSPackageId(parsePkg(pkg))
}

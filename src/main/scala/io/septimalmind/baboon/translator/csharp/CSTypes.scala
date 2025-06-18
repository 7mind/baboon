package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.Quote

object CSTypes {
  // Baboon packages
  val baboonRuntimePkg: CSPackageId = CSPackageId(NEList("Baboon", "Runtime", "Shared"))
  val baboonFixturePkg: CSPackageId = CSPackageId(NEList("Baboon", "Fixture"))
  val baboonTimePkg: CSPackageId    = CSPackageId(NEList("Baboon", "Time"))

  // System packages
  val csSystemPkg: CSPackageId               = CSPackageId(NEList("System"))
  val csGlobalizationPkg: CSPackageId        = CSPackageId(NEList("System", "Globalization"))
  val csCollectionsGenericPkg: CSPackageId   = CSPackageId(NEList("System", "Collections", "Generic"))
  val csCollectionsImmutablePkg: CSPackageId = CSPackageId(NEList("System", "Collections", "Immutable"))
  val csLinqPkg: CSPackageId                 = CSPackageId(NEList("System", "Linq"))
  val csIoPkg: CSPackageId                   = CSPackageId(NEList("System", "IO"))
  val csTextPkg: CSPackageId                 = CSPackageId(NEList("System.Text"))
  val csDiagnosticsPkg: CSPackageId          = CSPackageId(NEList("System", "Diagnostics"))

  // Newtonsoft packages
  val nsPkg: CSPackageId     = CSPackageId(NEList("Newtonsoft", "Json"))
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))
  val nunitPkg: CSPackageId  = CSPackageId(NEList("NUnit", "Framework"))

  // Nunit types
  val nunitTestFixture: CSType  = CSType(nunitPkg, "TestFixture", fq = false)
  val nunitOneTimeSetUp: CSType = CSType(nunitPkg, "OneTimeSetUp", fq = false)

  // Baboon conversions' types
  val abstractConversion: CSType        = CSType(baboonRuntimePkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType = CSType(baboonRuntimePkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType          = CSType(baboonRuntimePkg, "IBaboonGenerated", fq = false)
  val iBaboonAdtMemberMeta: CSType      = CSType(baboonRuntimePkg, "IBaboonAdtMemberMeta", fq = false)
  val iBaboonGeneratedLatest: CSType    = CSType(baboonRuntimePkg, "IBaboonGeneratedLatest", fq = false)
  val BaboonTools: CSType               = CSType(baboonRuntimePkg, "BaboonTools", fq = false)
  val BaboonTestTools: CSType           = CSType(baboonRuntimePkg, "BaboonTestTools", fq = false)

  // Baboon codec types
  val iBaboonCodecData: CSType       = CSType(baboonRuntimePkg, "IBaboonCodecData", fq = false)
  val iBaboonBinCodecIndexed: CSType = CSType(baboonRuntimePkg, "IBaboonBinCodecIndexed", fq = false)
  val baboonCodecContext: CSType     = CSType(baboonRuntimePkg, "BaboonCodecContext", fq = false)
  val iBaboonCodec: CSType           = CSType(baboonRuntimePkg, "IBaboonCodec", fq = false)
  val iBaboonValueCodec: CSType      = CSType(baboonRuntimePkg, "IBaboonValueCodec", fq = false)
  val iBaboonStreamCodec: CSType     = CSType(baboonRuntimePkg, "IBaboonStreamCodec", fq = false)

  val iBaboonJsonCodec: CSType     = CSType(baboonRuntimePkg, "IBaboonJsonCodec", fq = false)
  val iBaboonBinCodec: CSType      = CSType(baboonRuntimePkg, "IBaboonBinCodec", fq = false)
  val iBaboonTypeCodecs: CSType    = CSType(baboonRuntimePkg, "IBaboonTypeCodecs", fq = false)
  val baboonTypeCodecs: CSType     = CSType(baboonRuntimePkg, "BaboonTypeCodecs", fq = false)
  val abstractBaboonCodecs: CSType = CSType(baboonRuntimePkg, "AbstractBaboonCodecs", fq = false)

  val either: CSType = CSType(baboonRuntimePkg, "Either", fq = false)
  val unit: CSType   = CSType(baboonRuntimePkg, "Unit", fq = false)

  val baboonTimeFormats: CSType = CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false)

  val iBaboonMeta: CSType = CSType(baboonRuntimePkg, "IBaboonMeta", fq = false)

  // Baboon type
  val rpDateTime: CSType = CSType(baboonTimePkg, "RpDateTime", fq = false)

  // Baboon fixture
  val baboonFixture: CSType = CSType(baboonFixturePkg, "BaboonFixture", fq = false)

  // Newtonsoft types
  val nsJsonWriter: CSType     = CSType(nsPkg, "JsonWriter", fq = false)
  val nsJsonReader: CSType     = CSType(nsPkg, "JsonReader", fq = false)
  val nsJsonSerializer: CSType = CSType(nsPkg, "JsonSerializer", fq = false)
  val nsJsonConverter: CSType  = CSType(nsPkg, "JsonConverter", fq = false)
  val nsFormatting: CSType     = CSType(nsPkg, "Formatting", fq = false)
  val nsJToken: CSType         = CSType(nsLinqPkg, "JToken", fq = false)
  val nsJValue: CSType         = CSType(nsLinqPkg, "JValue", fq = false)
  val nsJArray: CSType         = CSType(nsLinqPkg, "JArray", fq = false)
  val nsJObject: CSType        = CSType(nsLinqPkg, "JObject", fq = false)
  val nsJProperty: CSType      = CSType(nsLinqPkg, "JProperty", fq = false)
  val nsJTokenType: CSType     = CSType(nsLinqPkg, "JTokenType", fq = false)

  val binaryReader: CSType = CSType(csIoPkg, "BinaryReader", fq = false)
  val binaryWriter: CSType = CSType(csIoPkg, "BinaryWriter", fq = false)
  val memoryStream: CSType = CSType(csIoPkg, "MemoryStream", fq = false)

  // C# types
  val csString: CSType        = CSType(csSystemPkg, "String", fq = false)
  val csGuid: CSType          = CSType(csSystemPkg, "Guid", fq = false)
  val csBoolean: CSType       = CSType(csSystemPkg, "Boolean", fq = false)
  val csStringBuilder: CSType = CSType(csTextPkg, "StringBuilder", fq = false)

  val csSByte: CSType = CSType(csSystemPkg, "sbyte", fq = false)
  val csInt16: CSType = CSType(csSystemPkg, "Int16", fq = false)
  val csInt32: CSType = CSType(csSystemPkg, "Int32", fq = false)
  val csInt64: CSType = CSType(csSystemPkg, "Int64", fq = false)

  val csByte: CSType   = CSType(csSystemPkg, "byte", fq = false)
  val csUInt16: CSType = CSType(csSystemPkg, "UInt16", fq = false)
  val csUInt32: CSType = CSType(csSystemPkg, "UInt32", fq = false)
  val csUInt64: CSType = CSType(csSystemPkg, "UInt64", fq = false)

  val csSingle: CSType  = CSType(csSystemPkg, "Single", fq = false)
  val csDouble: CSType  = CSType(csSystemPkg, "Double", fq = false)
  val csDecimal: CSType = CSType(csSystemPkg, "Decimal", fq = false)

  val csTpe: CSType  = CSType(csSystemPkg, "Type", fq = false)
  val csLazy: CSType = CSType(csSystemPkg, "Lazy", fq = false)

  val csEnum: CSType              = CSType(csSystemPkg, "Enum", fq = false)
  val csDateTime: CSType          = CSType(csSystemPkg, "DateTime", fq = false)
  val csTimeSpan: CSType          = CSType(csSystemPkg, "TimeSpan", fq = false)
  val csDayOfWeek: CSType         = CSType(csSystemPkg, "DayOfWeek", fq = false)
  val csArgumentException: CSType = CSType(csSystemPkg, "ArgumentException", fq = false)
  val csEnumerable: CSType        = CSType(csLinqPkg, "Enumerable", fq = false)
  val csRandom: CSType            = CSType(csSystemPkg, "Random", fq = false)
  val csIComparable: CSType       = CSType(csSystemPkg, "IComparable", fq = false)
  val csIEquatable: CSType        = CSType(csSystemPkg, "IEquatable", fq = false)

  val csIReadOnlyDictionary: CSType = CSType(csCollectionsGenericPkg, "IReadOnlyDictionary", fq = false)
  val csIReadOnlyList: CSType       = CSType(csCollectionsGenericPkg, "IReadOnlyList", fq = false)
  val csIReadOnlySet: CSType        = CSType(csCollectionsGenericPkg, "IReadOnlySet", fq = false)

  val csIDictionary: CSType = CSType(csCollectionsGenericPkg, "IDictionary", fq = false)
  val csIList: CSType       = CSType(csCollectionsGenericPkg, "IList", fq = false)
  val csISet: CSType        = CSType(csCollectionsGenericPkg, "ISet", fq = false)

  val csList: CSType       = CSType(csCollectionsGenericPkg, "List", fq = false)
  val csSet: CSType        = CSType(csCollectionsGenericPkg, "HashSet", fq = false)
  val csDictionary: CSType = CSType(csCollectionsGenericPkg, "Dictionary", fq = false)

  val csImmutableDictionary: CSType = CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false)
  val csImmutableList: CSType       = CSType(csCollectionsImmutablePkg, "ImmutableList", fq = false)
  val csImmutableHashSet: CSType    = CSType(csCollectionsImmutablePkg, "ImmutableHashSet", fq = false)

  val csKeyValuePair: CSType = CSType(csCollectionsGenericPkg, "KeyValuePair", fq = false)

  val csInvariantCulture: CSType = CSType(csGlobalizationPkg, "CultureInfo", fq = false)
  val csDateTimeStyles: CSType   = CSType(csGlobalizationPkg, "DateTimeStyles", fq = false)
  val csDateTimeKind: CSType     = CSType(csSystemPkg, "DateTimeKind", fq = false)
  val csTimeZoneInfo: CSType     = CSType(csSystemPkg, "TimeZoneInfo", fq = false)

  val debug: CSType = CSType(csDiagnosticsPkg, "Debug", fq = false)

  val mkDict = q"BbnToDictionary()"
  val mkList = q"BbnToList()"
  val mkSet  = q"ToImmutableHashSet()"

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseCsPkg(pkg: String): CSPackageId  = CSPackageId(parsePkg(pkg))
}

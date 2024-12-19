package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.RuntimeGenOpt
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.translator.{
  BaboonAbstractTranslator,
  OutputFile,
  Sources
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(defnTranslator: CSDefnTranslator,
                         trans: CSTypeTranslator,
                         handler: Subcontext[IndividualConversionHandler],
                         options: CompilerOptions,
                         codecs: Set[CSCodecTranslator],
                         tools: CSDefnTools,
) extends BaboonAbstractTranslator {

  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(family)
      rt <- sharedRuntime()
      testRuntime <- sharedTestRuntime
//      meta <- buildMeta(family)
      toRender = options.runtime match {
        case RuntimeGenOpt.Only    => rt
        case RuntimeGenOpt.With    => rt ++ translated ++ testRuntime
        case RuntimeGenOpt.Without => translated
      }
      rendered = toRender.map { o =>
        val content = renderTree(o)
        (o.path, OutputFile(content, o.isTest))
      }
      unique <- (rendered /*++ meta.map {
        case (k, v) => (k, OutputFile(v, isTest = false))
      }*/ ).toUniqueMap(c => NEList(BaboonIssue.NonUniqueOutputFiles(c)))
    } yield {
      Sources(unique)
    }
  }

//  private def buildMeta(family: BaboonFamily): Out[List[(String, String)]] = {
//
//    val data = family.domains.toSeq.flatMap {
//      case (_, lineage) =>
//        lineage.versions.toSeq.map {
//          case (ver, _) =>
//            VersionMeta(lineage.pkg.path.mkString("."), ver.version)
//        }
//
//    }
//    val meta: OutputMeta = OutputMeta(data.toList)
//    val json = meta.asJson.spaces2
//
//    Right(List((s"baboon-meta.json", json)))
//  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set.empty
      } else {
        Set(csSystemPkg, csCollectionsGenericPkg, csLinqPkg)
      }

    val forcedUses: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set(csLinqPkg)
      } else {
        Set.empty
      }

    val usedPackages = o.tree.values
      .collect {
        case t: CSValue.CSType => t
      }
      .flatMap { t =>
        if (!t.fq) {
          Seq(t.pkg)
        } else {
          Seq.empty
        }
      }
      .distinct
      .sortBy(_.parts.mkString("."))

    val available = Set(o.pkg)
    val requiredPackages = Set.empty
    val allPackages =
      (requiredPackages ++ usedPackages ++ forcedUses)
        .diff(available ++ alwaysAvailable)

    val imports = allPackages.toSeq
      .map { p =>
        if (p.isStatic) {
          q"using static ${p.parts.mkString(".")};"
        } else {
          q"using ${p.parts.mkString(".")};"
        }

      }
      .join("\n")

    val full =
      Seq(
        Seq(q"#nullable enable"),
        Seq(q"#pragma warning disable 612,618"), // deprecation warnings
        Seq(imports),
        Seq(o.tree)
      ).flatten
        .join("\n\n")

    full.mapRender {
      case t: CSValue.CSTypeName =>
        t.name
      case t: CSValue.CSType if !t.fq =>
        if (o.pkg == t.pkg || !t.pkg.parts.startsWith(o.pkg.parts)) {
          t.name
        } else {
          (t.pkg.parts :+ t.name).mkString(".")
        }

      case t: CSValue.CSType =>
        (t.pkg.parts :+ t.name).mkString(".")
    }
  }

  private def doTranslate(
    family: BaboonFamily
  ): Out[List[CSDefnTranslator.Output]] = {
    // TODO: fix .toSeq.toList

    family.domains.toSeq.toList.map {
      case (_, lineage) =>
        translateLineage(lineage)
    }.biFlatten
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {

    lineage.versions.toSeq.toList.map {
      case (_, domain) =>
        translateDomain(domain, lineage)
    }.biFlatten
  }

  private def translateDomain(domain: Domain,
                              lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val evo = lineage.evolution
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain, evo)
        case _ => Right(List.empty)
      }.biFlatten
      evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
      conversionSources <- if (options.generateConversions) {
        generateConversions(domain, lineage, evosToCurrent, defnSources)
      } else {
        Right(List.empty)
      }
      meta <- if (options.csWriteEvolutionDict) {
        generateMeta(domain, lineage)
      } else {
        Right(List.empty)
      }
    } yield {
      defnSources.map(_.output) ++ conversionSources ++ meta
    }
  }

  private def generateMeta(domain: Domain,
                           lineage: BaboonLineage,
  ): Out[List[CSDefnTranslator.Output]] = {
    val basename = tools.basename(domain, lineage.evolution, options)
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    val entries = lineage.evolution
      .typesUnchangedSince(domain.version)
      .toList
      .sortBy(_._1.toString)
      .map {
        case (tid, version) =>
          q"""_unmodified.Add("${tid.toString}", "${version.version}");"""
      }

    val metaTree =
      q"""public sealed class BaboonMeta : $iBaboonMeta
         |{
         |    private BaboonMeta()
         |    {
         |        ${entries.join("\n").shift(8).trim}
         |    }
         |
         |    public String UnmodifiedSince(String typeIdString)
         |    {
         |        return _unmodified[typeIdString];
         |    }
         |
         |    private readonly $csDict<$csString, $csString> _unmodified = new ();
         |
         |    internal static $csLazy<BaboonMeta> LazyInstance = new $csLazy<BaboonMeta>(() => new BaboonMeta());
         |
         |    public static BaboonMeta Instance { get { return LazyInstance.Value; } }
         |}""".stripMargin

    val metaSource = Seq(metaTree).join("\n\n")
    val meta = tools.inNs(pkg.parts.toSeq, metaSource)

    val metaOutput = CSDefnTranslator.Output(
      s"$basename/BaboonMeta.cs",
      meta,
      pkg,
      isTest = false
    )

    Right(List(metaOutput))
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.OutputExt]
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version, lineage.evolution)

    for {
      convs <- lineage.evolution.rules
        .filter(kv => toCurrent.contains(kv._1))
        .map {
          case (srcVer, rules) =>
            handler
              .provide(pkg)
              .provide(srcVer.from)
              .provide[Domain]("current")(domain)
              .provide[Domain]("source")(lineage.versions(srcVer.from))
              .provide(rules)
              .provide(lineage.evolution)
              .produce()
              .use(_.makeConvs())
        }
        .biFlatten
    } yield {
      val conversionRegs = convs.flatMap(_.reg.iterator.toSeq).toSeq
      val missing = convs.flatMap(_.missing.iterator.toSeq).toSeq

      val converter =
        q"""public interface RequiredConversions {
           |    ${missing.join("\n").shift(4).trim}
           |}
           |
           |public sealed class BaboonConversions : $abstractBaboonConversions
           |{
           |    public BaboonConversions(RequiredConversions requiredConversions)
           |    {
           |        ${conversionRegs.join("\n").shift(8).trim}
           |    }
           |
           |    override public $csList<$csString> VersionsFrom()
           |    {
           |        return new $csList<$csString> { ${toCurrent
             .map(_.from.version)
             .map(v => s"""\"$v\"""")
             .mkString(", ")} };
           |    }
           |
           |    override public $csString VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val codecs =
        q"""public sealed class BaboonCodecs : $abstractBaboonCodecs
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    internal static $csLazy<BaboonCodecs> LazyInstance = new $csLazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return LazyInstance.Value; } }
           |}""".stripMargin

      val basename = tools.basename(domain, lineage.evolution, options)

      val runtimeSource = Seq(converter, codecs).join("\n\n")
      val runtime = tools.inNs(pkg.parts.toSeq, runtimeSource)
      val runtimeOutput = CSDefnTranslator.Output(
        s"$basename/BaboonRuntime.cs",
        runtime,
        pkg,
        isTest = false
      )

      val convertersOutput = convs.map { conv =>
        CSDefnTranslator
          .Output(s"$basename/${conv.fname}", conv.conv, pkg, isTest = false)
      }

      List(runtimeOutput) ++ convertersOutput
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val sharedOutput = CSDefnTranslator.Output(
      s"BaboonRuntimeShared.cs",
      TextTree.text(
        IzResources.readAsString("baboon-runtime/cs/BaboonRuntimeShared.cs").get
      ),
      CSBaboonTranslator.baboonRtPkg,
      isTest = false
    )

    val timeOutput = CSDefnTranslator.Output(
      s"BaboonTime.cs",
      TextTree.text(
        IzResources.readAsString("baboon-runtime/cs/BaboonTime.cs").get
      ),
      CSBaboonTranslator.baboonTimePkg,
      isTest = false
    )

    Right(List(sharedOutput, timeOutput))
  }

  private def sharedTestRuntime: Out[List[CSDefnTranslator.Output]] = {
    Right(
      List(
        CSDefnTranslator.Output(
          "BaboonTestRuntimeShared.cs",
          TextTree.text(
            IzResources
              .readAsString("baboon-runtime/cs/BaboonTestRuntimeShared.cs")
              .get
          ),
          CSBaboonTranslator.baboonTestRtPkg,
          isTest = true,
        )
      )
    )
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]],
                                missing: Option[TextTree[CSValue]],
  )

  // Baboon packages
  val baboonRtPkg: CSPackageId = CSPackageId(
    NEList("Baboon", "Runtime", "Shared")
  )
  val baboonTestRtPkg: CSPackageId = CSPackageId(
    NEList("Baboon", "Test", "Runtime", "Shared")
  )
  val baboonTimePkg: CSPackageId = CSPackageId(NEList("Baboon", "Time"))

  // System packages
  val csSystemPkg: CSPackageId = CSPackageId(NEList("System"))
  val csGlobalizationPkg: CSPackageId = CSPackageId(
    NEList("System", "Globalization")
  )
  val csCollectionsGenericPkg: CSPackageId = CSPackageId(
    NEList("System", "Collections", "Generic")
  )
  val csCollectionsImmutablePkg: CSPackageId = CSPackageId(
    NEList("System", "Collections", "Immutable")
  )
  val csLinqPkg: CSPackageId = CSPackageId(NEList("System", "Linq"))
  val csIoPkg: CSPackageId = CSPackageId(NEList("System", "IO"))
  val csTextPkg: CSPackageId = CSPackageId(NEList("System.Text"))
  val csDiagnosticsPkg: CSPackageId = CSPackageId(
    NEList("System", "Diagnostics")
  )

  // Newtonsoft packages
  val nsPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json"))
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))
  val nunitPkg: CSPackageId = CSPackageId(NEList("NUnit", "Framework"))

  // Nunit types
  val nunitTestFixture: CSType =
    CSType(nunitPkg, "TestFixture", fq = false)
  val nunitOneTimeSetUp: CSType =
    CSType(nunitPkg, "OneTimeSetUp", fq = false)
  val testValuesGenerator: CSType =
    CSType(baboonTestRtPkg, "RVG", fq = false)

  // Baboon conversions' types
  val abstractConversion: CSType =
    CSType(baboonRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType =
    CSType(baboonRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType =
    CSType(baboonRtPkg, "IBaboonGenerated", fq = false)
  val iBaboonAdtMemberMeta: CSType =
    CSType(baboonRtPkg, "IBaboonAdtMemberMeta", fq = false)
  val iBaboonGeneratedLatest: CSType =
    CSType(baboonRtPkg, "IBaboonGeneratedLatest", fq = false)
  val BaboonTools: CSType =
    CSType(baboonRtPkg, "BaboonTools", fq = false)

  // Baboon codec types
  val iBaboonCodecData: CSType =
    CSType(baboonRtPkg, "IBaboonCodecData", fq = false)
  val iBaboonBinCodecIndexed: CSType =
    CSType(baboonRtPkg, "IBaboonBinCodecIndexed", fq = false)
  val baboonCodecContext: CSType =
    CSType(baboonRtPkg, "BaboonCodecContext", fq = false)
  val iBaboonCodec: CSType =
    CSType(baboonRtPkg, "IBaboonCodec", fq = false)
  val iBaboonValueCodec: CSType =
    CSType(baboonRtPkg, "IBaboonValueCodec", fq = false)
  val iBaboonStreamCodec: CSType =
    CSType(baboonRtPkg, "IBaboonStreamCodec", fq = false)

  val iBaboonJsonCodec: CSType =
    CSType(baboonRtPkg, "IBaboonJsonCodec", fq = false)
  val iBaboonBinCodec: CSType =
    CSType(baboonRtPkg, "IBaboonBinCodec", fq = false)
  val iBaboonTypeCodecs: CSType =
    CSType(baboonRtPkg, "IBaboonTypeCodecs", fq = false)
  val baboonTypeCodecs: CSType =
    CSType(baboonRtPkg, "BaboonTypeCodecs", fq = false)
  val abstractBaboonCodecs: CSType =
    CSType(baboonRtPkg, "AbstractBaboonCodecs", fq = false)

  val baboonTimeFormats: CSType =
    CSType(baboonTimePkg, "BaboonDateTimeFormats", fq = false)

  val iBaboonMeta: CSType =
    CSType(baboonRtPkg, "IBaboonMeta", fq = false)

  // Baboon type
  val rpDateTime: CSType =
    CSType(baboonTimePkg, "RpDateTime", fq = false)

  // Newtonsoft types
  val nsJsonWriter: CSType =
    CSType(nsPkg, "JsonWriter", fq = false)
  val nsJsonReader: CSType =
    CSType(nsPkg, "JsonReader", fq = false)
  val nsJsonSerializer: CSType =
    CSType(nsPkg, "JsonSerializer", fq = false)
  val nsJsonConverter: CSType =
    CSType(nsPkg, "JsonConverter", fq = false)
  val nsFormatting: CSType =
    CSType(nsPkg, "Formatting", fq = false)
  val nsJToken: CSType =
    CSType(nsLinqPkg, "JToken", fq = false)
  val nsJValue: CSType =
    CSType(nsLinqPkg, "JValue", fq = false)
  val nsJArray: CSType =
    CSType(nsLinqPkg, "JArray", fq = false)
  val nsJObject: CSType =
    CSType(nsLinqPkg, "JObject", fq = false)
  val nsJProperty: CSType =
    CSType(nsLinqPkg, "JProperty", fq = false)
  val nsJTokenType: CSType =
    CSType(nsLinqPkg, "JTokenType", fq = false)

  val binaryReader: CSType =
    CSType(csIoPkg, "BinaryReader", fq = false)
  val binaryWriter: CSType =
    CSType(csIoPkg, "BinaryWriter", fq = false)
  val memoryStream: CSType =
    CSType(csIoPkg, "MemoryStream", fq = false)

  // C# types
  val csString: CSType =
    CSType(csSystemPkg, "String", fq = false)
  val csGuid: CSType =
    CSType(csSystemPkg, "Guid", fq = false)
  val csBoolean: CSType =
    CSType(csSystemPkg, "Boolean", fq = false)
  val csStringBuilder: CSType =
    CSType(csTextPkg, "StringBuilder", fq = false)

  val csSByte: CSType =
    CSType(csSystemPkg, "sbyte", fq = false)
  val csInt16: CSType =
    CSType(csSystemPkg, "Int16", fq = false)
  val csInt32: CSType =
    CSType(csSystemPkg, "Int32", fq = false)
  val csInt64: CSType =
    CSType(csSystemPkg, "Int64", fq = false)

  val csByte: CSType =
    CSType(csSystemPkg, "byte", fq = false)
  val csUInt16: CSType =
    CSType(csSystemPkg, "UInt16", fq = false)
  val csUInt32: CSType =
    CSType(csSystemPkg, "UInt32", fq = false)
  val csUInt64: CSType =
    CSType(csSystemPkg, "UInt64", fq = false)

  val csSingle: CSType =
    CSType(csSystemPkg, "Single", fq = false)
  val csDouble: CSType =
    CSType(csSystemPkg, "Double", fq = false)
  val csDecimal: CSType =
    CSType(csSystemPkg, "Decimal", fq = false)

  val csTpe: CSType =
    CSType(csSystemPkg, "Type", fq = false)
  val csLazy: CSType =
    CSType(csSystemPkg, "Lazy", fq = false)
  val csList: CSType =
    CSType(csCollectionsGenericPkg, "List", fq = false)
  val csDict: CSType =
    CSType(csCollectionsGenericPkg, "Dictionary", fq = false)
  val csSet: CSType =
    CSType(csCollectionsGenericPkg, "HashSet", fq = false)
  val csEnum: CSType =
    CSType(csSystemPkg, "Enum", fq = false)
  val csDateTime: CSType =
    CSType(csSystemPkg, "DateTime", fq = false)
  val csTimeSpan: CSType =
    CSType(csSystemPkg, "TimeSpan", fq = false)
  val csDayOfWeek: CSType =
    CSType(csSystemPkg, "DayOfWeek", fq = false)
  val csArgumentException: CSType =
    CSType(csSystemPkg, "ArgumentException", fq = false)
  val csEnumerable: CSType =
    CSType(csLinqPkg, "Enumerable", fq = false)
  val csRandom: CSType =
    CSType(csSystemPkg, "Random", fq = false)
  val csIComparable: CSType =
    CSType(csSystemPkg, "IComparable", fq = false)
  val csIEquatable: CSType =
    CSType(csSystemPkg, "IEquatable", fq = false)

  val csImmutableDictionary: CSType =
    CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false)
  val csKeyValuePair: CSType =
    CSType(csCollectionsGenericPkg, "KeyValuePair", fq = false)

  val csInvariantCulture: CSType =
    CSType(csGlobalizationPkg, "CultureInfo", fq = false)
  val csDateTimeStyles: CSType =
    CSType(csGlobalizationPkg, "DateTimeStyles", fq = false)
  val csDateTimeKind: CSType =
    CSType(csSystemPkg, "DateTimeKind", fq = false)
  val csTimeZoneInfo: CSType =
    CSType(csSystemPkg, "TimeZoneInfo", fq = false)

  val debug: CSType =
    CSType(csDiagnosticsPkg, "Debug", fq = false)
}

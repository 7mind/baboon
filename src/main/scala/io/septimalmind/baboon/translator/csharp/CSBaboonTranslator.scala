package io.septimalmind.baboon.translator.csharp

import distage.Subcontext
import io.circe.syntax.*
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.RuntimeGenOpt
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.translator.{BaboonAbstractTranslator, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(
                          defnTranslator: CSDefnTranslator,
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
      meta <- buildMeta(family)
      toRender = options.runtime match {
        case RuntimeGenOpt.Only    => rt
        case RuntimeGenOpt.With    => rt ++ translated
        case RuntimeGenOpt.Without => translated
      }
      rendered = toRender.map { o =>
        (o.path, renderTree(o))
      }
      unique <- (rendered ++ meta).toUniqueMap(
        c => NEList(BaboonIssue.NonUniqueOutputFiles(c))
      )
    } yield {
      Sources(unique)
    }
  }

  private def buildMeta(family: BaboonFamily): Out[List[(String, String)]] = {

    val data = family.domains.toSeq.flatMap {
      case (_, lineage) =>
        lineage.versions.toSeq.map {
          case (ver, _) =>
            VersionMeta(lineage.pkg.path.mkString("."), ver.version)
        }

    }
    val meta: OutputMeta = OutputMeta(data.toList)
    val json = meta.asJson.spaces2

    Right(List((s"baboon-meta.json", json)))
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set.empty
      } else {
        Set(systemPkg, genericPkg, linqPkg)
      }

    val forcedUses: Set[CSPackageId] =
      if (options.disregardImplicitUsings) {
        Set(linqPkg)
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
        q"using ${p.parts.mkString(".")};"
      }
      .join("\n")

    val full =
      Seq(
        Seq(q"#nullable enable", q"#pragma warning disable 612,618"),
        Seq(imports),
        Seq(o.tree)
      ).flatten
        .join("\n\n")

    full.mapRender {
      case t: CSValue.CSType if !t.fq =>
        t.name
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
        //val isLatest =
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
    } yield {
      defnSources.map(_.output) ++ conversionSources
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val metaFields =
      (List(q"String id") ++ codecs.toList
        .sortBy(_.getClass.getName)
        .map(_.metaField())).join(", ")

    val base =
      q"""public interface IBaboonGenerated {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonTypeIdentifier();
         |
         |}
         |
         |public interface IBaboonGeneratedLatest : IBaboonGenerated {}
         |
         |public interface IConversion {
         |    public $csTpe TypeFrom();
         |    public $csTpe TypeTo();
         |}
         |
         |public interface IBaboonGeneratedConversion : IConversion
         |{
         |    public IBaboonGenerated Convert<C>(C? context, AbstractBaboonConversions conversions, IBaboonGenerated from);
         |}
         |
         |public interface IDynamicConversion<To> : IConversion
         |{
         |     public To Convert<C>(C? context, AbstractBaboonConversions conversions, dynamic from);
         |}
         |
         |public abstract class AbstractConversion<From, To> : IDynamicConversion<To>, IBaboonGeneratedConversion
         |{
         |    public abstract To Convert<C>(C? context, AbstractBaboonConversions conversions, From from);
         |
         |    public To Convert<C>(C? context, AbstractBaboonConversions conversions, dynamic from)
         |    {
         |        return Convert<C>(context, conversions, (From)from);
         |    }
         |
         |    IBaboonGenerated IBaboonGeneratedConversion.Convert<C>(C? context, $abstractBaboonConversions conversions, $iBaboonGenerated from) where C : default
         |    {
         |        if (from is not From fr)
         |        {
         |            throw new Exception(
         |                $"Can't use IBaboonGeneratedConversion interface when from is not of type {typeof(To).FullName}");
         |        }
         |        var res = Convert(context, conversions, fr);
         |
         |        if (res is not $iBaboonGenerated bg)
         |        {
         |            throw new $csArgumentException(
         |                $$"Can't use IBaboonGeneratedConversion interface for non IBaboonGenerated return type To = {typeof(To).FullName}");
         |        }
         |        return bg;
         |    }
         |
         |    public $csTpe TypeFrom() {
         |         return typeof(From);
         |    }
         |
         |     public $csTpe TypeTo() {
         |         return typeof(To);
         |     }
         |}
         |
         |public interface IBaboonCodecAbstract {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonTypeIdentifier();
         |}
         |
         |public interface IBaboonCodec<Instance> : IBaboonCodecAbstract {}
         |
         |public interface IBaboonValueCodec<Instance, Wire> : IBaboonCodec<Instance>
         |{
         |    Wire Encode(Instance instance);
         |    Instance Decode(Wire wire);
         |}
         |
         |public interface IBaboonJsonCodec<Instance> : IBaboonValueCodec<Instance, $nsJToken> {}
         |
         |public interface IBaboonStreamCodec<Instance, OutStream, InStream> : IBaboonCodec<Instance>
         |{
         |    void Encode(OutStream writer, Instance instance);
         |    Instance Decode(InStream wire);
         |}
         |
         |public interface IBaboonBinCodec<Instance> : IBaboonStreamCodec<Instance, $binaryWriter, $binaryReader> {}
         |
         |public class BaboonTools {
         |    public static A? ReadNullableValue<A>(Boolean ifNot, Func<A> thenReturn) where A: struct
         |    {
         |        if (ifNot) return null;
         |        return thenReturn();
         |    }
         |
         |    public static A? ReadNullableValue<A>($nsJToken? token, Func<$nsJToken, A> readValue) where A: struct
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |
         |    public static A? ReadValue<A>($nsJToken? token, Func<$nsJToken, A> readValue) where A: class
         |    {
         |        if (token == null || token.Type == $nsJTokenType.Null) return null;
         |        return readValue(token);
         |    }
         |}
         |
         |public record BaboonCodecImpls($metaFields);
         |
         |public abstract class BaboonAbstractCodecs
         |{
         |
         |    private $csDict<$csString, BaboonCodecImpls> codecs = new ();
         |
         |    public void Register(BaboonCodecImpls impls)
         |    {
         |        codecs[impls.id] = impls;
         |    }
         |
         |    public BaboonCodecImpls Find($csString id)
         |    {
         |        return codecs[id];
         |    }
         |
         |    public bool TryFind(String id, out BaboonCodecImpls? value)
         |    {
         |        return codecs.TryGetValue(id, out value);
         |    }
         |}""".stripMargin

    val key =
      q"""public class ConversionKey
         |{
         |    protected bool Equals(ConversionKey other)
         |    {
         |        return TypeFrom.Equals(other.TypeFrom) && TypeTo.Equals(other.TypeTo);
         |    }
         |
         |    public override bool Equals(object? obj)
         |    {
         |        if (ReferenceEquals(null, obj)) return false;
         |        if (ReferenceEquals(this, obj)) return true;
         |        if (obj.GetType() != this.GetType()) return false;
         |        return Equals((ConversionKey)obj);
         |    }
         |
         |    public override int GetHashCode()
         |    {
         |        return HashCode.Combine(TypeFrom, TypeTo);
         |    }
         |
         |    public ConversionKey($csTpe typeFrom, $csTpe typeTo)
         |    {
         |        TypeFrom = typeFrom;
         |        TypeTo = typeTo;
         |    }
         |
         |    public $csTpe TypeFrom { get; }
         |    public $csTpe TypeTo {get; }
         |}""".stripMargin

    val abstractAggregator =
      q"""public abstract class AbstractBaboonConversions
         |{
         |    private $csDict<ConversionKey, IConversion> convs = new ();
         |    private Dictionary<Type, List<IConversion>> convsWild = new ();
         |
         |    public abstract $csList<$csString> VersionsFrom();
         |
         |    public abstract String VersionTo();
         |
         |    public $csList<IConversion> AllConversions()
         |    {
         |        return convs.Values.ToList();
         |    }
         |
         |    public void Register(IConversion conversion)
         |    {
         |        var fromType = conversion.TypeFrom();
         |        var key = new ConversionKey(fromType, conversion.TypeTo());
         |        var wild = convsWild.TryGetValue(fromType, out var v) ? v : new List<IConversion>();
         |        wild.Add(conversion);
         |        convs[key] = conversion;
         |        convsWild[fromType] = wild;
         |    }
         |
         |    public void Register<From, To>(AbstractConversion<From, To> conversion)
         |    {
         |        var tFrom = typeof(From);
         |        var tTo = typeof(To);
         |        var key = new ConversionKey(tFrom, tTo);
         |        var wild = convsWild.TryGetValue(tFrom, out var v) ? v : new List<IConversion>();
         |        wild.Add(conversion);
         |        convs[key] = conversion;
         |        convsWild[tFrom] = wild;
         |    }
         |
         |    public IBaboonGenerated ConvertWithContext<C>(C? c, IBaboonGenerated from, IConversion conversion)
         |    {
         |        var tconv = (IBaboonGeneratedConversion)conversion;
         |        return tconv.Convert<C>(c, this, from);
         |    }
         |
         |    public IBaboonGenerated Convert(IBaboonGenerated from, IConversion conversion)
         |    {
         |        var tconv = (IBaboonGeneratedConversion)conversion;
         |        return tconv.Convert<Object>(null, this, from);
         |    }
         |
         |    public IReadOnlyList<IConversion> FindConversions(IBaboonGenerated value)
         |    {
         |        return !convsWild.TryGetValue(value.GetType(), out var res) ? new List<IConversion>() : res;
         |    }
         |
         |    public To ConvertWithContext<C, From, To>(C? c, From from)
         |    {
         |        var tFrom = typeof(From);
         |        var tTo = typeof(To);
         |
         |        if (from is To direct)
         |        {
         |            return direct;
         |        }
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        var conv = convs[key];
         |        var tconv = ((AbstractConversion<From, To>)conv);
         |        return tconv.Convert(c, this, from);
         |    }
         |
         |    public To ConvertWithContextDynamic<C, To>(C? c, Type tFrom, dynamic from)
         |    {
         |        var tTo = typeof(To);
         |
         |        if (from is To direct)
         |        {
         |            return direct;
         |        }
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        var conv = convs[key];
         |        var tconv = ((IDynamicConversion<To>)conv);
         |        return tconv.Convert(c, this, from);
         |    }
         |
         |    public To ConvertDynamic<To>(dynamic from)
         |        where To : IBaboonGenerated
         |    {
         |        return ConvertWithContextDynamic<Object, To>(null, from.GetType(), from);
         |    }
         |
         |    public To Convert<From, To>(From from)
         |        where From : IBaboonGenerated
         |        where To : IBaboonGenerated
         |    {
         |        return ConvertWithContext<Object, From, To>(null, from);
         |    }
         |
         |}""".stripMargin

    val formats =
      q"""public static class BaboonDateTimeFormats {
         |    public static readonly $csString TslDefault = "yyyy-MM-ddTHH:mm:ss.fff";
         |    public static readonly $csString[] Tsl = new string[] {
         |                "yyyy-MM-ddTHH:mm:ss",
         |                "yyyy-MM-ddTHH:mm:ss.f",
         |                "yyyy-MM-ddTHH:mm:ss.ff",
         |                "yyyy-MM-ddTHH:mm:ss.fff",
         |                "yyyy-MM-ddTHH:mm:ss.ffff",
         |                "yyyy-MM-ddTHH:mm:ss.fffff",
         |                "yyyy-MM-ddTHH:mm:ss.ffffff",
         |                "yyyy-MM-ddTHH:mm:ss.fffffff",
         |                "yyyy-MM-ddTHH:mm:ss.ffffffff",
         |                "yyyy-MM-ddTHH:mm:ss.fffffffff"
         |            };
         |
         |    public static readonly $csString TszDefault = "yyyy-MM-ddTHH:mm:ss.fffzzz";
         |    public static readonly $csString[] Tsz = new string[] {
         |               "yyyy-MM-ddTHH:mm:ssZ",
         |               "yyyy-MM-ddTHH:mm:ss.fZ",
         |               "yyyy-MM-ddTHH:mm:ss.ffZ",
         |               "yyyy-MM-ddTHH:mm:ss.fffZ",
         |               "yyyy-MM-ddTHH:mm:ss.ffffZ",
         |               "yyyy-MM-ddTHH:mm:ss.fffffZ",
         |               "yyyy-MM-ddTHH:mm:ss.ffffffZ",
         |               "yyyy-MM-ddTHH:mm:ss.fffffffZ",
         |               "yyyy-MM-ddTHH:mm:ss.ffffffffZ",
         |               "yyyy-MM-ddTHH:mm:ss.fffffffffZ",
         |               "yyyy-MM-ddTHH:mm:sszzz",
         |               "yyyy-MM-ddTHH:mm:ss.fzzz",
         |               "yyyy-MM-ddTHH:mm:ss.ffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.fffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.ffffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.fffffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.ffffffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.fffffffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.ffffffffzzz",
         |               "yyyy-MM-ddTHH:mm:ss.fffffffffzzz"
         |            };
         |
         |    public static readonly $csString TsuDefault = "yyyy-MM-ddTHH:mm:ss.fffZ";
         |    public static readonly $csString[] Tsu = Tsz;
         |
         |    public static $csString ToString($csDateTime dt) {
         |        return dt.ToString(dt.Kind == $csDateTimeKind.Utc ? TsuDefault : TszDefault, $csInvariantCulture.InvariantCulture);
         |    }
         |    public static $csDateTime FromString($csString dt) {
         |        return $csDateTime.ParseExact(dt, Tsz, $csInvariantCulture.InvariantCulture, $csDateTimeStyles.None);
         |    }
         |}
         |""".stripMargin

    val runtime = Seq(key, base, abstractAggregator, formats).join("\n\n")

    val rt =
      tools.inNs(CSBaboonTranslator.sharedRtPkg.parts.toSeq, runtime)

    Right(
      List(
        CSDefnTranslator
          .Output(
            s"Baboon-Runtime-Shared.cs",
            rt,
            CSBaboonTranslator.sharedRtPkg
          )
      )
    )
  }

  private def generateConversions(
    domain: Domain,
    lineage: BaboonLineage,
    toCurrent: Set[EvolutionStep],
    defnOut: List[CSDefnTranslator.OutputExt]
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version)

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
           |public class BaboonConversions : ${CSBaboonTranslator.abstractBaboonConversions}
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
        q"""public class BaboonCodecs : ${CSBaboonTranslator.abstractBaboonCodecs}
           |{
           |    private BaboonCodecs()
           |    {
           |        ${defnOut.map(_.codecReg).join("\n").shift(8).trim}
           |    }
           |
           |    private static Lazy<BaboonCodecs> instance = new Lazy<BaboonCodecs>(() => new BaboonCodecs());
           |
           |    public static BaboonCodecs Instance { get { return instance.Value; } }
           |}""".stripMargin

      val runtime = Seq(converter, codecs).join("\n\n")

      val rt = tools.inNs(pkg.parts.toSeq, runtime)

      List(
        CSDefnTranslator
          .Output(s"${tools.basename(domain)}/main/Baboon-Runtime.cs", rt, pkg)
      ) ++ convs.map { conv =>
        CSDefnTranslator
          .Output(s"${tools.basename(domain)}/${conv.fname}", conv.conv, pkg)
      }
    }
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]],
                                missing: Option[TextTree[CSValue]],
  )

  val sharedRtPkg: CSPackageId = CSPackageId(
    NEList("Baboon", "Runtime", "Shared")
  )
  val systemPkg: CSPackageId = CSPackageId(NEList("System"))
  val genericPkg: CSPackageId = CSPackageId(
    NEList("System", "Collections", "Generic")
  )
  val linqPkg: CSPackageId = CSPackageId(NEList("System", "Linq"))
  val ioPkg: CSPackageId = CSPackageId(NEList("System", "IO"))
  val nsPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json"))
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))

  val abstractConversion: CSType =
    CSType(sharedRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType =
    CSType(sharedRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType =
    CSType(sharedRtPkg, "IBaboonGenerated", fq = false)
  val iBaboonGeneratedLatest: CSType =
    CSType(sharedRtPkg, "IBaboonGeneratedLatest", fq = false)
  val BaboonTools: CSType =
    CSType(sharedRtPkg, "BaboonTools", fq = false)

  val iBaboonCodecAbstract: CSType =
    CSType(sharedRtPkg, "IBaboonCodecAbstract", fq = false)
  val iBaboonCodec: CSType =
    CSType(sharedRtPkg, "IBaboonCodec", fq = false)
  val iBaboonValueCodec: CSType =
    CSType(sharedRtPkg, "IBaboonValueCodec", fq = false)
  val iBaboonStreamCodec: CSType =
    CSType(sharedRtPkg, "IBaboonStreamCodec", fq = false)

  val iBaboonJsonCodec: CSType =
    CSType(sharedRtPkg, "IBaboonJsonCodec", fq = false)
  val iBaboonBinCodec: CSType =
    CSType(sharedRtPkg, "IBaboonBinCodec", fq = false)
  val baboonCodecImpls: CSType =
    CSType(sharedRtPkg, "BaboonCodecImpls", fq = false)
  val abstractBaboonCodecs: CSType =
    CSType(sharedRtPkg, "BaboonAbstractCodecs", fq = false)

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
    CSType(ioPkg, "BinaryReader", fq = false)
  val binaryWriter: CSType =
    CSType(ioPkg, "BinaryWriter", fq = false)

  val csTpe: CSType =
    CSType(systemPkg, "Type", fq = false)
  val csLazy: CSType =
    CSType(systemPkg, "Lazy", fq = false)
  val csList: CSType =
    CSType(genericPkg, "List", fq = false)
  val csDict: CSType =
    CSType(genericPkg, "Dictionary", fq = false)
  val csString: CSType =
    CSType(systemPkg, "String", fq = false)
  val csGuid: CSType =
    CSType(systemPkg, "Guid", fq = false)
  val csEnum: CSType =
    CSType(systemPkg, "Enum", fq = false)
  val csDateTime: CSType =
    CSType(systemPkg, "DateTime", fq = false)
  val csArgumentException: CSType =
    CSType(systemPkg, "ArgumentException", fq = false)
  val csEnumerable: CSType =
    CSType(linqPkg, "Enumerable", fq = false)

  val csKeyValuePair: CSType =
    CSType(genericPkg, "KeyValuePair", fq = false)

  val csNotImplementedException: CSType =
    CSType(
      CSPackageId("System.Runtime.Serialization"),
      "NotImplementedException",
      fq = false
    )

  val csInvariantCulture: CSType = CSType(
    CSPackageId("System.Globalization"),
    "CultureInfo",
    fq = false
  )

  val csDateTimeStyles: CSType = CSType(
    CSPackageId("System.Globalization"),
    "DateTimeStyles",
    fq = false
  )

  val csDateTimeKind: CSType = CSType(
    CSPackageId("System"),
    "DateTimeKind",
    fq = false
  )

  val csDateTimeFormats: CSType = CSType(
    sharedRtPkg,
    "BaboonDateTimeFormats",
    fq = false
  )
}

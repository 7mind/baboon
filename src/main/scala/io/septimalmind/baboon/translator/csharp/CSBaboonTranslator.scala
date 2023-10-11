package io.septimalmind.baboon.translator.csharp

import io.circe.syntax.*
import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.RuntimeGenOpt
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.translator.{AbstractBaboonTranslator, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.distage.LocalContext
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.functional.Identity
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSBaboonTranslator(
  defnTranslator: CSDefnTranslator,
  trans: CSTypeTranslator,
  handler: LocalContext[Identity, IndividualConversionHandler],
  options: CompilerOptions
) extends AbstractBaboonTranslator {

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
        translateDomain(domain, lineage.evolution)
    }.biFlatten
  }

  private def translateDomain(domain: Domain,
                              evo: BaboonEvolution,
  ): Out[List[CSDefnTranslator.Output]] = {
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain, evo)
        case _ => Right(List.empty)
      }.biFlatten
      evosToCurrent = evo.diffs.keySet.filter(_.to == domain.version)
      conversionSources <- if (options.generateConversions) {
        generateConversions(domain, evo, evosToCurrent)
      } else {
        Right(List.empty)
      }
    } yield {
      defnSources ++ conversionSources
    }
  }

  private def sharedRuntime(): Out[List[CSDefnTranslator.Output]] = {
    val base =
      q"""public interface IBaboonGenerated {
         |    public $csString BaboonDomainVersion();
         |    public $csString BaboonDomainIdentifier();
         |    public $csString BaboonTypeIdentifier();
         |}
         |
         |public interface IBaboonGeneratedLatest : IBaboonGenerated {}
         |
         |public interface IConversion {
         |    public $csTpe TypeFrom();
         |    public $csTpe TypeTo();
         |}
         |
         |public interface IDynamicConversion<To> : IConversion
         |{
         |     public To Convert<C>(C context, AbstractBaboonConversions conversions, dynamic from);
         |}
         |
         |public abstract class AbstractConversion<From, To> : IDynamicConversion<To>
         |{
         |    public abstract To Convert<C>(C context, AbstractBaboonConversions conversions, From from);
         |
         |    public To Convert<C>(C context, AbstractBaboonConversions conversions, dynamic from)
         |    {
         |        return Convert<C>(context, conversions, (From)from);
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
         |interface IBaboonCodec<Instance> {}
         |
         |interface IBaboonValueCodec<Instance, Wire> : IBaboonCodec<Instance>
         |{
         |    Wire Encode(Instance instance);
         |    Instance Decode(Wire wire);
         |}
         |
         |interface IBaboonJsonCodec<Instance> : IBaboonValueCodec<Instance, $nsJToken> {}
         |
         |interface IBaboonStreamCodec<Instance, OutStream, InStream> : IBaboonCodec<Instance>
         |{
         |    void Encode(OutStream writer, Instance instance);
         |    Instance Decode(InStream wire);
         |}
         |
         |interface IBaboonBinCodec<Instance> : IBaboonStreamCodec<Instance, $binaryWriter, $binaryReader> {}
         |""".stripMargin

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
         |        var key = new ConversionKey(conversion.TypeFrom(), conversion.TypeTo());
         |        convs.Add(key, conversion);
         |    }
         |
         |    public void Register<From, To>(AbstractConversion<From, To> conversion)
         |    {
         |        var tFrom = typeof(From);
         |        var tTo = typeof(To);
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        convs.Add(key, conversion);
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
         |    public To ConvertWithContextDynamic<C, To>(C? c, $csTpe tFrom, dynamic from)
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
         |    {
         |        return ConvertWithContextDynamic<Object, To>(null, from.GetType(), from);
         |    }
         |
         |    public To Convert<From, To>(From from)
         |    {
         |        return ConvertWithContext<Object, From, To>(null, from);
         |    }
         |
         |
         |}""".stripMargin

    val runtime = Seq(key, base, abstractAggregator).join("\n\n")

    val rt =
      defnTranslator.inNs(CSBaboonTranslator.sharedRtPkg.parts.toSeq, runtime)

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

  private def generateConversions(domain: Domain,
                                  value: BaboonEvolution,
                                  toCurrent: Set[EvolutionStep],
  ): Out[List[CSDefnTranslator.Output]] = {
    val pkg = trans.toCsPkg(domain.id, domain.version)

    for {
      convs <- value.rules
        .filter(kv => toCurrent.contains(kv._1))
        .map {
          case (srcVer, rules) =>
            handler
              .provide(pkg)
              .provide(srcVer.from)
              .provide(domain)
              .provide(rules)
              .produce()
              .use(_.makeConvs())
        }
        .biFlatten
    } yield {
      val regs = convs.flatMap(_.reg.iterator.toSeq).toSeq
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
           |        ${regs.join("\n").shift(8).trim}
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

      val runtime = Seq(converter).join("\n\n")

      val rt = defnTranslator.inNs(pkg.parts.toSeq, runtime)

      List(
        CSDefnTranslator
          .Output(
            s"${defnTranslator.basename(domain)}/Baboon-Runtime.cs",
            rt,
            pkg
          )
      ) ++ convs.map { conv =>
        CSDefnTranslator
          .Output(
            s"${defnTranslator.basename(domain)}/${conv.fname}",
            conv.conv,
            pkg
          )
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
  val nsLinqPkg: CSPackageId = CSPackageId(NEList("Newtonsoft", "Json", "Linq"))

  val abstractConversion: CSType =
    CSType(sharedRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType =
    CSType(sharedRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType =
    CSType(sharedRtPkg, "IBaboonGenerated", fq = false)
  val iBaboonGeneratedLatest: CSType =
    CSType(sharedRtPkg, "IBaboonGeneratedLatest", fq = false)

  val iBaboonCodec: CSType =
    CSType(sharedRtPkg, "IBaboonCodec", fq = false)
  val iBaboonValueCodec: CSType =
    CSType(sharedRtPkg, "IBaboonValueCodec", fq = false)
  val iBaboonJsonCodec: CSType =
    CSType(sharedRtPkg, "IBaboonJsonCodec", fq = false)
  val iBaboonStreamCodec: CSType =
    CSType(sharedRtPkg, "IBaboonStreamCodec", fq = false)
  val iBaboonBinCodec: CSType =
    CSType(sharedRtPkg, "IBaboonBinCodec", fq = false)

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
  val csEnum: CSType =
    CSType(systemPkg, "Enum", fq = false)
  val csArgumentException: CSType =
    CSType(systemPkg, "ArgumentException", fq = false)
  val csKeyValuePair: CSType =
    CSType(genericPkg, "KeyValuePair", fq = false)
}

package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
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

class CSBaboonTranslator(defnTranslator: CSDefnTranslator, trans: CSTypeTranslator, handler: LocalContext[Identity, IndividualConversionHandler])
    extends AbstractBaboonTranslator {

  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(family)
      rt <- sharedRuntime()
      rendered = (rt ++ translated).map { o =>
        (o.path, renderTree(o))
      }
      unique <- rendered.toUniqueMap(
        c => NEList(BaboonIssue.NonUniqueOutputFiles(c))
      )
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] = Set(
      CSPackageId(NEList("System")),
      CSPackageId(NEList("System", "Collections", "Generic")),
      CSPackageId(NEList("System", "Linq")),
    )

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
      (requiredPackages ++ usedPackages).diff(available ++ alwaysAvailable)

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
      conversionSources <- if (evosToCurrent.nonEmpty) {
        generateConversions(domain, evo, evosToCurrent)
      } else {
        Right(List.empty)
      }
    } yield {
      defnSources ++ conversionSources
    }
  }

  private def sharedRuntime() : Out[List[CSDefnTranslator.Output]] = {
    val base =
      q"""public interface IBaboonGenerated {}
         |
         |public interface IConversion {
         |    public Type TypeFrom();
         |    public Type TypeTo();
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
         |    public Type TypeFrom() {
         |         return typeof(From);
         |    }
         |
         |     public Type TypeTo() {
         |         return typeof(To);
         |     }
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
         |    public ConversionKey(Type typeFrom, Type typeTo)
         |    {
         |        TypeFrom = typeFrom;
         |        TypeTo = typeTo;
         |    }
         |
         |    public Type TypeFrom { get; }
         |    public Type TypeTo {get; }
         |}""".stripMargin

    val abstractAggregator =
      q"""public abstract class AbstractBaboonConversions
         |{
         |    private Dictionary<ConversionKey, IConversion> convs = new ();
         |
         |    public abstract List<String> VersionsFrom();
         |
         |    public abstract String VersionTo();
         |
         |    public List<IConversion> AllConversions()
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

    val rt = defnTranslator.inNs(CSBaboonTranslator.sharedRtPkg.parts.toSeq, runtime)

    Right(List(
      CSDefnTranslator
        .Output(s"Baboon-Runtime-Shared.cs", rt, CSBaboonTranslator.sharedRtPkg)
    ))
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
            handler.provide(pkg).provide(srcVer.from).provide(domain).provide(rules).produce().use(_.makeConvs())
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
           |    override public List<String> VersionsFrom()
           |    {
           |        return new List<String> { ${toCurrent.map(_.from.version).map(v => s"\"$v\"").mkString(", ")} };
           |    }
           |
           |    override public String VersionTo()
           |    {
           |        return "${domain.version.version}";
           |    }
           |}""".stripMargin

      val runtime = Seq(converter).join("\n\n")

      val rt = defnTranslator.inNs(pkg.parts.toSeq, runtime)

      List(
        CSDefnTranslator
          .Output(s"${defnTranslator.basename(domain)}/Baboon-Runtime.cs", rt, pkg)
      ) ++ convs.map { conv =>
        CSDefnTranslator
          .Output(s"${defnTranslator.basename(domain)}/${conv.fname}", conv.conv, pkg)
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


  val sharedRtPkg: CSPackageId = CSPackageId(NEList("Baboon", "Runtime", "Shared"))
  val abstractConversion: CSType = CSType(sharedRtPkg, "AbstractConversion", fq = false)
  val abstractBaboonConversions: CSType = CSType(sharedRtPkg, "AbstractBaboonConversions", fq = false)
  val iBaboonGenerated: CSType = CSType(sharedRtPkg, "IBaboonGenerated", fq = false)

}

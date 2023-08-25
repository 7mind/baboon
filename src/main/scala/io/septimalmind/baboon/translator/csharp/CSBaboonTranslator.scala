package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.translator.{
  AbstractBaboonTranslator,
  Sources,
  TextTree
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

class CSBaboonTranslator(options: CompilerOptions)
    extends AbstractBaboonTranslator {

  type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

  private val defnTranslator =
    new CSDefnTranslator.CSDefnTranslatorImpl(options)

  override def translate(family: BaboonFamily): Out[Sources] = {
    for {
      translated <- doTranslate(family)
      rendered = translated.map { o =>
        (o.path, renderTree(o))
      }
      unique <- rendered.toUniqueMap(
        c => NonEmptyList(BaboonIssue.NonUniqueOutputFiles(c))
      )
    } yield {
      Sources(unique)
    }
  }

  private def renderTree(o: CSDefnTranslator.Output): String = {
    val alwaysAvailable: Set[CSPackageId] = Set(
      CSPackageId(NonEmptyList("System")),
      CSPackageId(NonEmptyList("System", "Collections", "Generic")),
      CSPackageId(NonEmptyList("System", "Linq")),
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
    }.biFlatAggregate
  }

  private def translateLineage(
    lineage: BaboonLineage
  ): Out[List[CSDefnTranslator.Output]] = {

    lineage.versions.toSeq.toList.map {
      case (_, domain) =>
        //val isLatest =
        translateDomain(domain, lineage.evolution)
    }.biFlatAggregate
  }

  private def translateDomain(domain: Domain,
                              evo: BaboonEvolution,
  ): Out[List[CSDefnTranslator.Output]] = {
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain, evo)
        case _ => Right(List.empty)
      }.biFlatAggregate
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

  private def generateConversions(domain: Domain,
                                  value: BaboonEvolution,
                                  toCurrent: Set[EvolutionStep],
  ): Out[List[CSDefnTranslator.Output]] = {
    val transd = new CSDefnTranslator.CSDefnTranslatorImpl(options)
    val trans = new CSTypeTranslator()
    val pkg = trans.toCsPkg(domain.id, domain.version)

    val base =
      q"""public interface IConversion {
         |    public Type TypeFrom();
         |    public Type TypeTo();
         |}
         |
         |public interface IDynamicConversion<To> : IConversion
         |{
         |     public To Convert<C>(C context, BaboonConversions conversions, dynamic from);
         |}
         |
         |public abstract class AbstractConversion<From, To> : IDynamicConversion<To>
         |{
         |    public abstract To Convert<C>(C context, BaboonConversions conversions, From from);
         |
         |    public To Convert<C>(C context, BaboonConversions conversions, dynamic from)
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

    for {
      convs <- value.rules
        .filter(kv => toCurrent.contains(kv._1))
        .map {
          case (srcVer, rules) =>
            new IndividualConversionHandler(
              transd,
              trans,
              pkg,
              srcVer.from,
              domain,
              rules
            ).makeConvs()
        }
        .biFlatAggregate
    } yield {
      val regs = convs.flatMap(_.reg.iterator.toSeq).toSeq

      val converter =
        q"""public class BaboonConversions
           |{
           |    private Dictionary<ConversionKey, IConversion> convs = new ();
           |
           |    public BaboonConversions()
           |    {
           |        ${regs.join("\n").shift(8).trim}
           |    }
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

      val runtime = Seq(base, key, converter).join("\n\n")

      val rt = transd.inNs(pkg.parts.toSeq, runtime)

      List(
        CSDefnTranslator
          .Output(s"${transd.basename(domain)}/Baboon-Runtime.cs", rt, pkg)
      ) ++ convs.map { conv =>
        CSDefnTranslator
          .Output(s"${transd.basename(domain)}/${conv.fname}", conv.conv, pkg)
      }
    }
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]])
}

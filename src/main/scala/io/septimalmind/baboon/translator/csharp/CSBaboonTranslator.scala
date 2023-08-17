package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{
  AbstractBaboonTranslator,
  Sources,
  TextTree
}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.IzCollections.*
import izumi.fundamentals.collections.nonempty.NonEmptyList
import TextTree.*
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId

class CSBaboonTranslator() extends AbstractBaboonTranslator {

  type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

  private val defnTranslator = new CSDefnTranslator.CSDefnTranslatorImpl()

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
    val requiredPackages: Set[CSPackageId] = Set.empty /*Set(
      CSPackageId(NonEmptyList("System")),
      CSPackageId(NonEmptyList("System", "Collections", "Generic")),
      CSPackageId(NonEmptyList("System", "Linq")),
    )*/

    val usedPackages = o.tree.values.collect {
      case t: CSValue.CSType if !t.fq => t.pkg
    }.toSet

    val available = Set(o.pkg)
    val allPackages = (requiredPackages ++ usedPackages).diff(available)

    val imports = allPackages.toSeq
      .map { p =>
        q"using ${p.parts.mkString(".")};"
      }
      .join("\n")

    val full =
      Seq(Seq(q"#nullable enable"), Seq(imports), Seq(o.tree)).flatten
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
      case (version, domain) =>
        val evo = if (lineage.evolution.latest == version) {
          Some(lineage.evolution)
        } else {
          None
        }
        translateDomain(domain, evo)
    }.biFlatAggregate
  }

  private def translateDomain(
    domain: Domain,
    evo: Option[BaboonEvolution]
  ): Out[List[CSDefnTranslator.Output]] = {
    for {
      defnSources <- domain.defs.meta.nodes.toList.map {
        case (_, defn: DomainMember.User) =>
          defnTranslator.translate(defn, domain)
        case _ => Right(List.empty)
      }.biFlatAggregate
      conversionSources <- evo match {
        case Some(value) =>
          generateConversions(domain, value)
        case None =>
          Right(List.empty)
      }
    } yield {
      defnSources ++ conversionSources
    }
  }

  private def generateConversions(
    domain: Domain,
    value: BaboonEvolution
  ): Out[List[CSDefnTranslator.Output]] = {
    // TODO
    assert(domain.version == value.latest)

    val transd = new CSDefnTranslator.CSDefnTranslatorImpl()
    val trans = new CSTypeTranslator()
    val pkg = trans.toCsPkg(domain.id, domain.version)

    val convs = value.rules.flatMap {
      case (srcVer, rules) =>
        makeConvs(transd, trans, pkg, srcVer, domain, rules)
    }

    val base = q"""public interface IConversion<From, To>
                  |{
                  |    public To Convert<Ctx>(Ctx context, BaboonConversions conversions, From from);
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

    val regs = convs.flatMap(_.reg.iterator.toSeq).toSeq
    val converter =
      q"""
         |public class BaboonConversions
         |{
         |    private Dictionary<ConversionKey, dynamic> convs = new ();
         |
         |    public BaboonConversions()
         |    {
         |    ${regs.join("\n").shift(4)}
         |    }
         |
         |    public void Register<From, To>(IConversion<From, To> conversion)
         |    {
         |        var tFrom = typeof(From);
         |        var tTo = typeof(To);
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        convs.Add(key, conversion);
         |    }
         |
         |    public To ConvertWithContext<C, From, To>(C c, From from)
         |    {
         |        var tFrom = typeof(From);
         |        var tTo = typeof(To);
         |        var key = new ConversionKey(tFrom, tTo);
         |
         |        var conv = convs[key];
         |        var tconv = ((IConversion<From, To>)conv);
         |        return tconv.Convert(c, this, from);
         |    }
         |
         |    public To Convert<From, To>(From from)
         |    {
         |        return ConvertWithContext<Object, From, To>(null, from);
         |    }
         |
         |}""".stripMargin

    val runtime = Seq(base, key, converter).join("\n\n")

    val rt = transd.inNs(pkg.parts.toSeq, runtime)

    Right(
      List(
        CSDefnTranslator
          .Output(s"${transd.basename(domain)}/Baboon-Runtime.cs", rt, pkg)
      ) ++ convs.map { conv =>
        CSDefnTranslator
          .Output(s"${transd.basename(domain)}/${conv.fname}", conv.conv, pkg)
      }
    )
  }

  private def makeConvs(transd: CSDefnTranslator.CSDefnTranslatorImpl,
                        trans: CSTypeTranslator,
                        pkg: CSPackageId,
                        srcVer: Version,
                        domain: Domain,
                        rules: BaboonRuleset): List[RenderedConversion] = {
    //Register(new Convert_Testpkg_Pkg0_1_0_0_T1_D1_TO_Testpkg_Pkg0_2_0_0_T1_D1().GetConverter());

    rules.conversions.flatMap { conv =>
      val convname = Seq(
        "Convert",
        conv.sourceTpe.name.name,
        "From",
        srcVer.version.replace(".", "_")
      ).mkString("__")

      val fname = s"from-${srcVer.version}-${conv.sourceTpe.name.name}.cs"
      val tin = trans.toCsVal(conv.sourceTpe, srcVer).fullyQualified
      val tout =
        trans.toCsVal(conv.sourceTpe, domain.version).fullyQualified

      conv match {
        case _: Conversion.CustomConversionRequired =>
          val cdefn =
            q"""public abstract class ${convname} : IConversion<${tin}, ${tout}>
               |{
               |    public abstract ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from);
               |}""".stripMargin
          val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
          List(RenderedConversion(fname, ctree, None))
        case _: Conversion.RemovedTypeNoConversion =>
          List.empty
        case _: Conversion.CopyEnumByName =>
          val cdefn =
            q"""public class ${convname} : IConversion<${tin}, ${tout}>
               |{
               |    public ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from) {
               |        if (Enum.TryParse(from.ToString(), out ${tout} parsed))
               |        {
               |            return parsed;
               |        }
               |        else
               |        {
               |            throw new ArgumentException("Bad input, this is a Baboon bug");
               |        }
               |    }
               |}""".stripMargin
          val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
          val regtree = q"Register(new ${convname}());"
          List(RenderedConversion(fname, ctree, Some(regtree)))
        case c: Conversion.DtoConversion =>
          List.empty
        case c: Conversion.CopyAdtBranchByName =>
          List.empty
      }
    }
  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]])
}

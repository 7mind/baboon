package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.{
  AbstractBaboonTranslator,
  Sources,
  TextTree
}
import io.septimalmind.baboon.typer.model.{TypeId, *}
import izumi.fundamentals.collections.IzCollections.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.NonEmptyList
import TextTree.*
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.Conversion.FieldOp

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

    val base =
      q"""public interface IDynamicConversion<To>
         | {
         |     public To Convert<C>(C context, BaboonConversions conversions, dynamic from);
         | }
         |
         |public abstract class AbstractConversion<From, To> : IDynamicConversion<To>
         |{
         |    public abstract To Convert<C>(C context, BaboonConversions conversions, From from);
         |
         |    public To Convert<C>(C context, BaboonConversions conversions, dynamic from)
         |    {
         |        return Convert<C>(context, conversions, (From)from);
         |    }
         |}
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
      convs <- value.rules.map {
        case (srcVer, rules) =>
          makeConvs(transd, trans, pkg, srcVer, domain, rules)
      }.biFlatAggregate
    } yield {
      val regs = convs.flatMap(_.reg.iterator.toSeq).toSeq

      val converter =
        q"""public class BaboonConversions
           |{
           |    private Dictionary<ConversionKey, dynamic> convs = new ();
           |
           |    public BaboonConversions()
           |    {
           |        ${regs.join("\n").shift(8)}
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

  private def makeConvs(transd: CSDefnTranslator.CSDefnTranslatorImpl,
                        trans: CSTypeTranslator,
                        pkg: CSPackageId,
                        srcVer: Version,
                        domain: Domain,
                        rules: BaboonRuleset): Out[List[RenderedConversion]] = {
    rules.conversions.map { conv =>
      val convname = (Seq("Convert") ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "From",
        srcVer.version.replace(".", "_")
      )).mkString("__")

      val fname =
        (Seq("from", srcVer.version) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
          s"${conv.sourceTpe.name.name}.cs"
        )).mkString("-")
      val tin = trans.toCsVal(conv.sourceTpe, srcVer).fullyQualified
      val tout =
        trans.toCsVal(conv.sourceTpe, domain.version)

      def transfer(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
        val cnew =
          trans.asCsRef(tpe, domain.version)
        val cold = trans.asCsRef(tpe, srcVer, fullyQualified = true)

        val conv =
          q"conversions.ConvertWithContext<C, ${cold}, ${cnew}>(context, ${ref})"
        tpe match {
          case TypeRef.Scalar(id) =>
            id match {
              case _: TypeId.Builtin =>
                q"((${cnew}) $ref)"
              case _ => conv
            }
          case _: TypeRef.Constructor =>
            conv
        }
      }

      def transferId(tpe: TypeId, ref: TextTree[CSValue]): TextTree[CSValue] = {
        transfer(TypeRef.Scalar(tpe), ref)
      }

      conv match {
        case _: Conversion.CustomConversionRequired =>
          val cdefn =
            q"""public abstract class ${convname} : AbstractConversion<${tin}, ${tout}>
               |{
               |    public abstract override ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from);
               |}""".stripMargin
          val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
          Right(
            List(
              RenderedConversion(
                fname,
                ctree,
                Some(q"// Register(new ${convname}()); ")
              )
            )
          )
        case _: Conversion.RemovedTypeNoConversion =>
          Right(List.empty)
        case _: Conversion.CopyEnumByName =>
          val cdefn =
            q"""public class ${convname} : AbstractConversion<${tin}, ${tout}>
               |{
               |    public override ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from) {
               |        if (Enum.TryParse(from.ToString(), out ${tout} parsed))
               |        {
               |            return parsed;
               |        }
               |        else
               |        {
               |            throw new ArgumentException($$"Bad input, this is a Baboon bug: {from}");
               |        }
               |    }
               |}""".stripMargin
          val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
          val regtree = q"Register(new ${convname}());"
          Right(List(RenderedConversion(fname, ctree, Some(regtree))))
        case c: Conversion.CopyAdtBranchByName =>
          val branches = c.oldDefn.members.map { oldId =>
            val oldFqid = trans.toCsVal(oldId, srcVer).fullyQualified

            q"""if (from is ${oldFqid} fromAs)
               |{
               |    return ${transferId(oldId, q"fromAs")};
               |}""".stripMargin
          }.toSeq ++ Seq(q"""{
               |    throw new ArgumentException($$"Bad input: {from}");
               |}""".stripMargin)

          val cdefn =
            q"""public class ${convname} : AbstractConversion<${tin}, ${tout}>
               |{
               |    public override ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from) {
               |${branches.join("\nelse\n").shift(8)}
               |    }
               |}""".stripMargin
          val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
          val regtree = q"Register(new ${convname}());"
          Right(List(RenderedConversion(fname, ctree, Some(regtree))))
        case c: Conversion.DtoConversion =>
          for {
            newDefn <- domain.defs.meta.nodes(c.sourceTpe) match {
              case DomainMember.User(_, defn: Typedef.Dto) =>
                Right(defn)
              case _ => Left(NonEmptyList(BaboonIssue.TranslationBug()))
            }
            opIndex = c.ops.map(op => (op.targetField, op)).toMap
            exprs <- newDefn.fields.map { f =>
              val op = opIndex(f)
              val ftNew = trans.asCsRef(op.targetField.tpe, domain.version)
              val ftNewInit =
                trans.asCsRef(op.targetField.tpe, domain.version, mut = true)
              val base = op.targetField.name.name.capitalize
              val initExpr = op match {
                case o: FieldOp.Transfer =>
                  val fieldRef =
                    q"_from.${base}()"

                  val recConv = transfer(o.targetField.tpe, fieldRef)

                  o.targetField.tpe match {
                    case s: TypeRef.Scalar =>
                      s.id match {
                        case _: TypeId.Builtin =>
                          Right(Seq(fieldRef))
                        case _: TypeId.User =>
                          Right(Seq(recConv))
                      }
                    case c: TypeRef.Constructor
                        if c.id == TypeId.Builtins.lst =>
                      Right(
                        Seq(
                          q"(from e in $fieldRef select ${transfer(c.args.head, q"e")}).ToImmutableList()"
                        )
                      )
                    case c: TypeRef.Constructor
                        if c.id == TypeId.Builtins.map =>
                      Right(
                        Seq(
                          q"(from e in $fieldRef select KeyValuePair.Create(${transfer(
                            c.args.head,
                            q"e.Key"
                          )}, ${transfer(c.args.last, q"e.Value")})).ToImmutableDictionary(v => v.Key, v => v.Value)"
                        )
                      )
                    case c: TypeRef.Constructor
                        if c.id == TypeId.Builtins.set =>
                      Right(
                        Seq(
                          q"(from e in $fieldRef select ${transfer(c.args.head, q"e")}).ToImmutableHashSet()"
                        )
                      )
                    case c: TypeRef.Constructor
                        if c.id == TypeId.Builtins.opt =>
                      val tmp = q"_${base.toLowerCase}_tmp"

                      val recConv =
                        transfer(c.args.head, tmp)

                      Right(
                        Seq(
                          q"var $tmp = $fieldRef",
                          q"($tmp == null ? null : $recConv)"
                        )
                      )

                    case _ =>
                      Right(Seq(recConv))
                  }

                case o: FieldOp.InitializeWithDefault =>
                  o.targetField.tpe match {
                    case c: TypeRef.Constructor =>
                      c.id match {
                        case TypeId.Builtins.opt =>
                          Right(Seq(q"null"))
                        case TypeId.Builtins.set =>
                          // this is a safe assumption for now, we know there would be collections only
                          Right(Seq(q"(new $ftNewInit()).ToImmutableHashSet()"))
                        case TypeId.Builtins.lst =>
                          Right(Seq(q"(new $ftNewInit()).ToImmutableList()"))
                        case TypeId.Builtins.map =>
                          Right(
                            Seq(q"(new $ftNewInit()).ToImmutableDictionary()")
                          )

                        case _ =>
                          Left(NonEmptyList(BaboonIssue.TranslationBug()))
                      }
                    case _: TypeRef.Scalar =>
                      Left(NonEmptyList(BaboonIssue.TranslationBug()))
                  }

                case o: FieldOp.WrapIntoCollection =>
                  o.newTpe.id match {
                    case TypeId.Builtins.opt =>
                      Right(Seq(q"_from.${base}()"))
                    case TypeId.Builtins.set =>
                      Right(
                        Seq(
                          q"(new $ftNewInit { _from.${base}() }).ToImmutableHashSet()"
                        )
                      )
                    case TypeId.Builtins.lst =>
                      Right(
                        Seq(
                          q"(new $ftNewInit { _from.${base}() }).ToImmutableList()"
                        )
                      )
                    case _ =>
                      Left(NonEmptyList(BaboonIssue.TranslationBug()))
                  }

                case o: FieldOp.SwapCollectionType =>
                  o.oldTpe.id match {
                    case TypeId.Builtins.opt =>
                      val fieldRef =
                        q"_from.${base}()"
                      val tmp = q"_${base.toLowerCase}_tmp"

                      val recConv =
                        transfer(o.newTpe.args.head, tmp)

                      o.newTpe.id match {
                        case TypeId.Builtins.lst =>
                          Right(
                            Seq(
                              q"var $tmp = $fieldRef",
                              q"( ($tmp != null) ? new ${ftNewInit} { $recConv } : new ${ftNewInit}() ).ToImmutableList()"
                            )
                          )
                        case TypeId.Builtins.set =>
                          Right(
                            Seq(
                              q"var $tmp = $fieldRef",
                              q"( ($tmp != null) ? new ${ftNewInit} { $recConv } : new ${ftNewInit}() ).ToImmutableHashSet()"
                            )
                          )
                        case _ =>
                          Left(NonEmptyList(BaboonIssue.TranslationBug()))
                      }
                    case TypeId.Builtins.lst =>
                      o.newTpe.id match {
                        case TypeId.Builtins.set =>
                          Right(
                            Seq(
                              q"(new ${ftNewInit}(_from.${base}())).ToImmutableHashSet()"
                            )
                          )
                        case _ =>
                          Left(NonEmptyList(BaboonIssue.TranslationBug()))
                      }
                    case TypeId.Builtins.set =>
                      o.newTpe.id match {
                        case TypeId.Builtins.lst =>
                          Right(
                            Seq(
                              q"(new ${ftNewInit}(_from.${base}())).ToImmutableList()"
                            )
                          )
                        case _ =>
                          Left(NonEmptyList(BaboonIssue.TranslationBug()))
                      }
                    case _ =>
                      Left(NonEmptyList(BaboonIssue.TranslationBug()))
                  }

              }

              for {
                init <- initExpr
              } yield {
                val localName = q"_${base.toLowerCase}"
                val actualExpr = init.last;
                val assignment = q"${ftNew} ${localName} = $actualExpr"
                val full = (init.init ++ Seq(assignment)).join(";\n")
                (full, localName)
              }
            }.biAggregate
          } yield {
            val initExprs = exprs.map(_._1) ++ Seq(q"")
            val consExprs = exprs.map(_._2)

            val cdefn =
              q"""public class ${convname} : AbstractConversion<${tin}, ${tout}>
                 |{
                 |    public override ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} _from) {
                 |${initExprs.join(";\n").shift(8)}
                 |        return new ${tout}(
                 |${consExprs.join(",\n").shift(12)}
                 |        );
                 |    }
                 |}""".stripMargin

            val ctree = transd.inNs(pkg.parts.toSeq, cdefn)
            val regtree = q"Register(new ${convname}());"
            List(RenderedConversion(fname, ctree, Some(regtree)))
          }

      }
    }.biFlatAggregate

  }

}

object CSBaboonTranslator {
  case class RenderedConversion(fname: String,
                                conv: TextTree[CSValue],
                                reg: Option[TextTree[CSValue]])
}

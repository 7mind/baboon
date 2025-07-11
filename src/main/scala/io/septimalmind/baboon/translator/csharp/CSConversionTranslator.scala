package io.septimalmind.baboon.translator.csharp

import distage.Id
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object CSConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: CSPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      lineage: BaboonLineage,
    ): CSConversionTranslator[F]
  }
}

class CSConversionTranslator[F[+_, +_]: Error2](
  trans: CSTypeTranslator,
  pkg: CSPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: CSTreeTools,
  csTypeInfo: CSTypeInfo,
  lineage: BaboonLineage,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue.TranslationIssue], T]

  def makeConvs(): Out[List[RenderedConversion]] = {
    def makeName(prefix: String, conv: Conversion) = {
      (Seq(prefix) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "From",
        srcVer.version.replace(".", "_"),
      )).mkString("__")
    }

    val versionsMeta =
      q"""public override $csString VersionFrom() {
         |  return "${srcVer.version}";
         |}
         |
         |public override $csString VersionTo() {
         |    return "${domain.version.version}";
         |}""".stripMargin

//    import izumi.fundamentals.collections.IzCollections.*
    val (_, convsToTranslate) = rules.conversions.partition(c => csTypeInfo.canBeUpgradedTo(c.sourceTpe, srcDom.version, lineage).nonEmpty)

//    import izumi.fundamentals.platform.strings.IzString.*
//    println(
//      s"eliminated conversions ${srcDom.version}->${domain.version}: ${rules.conversions.map(c => (c.sourceTpe, csTypeInfo.canBeUpgradedTo(c.sourceTpe, srcDom.version, lineage))).niceList()}"
//    )
//    println(s"ignored: ${convsToIgnore.niceList()}")
//    println(convsToTranslate.niceList())

    F.flatTraverseAccumErrors(convsToTranslate) {
      conv =>
        val convname = makeName("Convert", conv)

        val fname =
          (Seq("from", srcVer.version) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
            s"${conv.sourceTpe.name.name}.cs"
          )).mkString("-")
        val tin = trans
          .asCsType(conv.sourceTpe, srcDom, lineage.evolution)
          .fullyQualified

        // This would fail if `sourceTpe` had been removed from `domain`. It's inconvenient to have this defined in each branch of the match below, so we use `def`
        def tout =
          trans.asCsType(conv.sourceTpe, domain, lineage.evolution)

        def transferId(tpe: TypeId.Scalar, ref: TextTree[CSValue]): TextTree[CSValue] = {
          transfer(TypeRef.Scalar(tpe), ref, 0)
        }

        val fullMeta =
          q"""|$versionsMeta
              |
              |public override $csString TypeId()
              |{
              |    return "${conv.sourceTpe.toString}";
              |}""".stripMargin.shift(4).trim

        conv match {
          case _: Conversion.CustomConversionRequired =>
            val cdefn =
              q"""public abstract class $convname : $abstractConversion<$tin, $tout>
                 |{
                 |    protected abstract override $tout DoConvert<C>(C? context, $abstractBaboonConversions conversions, $tin from) where C: default;
                 |
                 |    $fullMeta
                 |}""".stripMargin
            val ctree = tools.inNs(pkg.parts.toSeq, cdefn)

            val convMethodName = makeName("Conversion", conv)

            F.pure(
              List(
                RenderedConversion(
                  fname,
                  ctree,
                  Some(q"Register(requiredConversions.$convMethodName());"),
                  Some(
                    q"public $abstractConversion<$tin, $tout> $convMethodName();"
                  ),
                )
              )
            )
          case _: Conversion.RemovedTypeNoConversion =>
            F.pure(List.empty)
          case _: Conversion.NonDataTypeTypeNoConversion =>
            F.pure(List.empty)
          case _: Conversion.CopyEnumByName =>
            val cdefn =
              q"""public sealed class $convname : $abstractConversion<$tin, $tout>
                 |{
                 |    protected override $tout DoConvert<C>(C? context, $abstractBaboonConversions conversions, $tin from)  where C: default {
                 |        if ($csEnum.TryParse(from.ToString(), out $tout parsed))
                 |        {
                 |            return parsed;
                 |        }
                 |        throw new $csArgumentException($$"Bad input, this is a Baboon bug: {from}");
                 |    }
                 |
                 |    $fullMeta
                 |}""".stripMargin
            val ctree   = tools.inNs(pkg.parts.toSeq, cdefn)
            val regtree = q"Register(new $convname());"
            F.pure(List(RenderedConversion(fname, ctree, Some(regtree), None)))
          case c: Conversion.CopyAdtBranchByName =>
            val branches = c.oldDefn
              .dataMembers(srcDom)
              .map {
                oldId =>
                  val oldFqid =
                    trans.asCsType(oldId, srcDom, lineage.evolution).fullyQualified
                  val typedRef = q"fromAs_${oldId.name.name}"

                  q"""if (from is $oldFqid $typedRef)
                     |{
                     |    return ${transferId(oldId, typedRef)};
                     |}""".stripMargin
              }
              .toSeq ++ Seq(q"""{
                               |    throw new $csArgumentException($$"Bad input: {from}");
                               |}""".stripMargin)

            val cdefn =
              q"""public sealed class $convname : $abstractConversion<$tin, $tout>
                 |{
                 |    protected override $tout DoConvert<C>(C? context, $abstractBaboonConversions conversions, $tin from) where C: default {
                 |        ${branches.join("\n").shift(8).trim}
                 |    }
                 |
                 |    $fullMeta
                 |}""".stripMargin
            val ctree   = tools.inNs(pkg.parts.toSeq, cdefn)
            val regtree = q"Register(new $convname());"
            F.pure(List(RenderedConversion(fname, ctree, Some(regtree), None)))
          case c: Conversion.DtoConversion =>
            for {
              newDefn <- domain.defs.meta.nodes(c.sourceTpe) match {
                case DomainMember.User(_, defn: Typedef.Dto, _, _) =>
                  F.pure(defn)
                case _ => F.fail(NEList(BaboonIssue.TranslationBug()))
              }
              opIndex = c.ops.map(op => (op.targetField, op)).toMap
              exprs <- F.traverseAccumErrors(newDefn.fields) {
                f =>
                  val op = opIndex(f)
                  val ftNew =
                    trans.asCsRef(op.targetField.tpe, domain, lineage.evolution)
                  val ftNewInit =
                    trans.asCsRef(op.targetField.tpe, domain, lineage.evolution, mutableCollections = true)
                  val base     = op.targetField.name.name.capitalize
                  val fieldRef = q"_from.$base"
                  val initExpr = op match {
                    case o: FieldOp.Transfer =>
                      val recConv = transfer(o.targetField.tpe, fieldRef, 0)

                      o.targetField.tpe match {
                        case _: TypeRef.Scalar =>
                          F.pure(Seq(recConv))
                        case c: TypeRef.Constructor =>
                          F.pure(Seq(transfer(c, fieldRef, 0)))
                        case _ =>
                          F.pure(Seq(recConv))
                      }

                    case o: FieldOp.InitializeWithDefault =>
                      o.targetField.tpe match {
                        case c: TypeRef.Constructor =>
                          c.id match {
                            case TypeId.Builtins.opt =>
                              F.pure(Seq(q"null"))
                            case TypeId.Builtins.set =>
                              // this is a safe assumption for now, we know there would be collections only
                              F.pure(Seq(q"(new $ftNewInit()).${CSTypes.mkSet}"))
                            case TypeId.Builtins.lst =>
                              F.pure(Seq(q"(new $ftNewInit()).${CSTypes.mkList}"))
                            case TypeId.Builtins.map =>
                              F.pure(
                                Seq(q"(new $ftNewInit()).${CSTypes.mkDict}")
                              )

                            case _ =>
                              F.fail(NEList(BaboonIssue.TranslationBug()))
                          }
                        case _: TypeRef.Scalar =>
                          F.fail(NEList(BaboonIssue.TranslationBug()))
                      }

                    case o: FieldOp.WrapIntoCollection =>
                      o.newTpe.id match {
                        case TypeId.Builtins.opt =>
                          F.pure(Seq(fieldRef))
                        case TypeId.Builtins.set =>
                          F.pure(
                            Seq(
                              q"(new $ftNewInit { $fieldRef }).${CSTypes.mkSet}"
                            )
                          )
                        case TypeId.Builtins.lst =>
                          F.pure(
                            Seq(q"(new $ftNewInit { $fieldRef }).${CSTypes.mkList}")
                          )
                        case _ =>
                          F.fail(NEList(BaboonIssue.TranslationBug()))
                      }

                    case o: FieldOp.ExpandPrecision =>
                      (o.oldTpe, o.newTpe) match {
                        case (o: TypeRef.Constructor, n: TypeRef.Constructor) =>
                          swapCollType(
                            ftNewInit,
                            base,
                            fieldRef,
                            o.id,
                            n.id,
                            n.args,
                          )
                        case (_: TypeRef.Scalar, _: TypeRef.Scalar) =>
                          F.pure(Seq(fieldRef))
                        case _ =>
                          F.fail(NEList(BaboonIssue.TranslationBug()))
                      }

                    case o: FieldOp.SwapCollectionType =>
                      val oldId = o.oldTpe.id
                      val newId = o.newTpe.id
                      swapCollType(
                        ftNewInit,
                        base,
                        fieldRef,
                        oldId,
                        newId,
                        o.newTpe.args,
                      )

                  }

                  for {
                    init <- initExpr
                  } yield {
                    val localName  = q"_${base.toLowerCase}"
                    val actualExpr = init.last;
                    val assignment = q"$ftNew $localName = $actualExpr"
                    val full       = (init.init ++ Seq(assignment)).join(";\n")
                    (
                      full,
                      localName,
                    )
                  }
              }
            } yield {
              val initExprs = exprs.map(_._1) ++ Seq(q"")
              val consExprs = exprs.map(_._2)

              val cdefn =
                q"""public sealed class $convname : $abstractConversion<$tin, $tout>
                   |{
                   |    protected override $tout DoConvert<C>(C? context, $abstractBaboonConversions conversions, $tin _from) where C: default {
                   |        ${initExprs.join(";\n").shift(8).trim}
                   |        return new $tout(
                   |            ${consExprs.join(",\n").shift(12).trim}
                   |        );
                   |    }
                   |
                   |    $fullMeta
                   |}""".stripMargin

              val ctree   = tools.inNs(pkg.parts.toSeq, cdefn)
              val regtree = q"Register(new $convname());"
              List(RenderedConversion(fname, ctree, Some(regtree), None))
            }

        }
    }
  }

  private def transfer(tpe: TypeRef, ref: TextTree[CSValue], depth: Int, nullable: Boolean = false): TextTree[CSValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val cnew =
      trans.asCsRef(tpe, domain, lineage.evolution)

    val cold = trans.asCsRef(tpe, srcDom, lineage.evolution).fullyQualified

    val direct = if (nullable) {
      q"(($cnew?) $ref)"
    } else {
      q"(($cnew) $ref)"
    }
    val out = tpe match {
      case TypeRef.Scalar(id) =>
        val conv =
          q"conversions.ConvertWithContext<C, $cold, $cnew>(context, ($cold) $ref)"

        id match {
          case _: TypeId.Builtin =>
            direct
          case id: TypeId.User =>
            domain.defs.meta.nodes(id) match {
              case DomainMember.User(_, _: Typedef.Foreign, _, _) =>
                direct
              case _ =>
                conv
            }
        }
      case c: TypeRef.Constructor =>
        val tmp = q"e${depth.toString}"
        c match {
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
            q"(from $tmp in $ref select ${transfer(c.args.head, tmp, depth + 1)}).${CSTypes.mkList}"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
            val keyRef   = c.args.head
            val valueRef = c.args.last
            q"(from $tmp in $ref select new $csKeyValuePair<${trans
                .asCsRef(keyRef, domain, lineage.evolution)}, ${trans
                .asCsRef(valueRef, domain, lineage.evolution)}>(${transfer(
                c.args.head,
                q"$tmp.Key",
                depth + 1,
              )}, ${transfer(c.args.last, q"$tmp.Value", depth + 1)})).${CSTypes.mkDict}"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
            q"(from $tmp in $ref select ${transfer(c.args.head, tmp, depth + 1)}).${CSTypes.mkSet}"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
            val underlyingTpe = c.args.head
            val recConv =
              transfer(underlyingTpe, ref, depth + 1, nullable = true)

            q"($ref == null ? null : $recConv)"

          case _ =>
            ???
        }
    }
    out
//    q"/* ${srcDom.version.toString} -> ${domain.version.toString} ${cold.mapRender(_.toString)} -> ${cnew.mapRender(_.toString)} */ $out"
  }

  private def swapCollType(
    ftNewInit: TextTree[CSValue],
    base: String,
    fieldRef: TextTree[Nothing],
    oldId: TypeId.BuiltinCollection,
    newId: TypeId.BuiltinCollection,
    newCollArgs: NEList[TypeRef],
  ): F[NEList[BaboonIssue.TranslationBug], Seq[TextTree[CSValue]]] = {
    val collCsType = trans.asCsRef(newCollArgs.head, domain, lineage.evolution)

    val collInit =
      q"(new $ftNewInit(from e in $fieldRef select ($collCsType)e))"

    oldId match {
      case TypeId.Builtins.opt =>
        val tmp = q"_${base.toLowerCase}_tmp"

        val recConv =
          transfer(newCollArgs.head, tmp, 0)

        newId match {
          case TypeId.Builtins.lst =>
            F.pure(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).${CSTypes.mkList}",
              )
            )
          case TypeId.Builtins.set =>
            F.pure(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).${CSTypes.mkSet}",
              )
            )
          case TypeId.Builtins.opt =>
            F.pure(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? $recConv : null )",
              )
            )
          case _ =>
            F.fail(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.lst =>
        newId match {
          case TypeId.Builtins.set =>
            F.pure(Seq(q"$collInit.${CSTypes.mkSet}"))

          case TypeId.Builtins.lst =>
            F.pure(Seq(q"$collInit.${CSTypes.mkList}"))
          case _ =>
            F.fail(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.set =>
        newId match {
          case TypeId.Builtins.set =>
            F.pure(Seq(q"$collInit.${CSTypes.mkSet}"))
          case TypeId.Builtins.lst =>
            F.pure(Seq(q"$collInit.${CSTypes.mkList}"))
          case _ =>
            F.fail(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.map =>
        newId match {
          case TypeId.Builtins.map =>
            val kt = trans.asCsRef(newCollArgs.head, domain, lineage.evolution)
            val vt = trans.asCsRef(newCollArgs.last, domain, lineage.evolution)
            F.pure(
              Seq(
                q"(from e in $fieldRef select new $csKeyValuePair<$kt, $vt>(($kt)e.Key, ($vt)e.Value)).${CSTypes.mkDict}"
              )
            )
          case _ =>
            F.fail(NEList(BaboonIssue.TranslationBug()))
        }
      case _ =>
        F.fail(NEList(BaboonIssue.TranslationBug()))
    }
  }

}

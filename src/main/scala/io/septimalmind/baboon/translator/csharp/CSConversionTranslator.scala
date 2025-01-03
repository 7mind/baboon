package io.septimalmind.baboon.translator.csharp

import distage.Id
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSConversionTranslator(
  trans: CSTypeTranslator,
  pkg: CSPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: CSTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = Either[NEList[BaboonIssue.TranslationIssue], T]

  private def transfer(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
    val cnew =
      trans.asCsRef(tpe, domain, evo)
    val cold = trans.asCsRef(tpe, srcDom, evo, fullyQualified = true)

    val conv =
      q"conversions.ConvertWithContext<C, $cold, $cnew>(context, $ref)"
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case _: TypeId.Builtin =>
            q"(($cnew) $ref)"
          case _ => conv
        }
      case _: TypeRef.Constructor =>
        conv
    }
  }

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

    rules.conversions.map {
      conv =>
        val convname = makeName("Convert", conv)

        val fname =
          (Seq("from", srcVer.version) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
            s"${conv.sourceTpe.name.name}.cs"
          )).mkString("-")
        val tin = trans
          .toCsTypeRefDeref(conv.sourceTpe, srcDom, evo)
          .fullyQualified

        // This would fail if `sourceTpe` had been removed from `domain`. It's inconvenient to have this defined in each branch of the match below, so we use `def`
        def tout =
          trans.toCsTypeRefDeref(conv.sourceTpe, domain, evo)

        def transferId(tpe: TypeId.Scalar, ref: TextTree[CSValue]): TextTree[CSValue] = {
          transfer(TypeRef.Scalar(tpe), ref)
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

            Right(
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
            Right(List.empty)
          case _: Conversion.NonDataTypeTypeNoConversion =>
            Right(List.empty)
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
            Right(List(RenderedConversion(fname, ctree, Some(regtree), None)))
          case c: Conversion.CopyAdtBranchByName =>
            val branches = c.oldDefn
              .dataMembers(srcDom)
              .map {
                oldId =>
                  val oldFqid =
                    trans.toCsTypeRefDeref(oldId, srcDom, evo).fullyQualified
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
            Right(List(RenderedConversion(fname, ctree, Some(regtree), None)))
          case c: Conversion.DtoConversion =>
            for {
              newDefn <- domain.defs.meta.nodes(c.sourceTpe) match {
                case DomainMember.User(_, defn: Typedef.Dto, _) =>
                  Right(defn)
                case _ => Left(NEList(BaboonIssue.TranslationBug()))
              }
              opIndex = c.ops.map(op => (op.targetField, op)).toMap
              exprs <- newDefn.fields.map {
                f =>
                  val op = opIndex(f)
                  val ftNew =
                    trans.asCsRef(op.targetField.tpe, domain, evo)
                  val ftNewInit =
                    trans.asCsRef(op.targetField.tpe, domain, evo, mut = true)
                  val base     = op.targetField.name.name.capitalize
                  val fieldRef = q"_from.$base"
                  val initExpr = op match {
                    case o: FieldOp.Transfer =>
                      val recConv = transfer(o.targetField.tpe, fieldRef)

                      o.targetField.tpe match {
                        case s: TypeRef.Scalar =>
                          s.id match {
                            case _: TypeId.Builtin =>
                              Right(Seq(fieldRef))
                            case _: TypeId.User =>
                              Right(Seq(recConv))
                          }
                        case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
                          Right(
                            Seq(
                              q"(from e in $fieldRef select ${transfer(c.args.head, q"e")}).ToImmutableList()"
                            )
                          )
                        case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
                          val keyRef   = c.args.head
                          val valueRef = c.args.last
                          Right(
                            Seq(
                              q"(from e in $fieldRef select new $csKeyValuePair<${trans
                                  .asCsRef(keyRef, domain, evo)}, ${trans
                                  .asCsRef(valueRef, domain, evo)}>(${transfer(
                                  c.args.head,
                                  q"e.Key",
                                )}, ${transfer(c.args.last, q"e.Value")})).ToImmutableDictionary(v => v.Key, v => v.Value)"
                            )
                          )
                        case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
                          Right(
                            Seq(
                              q"(from e in $fieldRef select ${transfer(c.args.head, q"e")}).ToImmutableHashSet()"
                            )
                          )
                        case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
                          val tmp = q"_${base.toLowerCase}_tmp"

                          val recConv =
                            transfer(c.args.head, tmp)

                          Right(
                            Seq(
                              q"var $tmp = $fieldRef",
                              q"($tmp == null ? null : $recConv)",
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
                              Left(NEList(BaboonIssue.TranslationBug()))
                          }
                        case _: TypeRef.Scalar =>
                          Left(NEList(BaboonIssue.TranslationBug()))
                      }

                    case o: FieldOp.WrapIntoCollection =>
                      o.newTpe.id match {
                        case TypeId.Builtins.opt =>
                          Right(Seq(fieldRef))
                        case TypeId.Builtins.set =>
                          Right(
                            Seq(
                              q"(new $ftNewInit { $fieldRef }).ToImmutableHashSet()"
                            )
                          )
                        case TypeId.Builtins.lst =>
                          Right(
                            Seq(q"(new $ftNewInit { $fieldRef }).ToImmutableList()")
                          )
                        case _ =>
                          Left(NEList(BaboonIssue.TranslationBug()))
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
                          Right(Seq(fieldRef))
                        case _ =>
                          Left(NEList(BaboonIssue.TranslationBug()))
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
                    (full, localName)
                  }
              }.biSequence
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
    }.biFlatten

  }

  private def swapCollType(
    ftNewInit: TextTree[CSValue],
    base: String,
    fieldRef: TextTree[Nothing],
    oldId: TypeId.BuiltinCollection,
    newId: TypeId.BuiltinCollection,
    newCollArgs: NEList[TypeRef],
  ): Either[NEList[BaboonIssue.TranslationBug], Seq[TextTree[CSValue]]] = {
    val collCsType = trans.asCsRef(newCollArgs.head, domain, evo)

    val collInit =
      q"(new $ftNewInit(from e in $fieldRef select ($collCsType)e))"

    oldId match {
      case TypeId.Builtins.opt =>
        val tmp = q"_${base.toLowerCase}_tmp"

        val recConv =
          transfer(newCollArgs.head, tmp)

        newId match {
          case TypeId.Builtins.lst =>
            Right(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).ToImmutableList()",
              )
            )
          case TypeId.Builtins.set =>
            Right(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).ToImmutableHashSet()",
              )
            )
          case TypeId.Builtins.opt =>
            Right(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? $recConv : null )",
              )
            )
          case _ =>
            Left(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.lst =>
        newId match {
          case TypeId.Builtins.set =>
            Right(Seq(q"$collInit.ToImmutableHashSet()"))

          case TypeId.Builtins.lst =>
            Right(Seq(q"$collInit.ToImmutableList()"))
          case _ =>
            Left(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.set =>
        newId match {
          case TypeId.Builtins.set =>
            Right(Seq(q"$collInit.ToImmutableHashSet()"))
          case TypeId.Builtins.lst =>
            Right(Seq(q"$collInit.ToImmutableList()"))
          case _ =>
            Left(NEList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.map =>
        newId match {
          case TypeId.Builtins.map =>
            val kt = trans.asCsRef(newCollArgs.head, domain, evo)
            val vt = trans.asCsRef(newCollArgs.last, domain, evo)
            Right(
              Seq(
                q"(from e in $fieldRef select new $csKeyValuePair<$kt, $vt>(($kt)e.Key, ($vt)e.Value)).ToImmutableDictionary(v => v.Key, v => v.Value)"
              )
            )
          case _ =>
            Left(NEList(BaboonIssue.TranslationBug()))
        }
      case _ =>
        Left(NEList(BaboonIssue.TranslationBug()))
    }
  }

}

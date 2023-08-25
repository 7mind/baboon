package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.csharp.CSValue.CSPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

class IndividualConversionHandler(transd: CSDefnTranslator.CSDefnTranslatorImpl,
                                  trans: CSTypeTranslator,
                                  pkg: CSPackageId,
                                  srcVer: Version,
                                  domain: Domain,
                                  rules: BaboonRuleset) {
  type Out[T] = Either[NonEmptyList[BaboonIssue.TranslationIssue], T]

  private def transfer(tpe: TypeRef,
                       ref: TextTree[CSValue]): TextTree[CSValue] = {
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

  def makeConvs(): Out[List[RenderedConversion]] = {
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

      def transferId(tpe: TypeId.Scalar,
                     ref: TextTree[CSValue]): TextTree[CSValue] = {
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
            val typedRef = q"fromAs_${oldId.name.name}"

            q"""if (from is ${oldFqid} $typedRef)
               |{
               |    return ${transferId(oldId, typedRef)};
               |}""".stripMargin
          }.toSeq ++ Seq(q"""{
                                                     |    throw new ArgumentException($$"Bad input: {from}");
                                                     |}""".stripMargin)

          val cdefn =
            q"""public class ${convname} : AbstractConversion<${tin}, ${tout}>
               |{
               |    public override ${tout} Convert<C>(C context, BaboonConversions conversions, ${tin} from) {
               |        ${branches.join("\nelse\n").shift(8).trim}
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
              val fieldRef = q"_from.${base}"
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
                      Left(NonEmptyList(BaboonIssue.TranslationBug()))
                  }

                case o: FieldOp.ExpandPrecision =>
                  (o.oldTpe, o.newTpe) match {
                    case (o: TypeRef.Constructor, n: TypeRef.Constructor) =>
                      swapCollType(ftNewInit, base, fieldRef, n, o.id, n.id)
                    case (_: TypeRef.Scalar, _: TypeRef.Scalar) =>
                      Right(Seq(fieldRef))
                    case _ =>
                      Left(NonEmptyList(BaboonIssue.TranslationBug()))
                  }

                case o: FieldOp.SwapCollectionType =>
                  val oldId = o.oldTpe.id
                  val newId = o.newTpe.id
                  swapCollType(
                    ftNewInit,
                    base,
                    fieldRef,
                    o.newTpe,
                    oldId,
                    newId
                  )

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
                 |        ${initExprs.join(";\n").shift(8).trim}
                 |        return new ${tout}(
                 |            ${consExprs.join(",\n").shift(12).trim}
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

  private def swapCollType(ftNewInit: TextTree[CSValue],
                           base: String,
                           fieldRef: Node[Nothing],
                           newTpe: TypeRef.Constructor,
                           oldId: TypeId.BuiltinCollection,
                           newId: TypeId.BuiltinCollection): Either[
    NonEmptyList[BaboonIssue.TranslationBug],
    Seq[TextTree[CSValue]]
  ] = {
    val collType = newTpe.args.head
    val collCsType = trans.asCsRef(collType, domain.version)

    val collInit =
      q"(new ${ftNewInit}(from e in $fieldRef select ($collCsType)e))"
    oldId match {
      case TypeId.Builtins.opt =>
        val tmp = q"_${base.toLowerCase}_tmp"

        val recConv =
          transfer(collType, tmp)

        newId match {
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
          case TypeId.Builtins.opt =>
            Right(
              Seq(
                q"var $tmp = $fieldRef",
                q"( ($tmp != null) ? $recConv : null )"
              )
            )
          case _ =>
            Left(NonEmptyList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.lst =>
        newId match {
          case TypeId.Builtins.set =>
            Right(Seq(q"$collInit.ToImmutableHashSet()"))

          case TypeId.Builtins.lst =>
            Right(Seq(q"$collInit.ToImmutableList()"))
          case _ =>
            Left(NonEmptyList(BaboonIssue.TranslationBug()))
        }
      case TypeId.Builtins.set =>
        newId match {
          case TypeId.Builtins.set =>
            Right(Seq(q"$collInit.ToImmutableHashSet()"))
          case TypeId.Builtins.lst =>
            Right(Seq(q"$collInit.ToImmutableList()"))
          case _ =>
            Left(NonEmptyList(BaboonIssue.TranslationBug()))
        }
      case _ =>
        Left(NonEmptyList(BaboonIssue.TranslationBug()))
    }
  }

}
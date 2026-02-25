package io.septimalmind.baboon.translator.typescript

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object TsConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): TsConversionTranslator[F]
  }
}

case class TsRenderedConversion(
  fname: String,
  conv: TextTree[TsValue],
  reg: Option[TextTree[TsValue]],
  missing: Option[TextTree[TsValue]],
)

class TsConversionTranslator[F[+_, +_]: Error2](
  trans: TsTypeTranslator,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  evo: BaboonEvolution,
  tsFileTools: TsFileTools,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def hasUserType(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(_: TypeId.User) => true
      case TypeRef.Constructor(_, args)   => args.exists(hasUserType)
      case _                              => false
    }
  }

  private def jsonConvert(expr: TextTree[TsValue]): TextTree[TsValue] = {
    q"JSON.parse(JSON.stringify($expr))"
  }

  def makeConvs: Out[List[TsRenderedConversion]] = {
    def makeName(prefix: String, conv: Conversion): String =
      (Seq(prefix) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "from",
        srcVer.v.toString.replace('.', '_'),
      )).mkString("__")

    val targetedConversions = rules.conversions.collect { case tc: TargetedConversion => tc }

    F.flatTraverseAccumErrors(targetedConversions) {
      conv =>
        val fnName = trans.camelToKebab(makeName("convert", conv)).replace('-', '_')
        val fname = (Seq("from", srcVer.v.toString.replace('.', '_')) ++ conv.sourceTpe.owner.asPseudoPkg.map(_.toLowerCase) ++ Seq(
          s"${trans.camelToKebab(conv.sourceTpe.name.name)}.ts"
        )).mkString("_")

        val tout = trans.asTsTypeKeepForeigns(conv.targetTpe, domain, evo, tsFileTools.definitionsBasePkg)
        val tin = trans
          .asTsTypeKeepForeigns(conv.sourceTpe, srcDom, evo, tsFileTools.definitionsBasePkg)
          .withAlias(s"${conv.sourceTpe.name.name}_${srcDom.version.format(prefix = "_", delimiter = "_")}")

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            List(
              TsRenderedConversion(
                fname,
                q"""// Custom conversion required: $tin -> $tout
                   |// Implement this function manually:
                   |// export function $fnName(from: $tin): $tout { throw new Error("Not implemented"); }""".stripMargin,
                None,
                Some(q"export function $fnName(from: $tin): $tout;"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q""""$fromName": return "$toName" as $tout;"""
            }
            val mappedExpr = if (mappingEntries.isEmpty) {
              q"return from as unknown as $tout;"
            } else {
              q"""switch (from) {
                 |    ${mappingEntries.toList.joinN().shift(4).trim}
                 |    default: return from as unknown as $tout;
                 |}""".stripMargin
            }

            List(
              TsRenderedConversion(
                fname,
                q"""export function $fnName(from: $tin): $tout {
                   |    $mappedExpr
                   |}""".stripMargin,
                Some(q"$fnName"),
                None,
              )
            )

          case c: Conversion.CopyAdtBranchByName =>
            val adtType = trans.asTsTypeKeepForeigns(conv.targetTpe, domain, evo, tsFileTools.definitionsBasePkg)
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldTpe = trans
                  .asTsTypeKeepForeigns(oldId, srcDom, evo, tsFileTools.definitionsBasePkg)
                  .withAlias(s"${oldId.name.name}_${srcDom.version.format(prefix = "_", delimiter = "_")}")
                val newId         = c.branchMapping.getOrElse(oldId.name.name, oldId)
                val newBranchType = trans.asTsTypeKeepForeigns(newId, domain, evo, tsFileTools.definitionsBasePkg)
                val factoryFn     = TsValue.TsType(adtType.moduleId, s"${adtType.name}_${newId.name.name}")
                q"""if (from instanceof $oldTpe) {
                   |    return JSON.parse(JSON.stringify(from)) as $newBranchType
                   |}""".stripMargin
            }
            List(
              TsRenderedConversion(
                fname,
                q"""export function $fnName(from: $tin): $tout {
                   |    ${cases.toList.joinN().shift(4).trim}
                   |
                   |    throw new Error("Unknown ADT branch: " + from);
                   |}""".stripMargin,
                Some(q"$fnName"),
                None,
              )
            )

          case c: Conversion.DtoConversion =>
            val defnTypeId = c.targetTpe
            val dto = domain.defs.meta.nodes(defnTypeId) match {
              case DomainMember.User(_, d: Typedef.Dto, _, _) => d
              case _                                          => throw new IllegalStateException("DTO expected")
            }
            val ops = c.ops.map(o => o.targetField -> o).toMap
            val assigns = dto.fields.map {
              f =>
                val op  = ops(f)
                val fld = f.name.name
                op match {
                  case _: FieldOp.Transfer =>
                    if (hasUserType(f.tpe)) {
                      jsonConvert(q"from.$fld")
                    } else {
                      q"from.$fld"
                    }
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.lst => q"[]"
                          case TypeId.Builtins.set => q"new Set()"
                          case TypeId.Builtins.map => q"new Map()"
                          case TypeId.Builtins.opt => q"undefined"
                          case _                   => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }
                  case _: FieldOp.WrapIntoCollection =>
                    val innerExpr = if (hasUserType(f.tpe)) jsonConvert(q"from.$fld") else q"from.$fld"
                    f.tpe match {
                      case TypeRef.Constructor(TypeId.Builtins.opt, _) => q"$innerExpr"
                      case TypeRef.Constructor(TypeId.Builtins.set, _) => q"new Set([$innerExpr])"
                      case TypeRef.Constructor(TypeId.Builtins.lst, _) => q"[$innerExpr]"
                      case _                                           => q"[$innerExpr]"
                    }
                  case _: FieldOp.ExpandPrecision =>
                    jsonConvert(q"from.$fld")
                  case _: FieldOp.SwapCollectionType =>
                    jsonConvert(q"from.$fld")
                  case o: FieldOp.Rename =>
                    val srcFld = o.sourceFieldName.name
                    if (hasUserType(f.tpe)) {
                      jsonConvert(q"from.$srcFld")
                    } else {
                      q"from.$srcFld"
                    }
                  case o: FieldOp.Redef =>
                    val srcFld = o.sourceFieldName.name
                    jsonConvert(q"from.$srcFld")
                }
            }

            List(
              TsRenderedConversion(
                fname,
                q"""export function $fnName(from: $tin): $tout {
                   |    return new $tout (
                   |        ${assigns.joinN().shift(8).trim}
                   |    )
                   |}""".stripMargin,
                Some(q"$fnName"),
                None,
              )
            )
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }
}

package io.septimalmind.baboon.translator.rust

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase
import io.septimalmind.baboon.translator.rust.RsValue.RsCrateId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object RsConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      crate: RsCrateId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): RsConversionTranslator[F]
  }
}

case class RsRenderedConversion(
  fname: String,
  conv: TextTree[RsValue],
  reg: Option[TextTree[RsValue]],
  missing: Option[TextTree[RsValue]],
)

class RsConversionTranslator[F[+_, +_]: Error2](
  trans: RsTypeTranslator,
  crate: RsCrateId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def hasUserType(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(_: TypeId.User)  => true
      case TypeRef.Constructor(_, args)     => args.exists(hasUserType)
      case _                               => false
    }
  }

  private def serdeConvert(expr: TextTree[RsValue]): TextTree[RsValue] = {
    q"serde_json::from_value(serde_json::to_value(&$expr).unwrap()).unwrap()"
  }

  def makeConvs: Out[List[RsRenderedConversion]] = {
    def makeName(prefix: String, conv: Conversion): String =
      (Seq(prefix) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "from",
        srcVer.v.toString.replace('.', '_'),
      )).mkString("__")

    val targetedConversions = rules.conversions.collect { case tc: TargetedConversion => tc }

    F.flatTraverseAccumErrors(targetedConversions) {
      conv =>
        val fnName = toSnakeCase(makeName("convert", conv))
        val fname = (Seq("from", srcVer.v.toString.replace('.', '_')) ++ conv.sourceTpe.owner.asPseudoPkg.map(_.toLowerCase) ++ Seq(
          s"${toSnakeCase(conv.sourceTpe.name.name)}.rs"
        )).mkString("_")

        val tin  = trans.asRsType(conv.sourceTpe, srcDom, evo).fullyQualified
        val tout = trans.asRsType(conv.targetTpe, domain, evo).fullyQualified

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            List(
              RsRenderedConversion(
                fname,
                q"""// Custom conversion required: $tin -> $tout
                   |// Implement this function manually:
                   |// pub fn $fnName(from: &$tin) -> $tout { todo!() }""".stripMargin,
                None,
                Some(q"pub fn $fnName(from: &$tin) -> $tout;"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q""""$fromName" => "$tout::$toName".parse().expect("enum parse"),"""
            }
            val mappedExpr = if (mappingEntries.isEmpty) {
              q"""from.to_string().parse().expect("enum parse")"""
            } else {
              q"""match from.to_string().as_str() {
                 |    ${mappingEntries.toList.joinN().shift(4).trim}
                 |    other => other.parse().expect("enum parse"),
                 |}""".stripMargin
            }

            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName(from: &$tin) -> $tout {
                   |    $mappedExpr
                   |}""".stripMargin,
                Some(q"$fnName"),
                None,
              )
            )

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map { oldId =>
              val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
              q"""$tin::${oldId.name.name.capitalize}(x) => $tout::${newId.name.name.capitalize}(serde_json::from_value(serde_json::to_value(x).unwrap()).unwrap()),"""
            }
            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName(from: &$tin) -> $tout {
                   |    match from {
                   |        ${cases.toList.joinN().shift(8).trim}
                   |    }
                   |}""".stripMargin,
                Some(q"$fnName"),
                None,
              )
            )

          case c: Conversion.DtoConversion =>
            val defnTypeId = c.targetTpe
            val dto = domain.defs.meta.nodes(defnTypeId) match {
              case DomainMember.User(_, d: Typedef.Dto, _, _) => d
              case _ => throw new IllegalStateException("DTO expected")
            }
            val ops = c.ops.map(o => o.targetField -> o).toMap
            val assigns = dto.fields.map { f =>
              val op  = ops(f)
              val fld = toSnakeCase(f.name.name)
              val expr = op match {
                case _: FieldOp.Transfer =>
                  if (hasUserType(f.tpe)) {
                    serdeConvert(q"from.$fld")
                  } else {
                    q"from.$fld.clone()"
                  }
                case o: FieldOp.InitializeWithDefault =>
                  o.targetField.tpe match {
                    case TypeRef.Constructor(id, _) =>
                      id match {
                        case TypeId.Builtins.lst => q"Vec::new()"
                        case TypeId.Builtins.set => q"std::collections::BTreeSet::new()"
                        case TypeId.Builtins.map => q"std::collections::BTreeMap::new()"
                        case TypeId.Builtins.opt => q"None"
                        case _ => throw new IllegalStateException(s"Unsupported constructor type: $id")
                      }
                    case _ => throw new IllegalStateException("Unsupported target field type")
                  }
                case _: FieldOp.WrapIntoCollection =>
                  val innerExpr = if (hasUserType(f.tpe)) serdeConvert(q"from.$fld") else q"from.$fld.clone()"
                  f.tpe match {
                    case TypeRef.Constructor(TypeId.Builtins.opt, _) => q"Some($innerExpr)"
                    case TypeRef.Constructor(TypeId.Builtins.set, _) => q"std::collections::BTreeSet::from([$innerExpr])"
                    case TypeRef.Constructor(TypeId.Builtins.lst, _) => q"vec![$innerExpr]"
                    case _ => q"vec![$innerExpr]"
                  }
                case _: FieldOp.ExpandPrecision =>
                  serdeConvert(q"from.$fld")
                case _: FieldOp.SwapCollectionType =>
                  serdeConvert(q"from.$fld")
                case o: FieldOp.Rename =>
                  val srcFld = toSnakeCase(o.sourceFieldName.name)
                  if (hasUserType(f.tpe)) {
                    serdeConvert(q"from.$srcFld")
                  } else {
                    q"from.$srcFld.clone()"
                  }
                case o: FieldOp.Redef =>
                  val srcFld = toSnakeCase(o.sourceFieldName.name)
                  serdeConvert(q"from.$srcFld")
              }
              q"$fld: $expr,"
            }

            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName(from: &$tin) -> $tout {
                   |    $tout {
                   |        ${assigns.joinN().shift(8).trim}
                   |    }
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

package io.septimalmind.baboon.translator.typescript

import distage.Id
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.typescript.TsTypes.tsBaboonDecimal
import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
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
  exportedName: Option[String],
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

  private def conversionName(source: TypeId.User): String = {
    val parts = Seq("convert") ++ source.owner.asPseudoPkg ++ Seq(source.name.name, "from", srcVer.v.toString.replace('.', '_'))
    trans.camelToKebab(parts.mkString("__")).replace('-', '_')
  }

  private def conversionFile(source: TypeId.User): String = {
    (Seq("from", srcVer.v.toString.replace('.', '_')) ++ source.owner.asPseudoPkg.map(_.toLowerCase) ++ Seq(
      s"${trans.camelToKebab(source.name.name)}.ts"
    )).mkString("_")
  }

  private def conversionRef(source: TypeId.User, target: TypeId.User): TsType = {
    require(
      rules.conversions.exists {
        case c: TargetedConversion => c.sourceTpe == source && c.targetTpe == target
        case _                     => false
      },
      s"Missing typed conversion: $source -> $target",
    )
    val outputPath = s"${tsFileTools.basename(domain, evo)}/${conversionFile(source)}"
    TsType(TsModuleId(tsFileTools.definitionsBasePkg ++ outputPath.stripSuffix(".ts").split('/').toList), conversionName(source))
  }

  private def convertUser(source: TypeId.User, target: TypeId.User, value: TextTree[TsValue]): TextTree[TsValue] = {
    rules.conversions.find {
      case c: TargetedConversion => c.sourceTpe == source && c.targetTpe == target
      case _                     => false
    } match {
      case Some(_: Conversion.CustomConversionRequired) =>
        q"""(() => { throw new Error("Custom conversion required: ${source.name.name} -> ${target.name.name}"); })()"""
      case _ => q"${conversionRef(source, target)}($value)"
    }
  }

  private def emptyCollection(tpe: TypeRef.Constructor): TextTree[TsValue] = tpe.id match {
    case TypeId.Builtins.opt => q"undefined"
    case TypeId.Builtins.lst => q"[]"
    case TypeId.Builtins.set => q"new Set()"
    case TypeId.Builtins.map => if (trans.isStringKeyMap(tpe)) q"{}" else q"new Map()"
    case other               => throw new IllegalStateException(s"Unsupported collection default: $other")
  }

  private def transfer(oldTpe: TypeRef, newTpe: TypeRef, value: TextTree[TsValue]): TextTree[TsValue] = {
    if (oldTpe == newTpe && !hasUserType(newTpe)) return value
    (oldTpe, newTpe) match {
      case (old: TypeRef.Any, current: TypeRef.Any) if old == current => value
      case (TypeRef.Scalar(oldId: TypeId.User), TypeRef.Scalar(newId: TypeId.User)) =>
        domain.defs.meta.nodes(newId) match {
          case DomainMember.User(_, _: Typedef.Foreign, _, _) => value
          case _                                              => convertUser(oldId, newId, value)
        }
      case (TypeRef.Scalar(_: TypeId.BuiltinScalar), TypeRef.Scalar(newId: TypeId.BuiltinScalar)) =>
        newId match {
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"BigInt($value)"
          case TypeId.Builtins.f128                      => q"$tsBaboonDecimal.fromString(String($value))"
          case TypeId.Builtins.i08 | TypeId.Builtins.u08 | TypeId.Builtins.i16 | TypeId.Builtins.u16 | TypeId.Builtins.i32 | TypeId.Builtins.u32 | TypeId.Builtins.f32 |
              TypeId.Builtins.f64 =>
            q"Number($value)"
          case other => throw new IllegalStateException(s"Unsupported scalar conversion: $oldTpe -> $other")
        }
      case (old: TypeRef.Scalar, current: TypeRef.Constructor) =>
        val element = transfer(old, current.args.head, value)
        current.id match {
          case TypeId.Builtins.opt => element
          case TypeId.Builtins.lst => q"[$element]"
          case TypeId.Builtins.set => q"new Set([$element])"
          case other               => throw new IllegalStateException(s"Unsupported collection wrapping: $other")
        }
      case (old: TypeRef.Constructor, current: TypeRef.Constructor) if old.id == TypeId.Builtins.opt =>
        val element = transfer(old.args.head, current.args.head, value)
        val present = current.id match {
          case TypeId.Builtins.opt => element
          case TypeId.Builtins.lst => q"[$element]"
          case TypeId.Builtins.set => q"new Set([$element])"
          case other               => throw new IllegalStateException(s"Unsupported optional conversion: $other")
        }
        q"($value === undefined ? ${emptyCollection(current)} : $present)"
      case (old: TypeRef.Constructor, current: TypeRef.Constructor)
          if Set(TypeId.Builtins.lst, TypeId.Builtins.set).contains(old.id) && Set(TypeId.Builtins.lst, TypeId.Builtins.set).contains(current.id) =>
        val element = transfer(old.args.head, current.args.head, q"item")
        val mapped  = q"Array.from($value, item => $element)"
        if (current.id == TypeId.Builtins.set) q"new Set($mapped)" else mapped
      case (old: TypeRef.Constructor, current: TypeRef.Constructor) if old.id == TypeId.Builtins.map && current.id == TypeId.Builtins.map =>
        val entries    = if (trans.isStringKeyMap(old)) q"Object.entries($value)" else q"Array.from($value.entries())"
        val key        = transfer(old.args.head, current.args.head, q"key")
        val entryValue = transfer(old.args.last, current.args.last, q"entryValue")
        val mapped     = q"$entries.map(([key, entryValue]) => [$key, $entryValue] as const)"
        if (trans.isStringKeyMap(current)) q"Object.fromEntries($mapped)" else q"new Map($mapped)"
      case _ => throw new IllegalStateException(s"Unsupported typed conversion: $oldTpe -> $newTpe")
    }
  }

  def makeConvs: Out[List[TsRenderedConversion]] = {
    val targetedConversions = rules.conversions.collect { case tc: TargetedConversion => tc }

    F.flatTraverseAccumErrors(targetedConversions) {
      conv =>
        val fnName = conversionName(conv.sourceTpe)
        val fname  = conversionFile(conv.sourceTpe)

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
                q"case $tin.${trans.enumMemberIdentifier(fromName)}: return $tout.${trans.enumMemberIdentifier(toName)};"
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
                Some(fnName),
                None,
              )
            )

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldTpe = trans
                  .asTsTypeKeepForeigns(oldId, srcDom, evo, tsFileTools.definitionsBasePkg)
                  .withAlias(s"${oldId.name.name}_${srcDom.version.format(prefix = "_", delimiter = "_")}")
                val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
                q"""if (from instanceof $oldTpe) {
                   |    return ${convertUser(oldId, newId, q"from")}
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
                Some(fnName),
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
                    transfer(f.tpe, f.tpe, q"from.$fld")
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case tpe: TypeRef.Constructor => emptyCollection(tpe)
                      // `any` fields require a meta header bound to the field's variant; there is
                      // no schema-agnostic default. Validator forbids adding new `any` fields without
                      // an explicit migration path, so this branch is best-effort fail-fast — if it
                      // fires, evolution rules let through a case they shouldn't have. Mirrors
                      // `CSConversionTranslator` (`F.fail(TranslationBug)`).
                      case _: TypeRef.Any => throw new IllegalStateException("any-typed field cannot be initialised with a default")
                      case _              => throw new IllegalStateException("Unsupported target field type")
                    }
                  case o: FieldOp.Modify =>
                    transfer(o.oldTpe, o.newTpe, q"from.$fld")
                  case o: FieldOp.Rename =>
                    val srcFld = o.sourceFieldName.name
                    transfer(f.tpe, f.tpe, q"from.$srcFld")
                  case o: FieldOp.Redef =>
                    val srcFld = o.sourceFieldName.name
                    transfer(o.modify.oldTpe, o.modify.newTpe, q"from.$srcFld")
                }
            }

            List(
              TsRenderedConversion(
                fname,
                q"""export function $fnName(from: $tin): $tout {
                   |    return new $tout (
                   |        ${assigns.join(",\n").shift(8).trim}
                   |    )
                   |}""".stripMargin,
                Some(fnName),
                None,
              )
            )
        }

        F.pure(rendered): Out[List[TsRenderedConversion]]
    }
  }
}

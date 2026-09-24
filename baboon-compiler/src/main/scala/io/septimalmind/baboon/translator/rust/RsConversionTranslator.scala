package io.septimalmind.baboon.translator.rust

import distage.Id
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{escapeRustModuleName, escapeRustTypeName, toSnakeCase, toSnakeCaseFileName}
import io.septimalmind.baboon.translator.rust.RsValue.RsCrateId
import io.septimalmind.baboon.typer.BaboonEnquiries
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
)

class RsConversionTranslator[F[+_, +_]: Error2](
  trans: RsTypeTranslator,
  crate: RsCrateId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  evo: BaboonEvolution,
  enquiries: BaboonEnquiries,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private val codecCtx = q"&crate::baboon_runtime::BaboonCodecContext::Compact"

  /** A pair of types with no targeted conversion is copied structurally, through JSON. A
    * generated type carries `encode_json`/`decode_json`; a foreign type carries neither —
    * Baboon does not know its shape — so it keeps the serde impl its host provides, and so do
    * the builtins.
    */
  private def structuralConvert(oldTpe: TypeRef, newTpe: TypeRef, ref: TextTree[RsValue]): TextTree[RsValue] = {
    (oldTpe, newTpe) match {
      case (TypeRef.Scalar(oldId), TypeRef.Scalar(newId)) if hasJsonCodec(oldId, srcDom) && hasJsonCodec(newId, domain) =>
        val target = trans.asRsRef(newTpe, domain, evo)
        q"$target::decode_json($codecCtx, &($ref).encode_json($codecCtx).unwrap()).unwrap()"
      case _ =>
        q"serde_json::from_value(serde_json::to_value(&$ref).unwrap()).unwrap()"
    }
  }

  private def hasJsonCodec(id: TypeId, dom: Domain): Boolean = id match {
    case u: TypeId.User =>
      dom.defs.meta.nodes.get(u) match {
        case Some(DomainMember.User(_, _: Typedef.Dto | _: Typedef.Enum | _: Typedef.Adt, _, _)) => true
        case _                                                                                   => false
      }
    case _ => false
  }

  private val sourceRepresentation = new RsFieldRepresentation(srcDom, evo, trans, enquiries)
  private val targetRepresentation = new RsFieldRepresentation(domain, evo, trans, enquiries)

  private def conversionName(conv: Conversion): String =
    toSnakeCase(
      (Seq("convert") ++ conv.sourceTpe.owner.asPseudoPkg.map(s => escapeRustModuleName(s.toLowerCase)) ++ Seq(
        conv.sourceTpe.name.name,
        "from",
        srcVer.v.toString.replace('.', '_'),
      )).mkString("__")
    )

  private def conversionFile(conv: Conversion): String =
    (Seq("from", srcVer.v.toString.replace('.', '_')) ++ conv.sourceTpe.owner.asPseudoPkg.map(s => escapeRustModuleName(s.toLowerCase)) ++ Seq(
      toSnakeCaseFileName(conv.sourceTpe.name.name)
    )).mkString("_")

  private def transferField(name: FieldName, oldTpe: TypeRef, newTpe: TypeRef): TextTree[RsValue] = {
    val field = toSnakeCase(name.name)
    val ref   = if (sourceRepresentation.needsBox(oldTpe)) q"from.$field.as_ref()" else q"&from.$field"
    val value = transfer(oldTpe, newTpe, ref)
    if (targetRepresentation.needsBox(newTpe)) q"Box::new($value)" else value
  }

  // References here borrow surface values; field-level boxes are handled by transferField.
  private def transfer(oldTpe: TypeRef, newTpe: TypeRef, ref: TextTree[RsValue]): TextTree[RsValue] = {
    (oldTpe, newTpe) match {
      case (TypeRef.Scalar(oldId: TypeId.User), TypeRef.Scalar(newId: TypeId.User)) =>
        rules.conversions.collectFirst {
          case c: TargetedConversion if c.sourceTpe == oldId && c.targetTpe == newId && !c.isInstanceOf[Conversion.CustomConversionRequired] =>
            q"${crate.parts.mkString("::")}::${conversionFile(c)}::${conversionName(c)}($ref)"
        }.getOrElse(structuralConvert(oldTpe, newTpe, ref))
      case (TypeRef.Scalar(oldId: TypeId.BuiltinScalar), TypeRef.Scalar(newId: TypeId.BuiltinScalar)) if oldId != newId =>
        val integers = Set(
          TypeId.Builtins.i08,
          TypeId.Builtins.i16,
          TypeId.Builtins.i32,
          TypeId.Builtins.i64,
          TypeId.Builtins.u08,
          TypeId.Builtins.u16,
          TypeId.Builtins.u32,
          TypeId.Builtins.u64,
        )
        if (integers.contains(oldId) && integers.contains(newId)) q"(*($ref)) as ${trans.asRsRef(newTpe, domain, evo)}"
        else structuralConvert(oldTpe, newTpe, ref)
      case (old: TypeRef.Scalar, TypeRef.Constructor(newId, args)) =>
        val inner = transfer(old, args.head, ref)
        newId match {
          case TypeId.Builtins.opt => q"Some($inner)"
          case TypeId.Builtins.lst => q"vec![$inner]"
          case TypeId.Builtins.set => q"std::collections::BTreeSet::from([$inner])"
          case _                   => structuralConvert(oldTpe, newTpe, ref)
        }
      case (TypeRef.Constructor(oldId, oldArgs), TypeRef.Constructor(newId, newArgs)) =>
        (oldId, newId) match {
          case (TypeId.Builtins.map, TypeId.Builtins.map) =>
            val key   = transfer(oldArgs.head, newArgs.head, q"k")
            val value = transfer(oldArgs.last, newArgs.last, q"v")
            q"($ref).iter().map(|(k, v)| ($key, $value)).collect()"
          case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
            val inner = transfer(oldArgs.head, newArgs.head, q"e")
            q"($ref).as_ref().map(|e| $inner)"
          case (_, TypeId.Builtins.lst | TypeId.Builtins.set) =>
            val inner = transfer(oldArgs.head, newArgs.head, q"e")
            q"($ref).iter().map(|e| $inner).collect()"
          case _ => structuralConvert(oldTpe, newTpe, ref)
        }
      case _ if oldTpe == newTpe => q"(*($ref)).clone()"
      case _                     => structuralConvert(oldTpe, newTpe, ref)
    }
  }

  def makeConvs: Out[List[RsRenderedConversion]] = {
    val targetedConversions = rules.conversions.collect { case tc: TargetedConversion => tc }

    F.flatTraverseAccumErrors(targetedConversions) {
      conv =>
        val fnName = conversionName(conv)
        val fname  = s"${conversionFile(conv)}.rs"

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
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q""""$fromName" => "$toName","""
            }
            val mappedExpr = if (mappingEntries.isEmpty) {
              q"""from.to_string().parse().expect("enum parse")"""
            } else {
              q"""{
                 |    let s = from.to_string();
                 |    let name: &str = match s.as_str() {
                 |        ${mappingEntries.toList.joinN().shift(8).trim}
                 |        other => other,
                 |    };
                 |    name.parse().expect("enum parse")
                 |}""".stripMargin
            }

            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName(from: &$tin) -> $tout {
                   |    $mappedExpr
                   |}""".stripMargin,
              )
            )

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val newId     = c.branchMapping.getOrElse(oldId.name.name, oldId)
                val converted = transfer(TypeRef.Scalar(oldId), TypeRef.Scalar(newId), q"x")
                q"""$tin::${escapeRustTypeName(oldId.name.name.capitalize)}(x) => $tout::${escapeRustTypeName(newId.name.name.capitalize)}($converted),"""
            }
            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName(from: &$tin) -> $tout {
                   |    match from {
                   |        ${cases.toList.joinN().shift(8).trim}
                   |    }
                   |}""".stripMargin,
              )
            )

          case c: Conversion.DtoConversion =>
            val defnTypeId = c.targetTpe
            val dto = domain.defs.meta.nodes(defnTypeId) match {
              case DomainMember.User(_, d: Typedef.Dto, _, _) => d
              case _                                          => throw new IllegalStateException("DTO expected")
            }
            val ops = c.ops.map(o => o.targetField -> o).toMap
            val usesFrom = dto.fields.exists {
              f =>
                ops(f) match {
                  case _: FieldOp.InitializeWithDefault => false
                  case _                                => true
                }
            }
            val fromParam = if (usesFrom) "from" else "_from"
            val assigns = dto.fields.map {
              f =>
                val op  = ops(f)
                val fld = toSnakeCase(f.name.name)
                val expr = op match {
                  case o: FieldOp.Transfer =>
                    transferField(f.name, o.sourceTpe, f.tpe)
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.lst => q"Vec::new()"
                          case TypeId.Builtins.set => q"std::collections::BTreeSet::new()"
                          case TypeId.Builtins.map => q"std::collections::BTreeMap::new()"
                          case TypeId.Builtins.opt => q"None"
                          case _                   => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      // `any` fields require a meta header bound to the field's variant; there is
                      // no schema-agnostic default value. The validator forbids adding new `any`
                      // fields without an explicit migration path, so this branch is best-effort
                      // fail-fast — if it fires, evolution rules let through a case they shouldn't.
                      // Matches sibling branches in this match (IllegalStateException). PR 3.2
                      // CSConversionTranslator went via F.fail(TranslationBug); the surrounding
                      // Rust code uses local-throw convention for unsupported defaults so we
                      // follow that convention here.
                      case _: TypeRef.Any =>
                        throw new IllegalStateException("BUG: `any` field has no schema-agnostic default; evolution rules should reject InitializeWithDefault on Any")
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }
                  case o: FieldOp.Modify =>
                    transferField(o.fieldName, o.oldTpe, o.newTpe)
                  case o: FieldOp.Rename =>
                    transferField(o.sourceFieldName, f.tpe, f.tpe)
                  case o: FieldOp.Redef =>
                    transferField(o.sourceFieldName, o.modify.oldTpe, o.modify.newTpe)
                }
                q"$fld: $expr,"
            }

            List(
              RsRenderedConversion(
                fname,
                q"""pub fn $fnName($fromParam: &$tin) -> $tout {
                   |    $tout {
                   |        ${assigns.joinN().shift(8).trim}
                   |    }
                   |}""".stripMargin,
              )
            )
        }

        F.pure(rendered): Out[List[RsRenderedConversion]]
    }
  }
}

package io.septimalmind.baboon.translator.dart

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.dart.DtBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.translator.dart.DtValue.DtPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object DtConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: DtPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): DtConversionTranslator[F]
  }
}

class DtConversionTranslator[F[+_, +_]: Error2](
  trans: DtTypeTranslator,
  pkg: DtPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: DtTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def transfer(newTpe: TypeRef, oldRef: TextTree[DtValue], depth: Int, maybeOldTpe: Option[TypeRef] = None): TextTree[DtValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = trans.asDtRef(newTpe, domain, evo)
    val oldTypeRefTree = trans.asDtRef(oldTpe, srcDom, evo).fullyQualified

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, depth + 1, Some(s))
        c.id match {
          case TypeId.Builtins.opt =>
            q"$headTransfer"
          case TypeId.Builtins.lst =>
            q"[$headTransfer]"
          case TypeId.Builtins.set =>
            q"{$headTransfer}"
          case _ =>
            throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
        }

      case (ns: TypeRef.Scalar, os: TypeRef.Scalar) =>
        transferScalar(oldRef, newTypeRefTree, oldTypeRefTree, ns, os)
      case (TypeRef.Scalar(_), c: TypeRef.Constructor) =>
        throw new IllegalStateException(s"Unsupported scalar to constructor conversion: ${c.id}")
      case (cn: TypeRef.Constructor, co: TypeRef.Constructor) =>
        transferConstructor(oldRef, depth, cn, co)
    }
  }

  private def transferConstructor(
    oldRef: TextTree[DtValue],
    depth: Int,
    cn: TypeRef.Constructor,
    co: TypeRef.Constructor,
  ): TextTree[DtValue] = {
    val tmp = q"e${depth.toString}"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"$oldRef.map(($tmp) => ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).toList()"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"$tmp.key"
        val vv       = q"$tmp.value"

        q"Map.fromEntries($oldRef.entries.map(($tmp) => MapEntry(${transfer(keyRef, kv, depth + 1, Some(co.args.head))}, ${transfer(valueRef, vv, depth + 1, Some(co.args.last))})))"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"$oldRef.map(($tmp) => ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).toSet()"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        val inner = transfer(c.args.head, tmp, depth + 1, Some(co.args.head))
        q"($oldRef != null) ? (() { final $tmp = $oldRef!; return $inner; })() : null"
      case c =>
        throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(
    oldRef: TextTree[DtValue],
    newTypeRefTree: TextTree[DtValue],
    oldTypeRefTree: TextTree[DtValue],
    s: TypeRef.Scalar,
    os: TypeRef.Scalar,
  ): TextTree[DtValue] = {
    val direct =
      if (s == os) oldRef
      else {
        builtinConversion(s.id, os.id) match {
          case Some(method) => q"$method($oldRef)"
          case None         => q"$oldRef as $newTypeRefTree"
        }
      }

    val conv =
      q"conversions.convertWithContext(context, $oldRef, '${os.id.toString}', '${s.id.toString}') as $newTypeRefTree"

    s.id match {
      case _: TypeId.Builtin => direct
      case id: TypeId.User =>
        domain.defs.meta.nodes(id) match {
          case DomainMember.User(_, _: Typedef.Foreign, _, _) => direct
          case _                                              => conv
        }
    }
  }

  private def builtinConversion(to: TypeId, from: TypeId): Option[String] = {
    (from, to) match {
      case _ if from == to                                                 => None
      case (_, TypeId.Builtins.i16 | TypeId.Builtins.i32 |
              TypeId.Builtins.i64 | TypeId.Builtins.u16 |
              TypeId.Builtins.u32 | TypeId.Builtins.u64)                   => None // int -> int in Dart, identity
      case (_, TypeId.Builtins.f64)                                        => Some("(x) => x.toDouble()")
      case (_, TypeId.Builtins.f128)                                       => Some("BaboonDecimal.fromString")
      case _                                                               => None
    }
  }

  def makeConvs: Out[List[RenderedConversion]] = {
    def makeName(prefix: String, conv: Conversion): String =
      (Seq(prefix) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "From",
        srcVer.v.toString.replace('.', '_'),
      )).mkString("__")

    val targetedConversions = rules.conversions.collect { case tc: TargetedConversion => tc }

    F.flatTraverseAccumErrors(targetedConversions) {
      conv =>
        val className = makeName("Convert", conv)
        val fname = (Seq("from", srcVer.v.toString) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
          s"${trans.toSnakeCase(conv.sourceTpe.name.name)}.dart"
        )).mkString("-")

        val tin  = trans.asDtType(conv.sourceTpe, srcDom, evo).fullyQualified
        def tout = trans.asDtType(conv.targetTpe, domain, evo)

        val meta = q"""@override String get versionFrom => '${srcVer.v.toString}';
                      |@override String get versionTo => '${domain.version.v.toString}';
                      |@override String get typeId => '${conv.sourceTpe.toString}';
                      """.stripMargin.trim

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""abstract class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    $tout doConvert(
                              |      dynamic context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from,
                              |    );
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim

            val convMethodName = makeName("conversion", conv)

            List(
              RenderedConversion(
                fname,
                tools.inLib(classDef),
                Some(q"""register(required.$convMethodName());"""),
                Some(q"$baboonAbstractConversion<$tin, $tout> $convMethodName();"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q"'$fromName': '$toName',"
            }.toList
            val mappedExpr =
              if (mappingEntries.isEmpty) {
                q"from.name"
              } else {
                q"""({
                   |  ${mappingEntries.joinN().shift(2).trim}
                   |}[from.name]) ?? from.name""".stripMargin
              }
            val classDef = q"""class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    static final $className instance = $className._();
                              |    ${className}._();
                              |    @override $tout doConvert(
                              |      dynamic context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from,
                              |    ) {
                              |      final mapped = $mappedExpr;
                              |      final result = $tout.parse(mapped);
                              |      if (result == null) throw ArgumentError('Cannot parse enum: $$from');
                              |      return result;
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.instance);"
            List(RenderedConversion(fname, tools.inLib(classDef), Some(regtree), None))

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT  = trans.asDtType(oldId, srcDom, evo).fullyQualified
                val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
                q"if (from is $oldT) { final x = from; return ${transfer(TypeRef.Scalar(newId), q"x", 1, Some(TypeRef.Scalar(oldId)))}; }"
            }

            val classDef = q"""class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    static final $className instance = $className._();
                              |    ${className}._();
                              |    @override $tout doConvert(
                              |      dynamic context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from,
                              |    ) {
                              |      ${cases.joinN().shift(6).trim}
                              |      throw ArgumentError('Bad input: $$from');
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.instance);"
            List(RenderedConversion(fname, tools.inLib(classDef), Some(regtree), None))

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
                val expr = op match {
                  case o: FieldOp.Transfer => transfer(o.targetField.tpe, q"from.$fld", 1)
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.lst => q"[]"
                          case TypeId.Builtins.set => q"{}"
                          case TypeId.Builtins.map => q"{}"
                          case TypeId.Builtins.opt => q"null"
                          case _ => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }

                  case _: FieldOp.WrapIntoCollection =>
                    f.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.opt => q"from.$fld"
                          case TypeId.Builtins.lst => q"[from.$fld]"
                          case TypeId.Builtins.set => q"{from.$fld}"
                          case _ => throw new IllegalStateException(s"Unsupported collection wrap type: $id")
                        }
                      case _ => throw new IllegalStateException("WrapIntoCollection target must be a constructor type")
                    }
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"from.$fld", 1, Some(o.oldTpe))
                  case o: FieldOp.SwapCollectionType  => swapCollType(q"from.$fld", o, 0)
                  case o: FieldOp.Rename             => transfer(o.targetField.tpe, q"from.${o.sourceFieldName.name}", 1)
                  case o: FieldOp.Redef =>
                    val srcFieldRef = q"from.${o.sourceFieldName.name}"
                    o.modify match {
                      case _: FieldOp.WrapIntoCollection =>
                        o.targetField.tpe match {
                          case TypeRef.Constructor(id, _) =>
                            id match {
                              case TypeId.Builtins.opt => q"$srcFieldRef"
                              case TypeId.Builtins.lst => q"[$srcFieldRef]"
                              case TypeId.Builtins.set => q"{$srcFieldRef}"
                              case _ => throw new IllegalStateException(s"Unsupported collection wrap type: $id")
                            }
                          case _ => throw new IllegalStateException("WrapIntoCollection target must be a constructor type")
                        }
                      case m: FieldOp.ExpandPrecision =>
                        transfer(m.newTpe, srcFieldRef, 1, Some(m.oldTpe))
                      case m: FieldOp.SwapCollectionType =>
                        swapCollType(srcFieldRef, m, 0)
                    }
                }
                q"final ${f.name.name.toLowerCase} = $expr;"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name}: ${f.name.name.toLowerCase}")
            val classDef = q"""class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    static final $className instance = $className._();
                              |    ${className}._();
                              |    @override $tout doConvert(
                              |      dynamic context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from,
                              |    ) {
                              |      ${assigns.joinN().shift(6).trim}
                              |      return $tout(
                              |        ${ctorArgs.join(",\n").shift(8).trim}
                              |      );
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.instance);"
            List(RenderedConversion(fname, tools.inLib(classDef), Some(regtree), None))
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }

  private def swapCollType(fieldRef: TextTree[DtValue], op: FieldOp.SwapCollectionType, depth: Int): TextTree[DtValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"e"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"""($fieldRef != null) ? [(() { final e = $fieldRef!; return ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}; })()] : <${trans.asDtRef(newArgs.head, domain, evo)}>[]"""
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"""($fieldRef != null) ? {(() { final e = $fieldRef!; return ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}; })()} : <${trans.asDtRef(newArgs.head, domain, evo)}>{}"""
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"""($fieldRef != null) ? (() { final e = $fieldRef!; return ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}; })() : null"""

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"""$fieldRef.map((e) => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList()"""
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"""$fieldRef.map((e) => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toSet()"""

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"""$fieldRef.map((e) => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList()"""
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"""$fieldRef.map((e) => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toSet()"""

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val kv = q"entry.key"
        val vv = q"entry.value"
        q"""Map.fromEntries($fieldRef.entries.map((entry) => MapEntry(${transfer(newArgs.head, kv, 1, Some(oldArgs.head))}, ${transfer(
            newArgs.last,
            vv,
            1,
            Some(oldArgs.last),
          )})))"""
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }
}

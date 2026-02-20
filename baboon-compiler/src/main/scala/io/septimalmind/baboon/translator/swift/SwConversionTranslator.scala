package io.septimalmind.baboon.translator.swift

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.swift.SwBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.translator.swift.SwValue.SwPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object SwConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: SwPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): SwConversionTranslator[F]
  }
}

class SwConversionTranslator[F[+_, +_]: Error2](
  trans: SwTypeTranslator,
  pkg: SwPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: SwTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def transfer(newTpe: TypeRef, oldRef: TextTree[SwValue], depth: Int, maybeOldTpe: Option[TypeRef] = None): TextTree[SwValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = trans.asSwRef(newTpe, domain, evo)
    val oldTypeRefTree = trans.asSwRef(oldTpe, srcDom, evo).fullyQualified

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, depth + 1, Some(s))
        c.id match {
          case TypeId.Builtins.opt =>
            q"$headTransfer"
          case TypeId.Builtins.lst =>
            q"[$headTransfer]"
          case TypeId.Builtins.set =>
            q"Set([$headTransfer])"
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
    oldRef: TextTree[SwValue],
    depth: Int,
    cn: TypeRef.Constructor,
    co: TypeRef.Constructor,
  ): TextTree[SwValue] = {
    val tmp = q"e${depth.toString}"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"$oldRef.map { $tmp in ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))} }"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"$tmp.key"
        val vv       = q"$tmp.value"

        q"Dictionary(uniqueKeysWithValues: $oldRef.map { $tmp in (${transfer(keyRef, kv, depth + 1, Some(co.args.head))}, ${transfer(valueRef, vv, depth + 1, Some(co.args.last))}) })"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"Set($oldRef.map { $tmp in ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))} })"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        val inner = transfer(c.args.head, tmp, depth + 1, Some(co.args.head))
        q"$oldRef.map { $tmp in $inner }"
      case c =>
        throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(
    oldRef: TextTree[SwValue],
    newTypeRefTree: TextTree[SwValue],
    oldTypeRefTree: TextTree[SwValue],
    s: TypeRef.Scalar,
    os: TypeRef.Scalar,
  ): TextTree[SwValue] = {
    val direct =
      if (s == os) oldRef
      else {
        builtinConversion(s.id, os.id) match {
          case Some(method) => q"$method($oldRef)"
          case None         => q"$oldRef as! $newTypeRefTree"
        }
      }

    val conv =
      q"""({ () -> $newTypeRefTree in
         |    let converted: $newTypeRefTree = conversions.convertWithContext(context, $oldRef, "${os.id.toString}", "${s.id.toString}")
         |    return converted
         |}())""".stripMargin

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
      case _ if from == to => None
      case (_, TypeId.Builtins.i16) => Some("Int16")
      case (_, TypeId.Builtins.i32) => Some("Int32")
      case (_, TypeId.Builtins.i64) => Some("Int64")
      case (_, TypeId.Builtins.u16) => Some("UInt16")
      case (_, TypeId.Builtins.u32) => Some("UInt32")
      case (_, TypeId.Builtins.u64) => Some("UInt64")
      case (_, TypeId.Builtins.f64) => Some("Double")
      case (_, TypeId.Builtins.f128) => Some("BaboonDecimal")
      case _ => None
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
          s"${trans.toSnakeCase(conv.sourceTpe.name.name)}.swift"
        )).mkString("-")

        val tin  = trans.asSwType(conv.sourceTpe, srcDom, evo).fullyQualified
        def tout = trans.asSwType(conv.targetTpe, domain, evo)

        val meta = q"""override var versionFrom: String { "${srcVer.v.toString}" }
                      |override var versionTo: String { "${domain.version.v.toString}" }
                      |override var typeId: String { "${conv.sourceTpe.toString}" }
                      """.stripMargin.trim

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""class $className: $baboonAbstractConversion<$tin, $tout> {
                              |    override func doConvert(
                              |        _ context: Any?,
                              |        _ conversions: $baboonAbstractConversions,
                              |        _ from: $tin
                              |    ) -> $tout {
                              |        fatalError("Must be overridden")
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim

            val convMethodName = makeName("conversion", conv)

            List(
              RenderedConversion(
                fname,
                tools.inLib(classDef),
                Some(q"""register("${conv.targetTpe.toString}", { required.$convMethodName() })"""),
                Some(q"func $convMethodName() -> $baboonAbstractConversion<$tin, $tout>"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q""""$fromName": "$toName","""
            }.toList
            val mappedExpr =
              if (mappingEntries.isEmpty) {
                q"from.rawValue"
              } else {
                q"""[
                   |    ${mappingEntries.joinN().shift(4).trim}
                   |][from.rawValue] ?? from.rawValue""".stripMargin
              }
            val classDef = q"""class $className: $baboonAbstractConversion<$tin, $tout> {
                              |    static let instance = $className()
                              |    private override init() { super.init() }
                              |    override func doConvert(
                              |        _ context: Any?,
                              |        _ conversions: $baboonAbstractConversions,
                              |        _ from: $tin
                              |    ) -> $tout {
                              |        let mapped = $mappedExpr
                              |        guard let result = $tout.parse(mapped) else {
                              |            fatalError("Cannot parse enum: \\(from)")
                              |        }
                              |        return result
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"""register("${conv.targetTpe.toString}", { $className.instance })"""
            List(RenderedConversion(fname, tools.inLib(classDef), Some(regtree), None))

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldCaseName = oldId.name.name.head.toLower.toString + oldId.name.name.tail
                val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
                val newCaseName = newId.name.name.head.toLower.toString + newId.name.name.tail
                val converted = transfer(TypeRef.Scalar(newId), q"x", 1, Some(TypeRef.Scalar(oldId)))
                q"if case .$oldCaseName(let x) = from { return .$newCaseName($converted) }"
            }

            val classDef = q"""class $className: $baboonAbstractConversion<$tin, $tout> {
                              |    static let instance = $className()
                              |    private override init() { super.init() }
                              |    override func doConvert(
                              |        _ context: Any?,
                              |        _ conversions: $baboonAbstractConversions,
                              |        _ from: $tin
                              |    ) -> $tout {
                              |        ${cases.joinN().shift(8).trim}
                              |        fatalError("Bad input: \\(from)")
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"""register("${conv.targetTpe.toString}", { $className.instance })"""
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
                val fld = trans.escapeSwiftKeyword(f.name.name)
                val ftype = trans.asSwRef(f.tpe, domain, evo)
                val expr = op match {
                  case o: FieldOp.Transfer => transfer(o.targetField.tpe, q"from.$fld", 1)
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.lst => q"[]"
                          case TypeId.Builtins.set => q"Set()"
                          case TypeId.Builtins.map => q"[:]"
                          case TypeId.Builtins.opt => q"nil"
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
                          case TypeId.Builtins.set => q"Set([from.$fld])"
                          case _ => throw new IllegalStateException(s"Unsupported collection wrap type: $id")
                        }
                      case _ => throw new IllegalStateException("WrapIntoCollection target must be a constructor type")
                    }
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"from.$fld", 1, Some(o.oldTpe))
                  case o: FieldOp.SwapCollectionType  => swapCollType(q"from.$fld", o, 0)
                  case o: FieldOp.Rename             => transfer(o.targetField.tpe, q"from.${trans.escapeSwiftKeyword(o.sourceFieldName.name)}", 1)
                  case o: FieldOp.Redef =>
                    val srcFieldRef = q"from.${trans.escapeSwiftKeyword(o.sourceFieldName.name)}"
                    o.modify match {
                      case _: FieldOp.WrapIntoCollection =>
                        o.targetField.tpe match {
                          case TypeRef.Constructor(id, _) =>
                            id match {
                              case TypeId.Builtins.opt => q"$srcFieldRef"
                              case TypeId.Builtins.lst => q"[$srcFieldRef]"
                              case TypeId.Builtins.set => q"Set([$srcFieldRef])"
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
                q"let ${f.name.name.toLowerCase}: $ftype = $expr"
            }
            val ctorArgs = dto.fields.map(f => q"${trans.escapeSwiftKeyword(f.name.name)}: ${f.name.name.toLowerCase}")
            val classDef = q"""class $className: $baboonAbstractConversion<$tin, $tout> {
                              |    static let instance = $className()
                              |    private override init() { super.init() }
                              |    override func doConvert(
                              |        _ context: Any?,
                              |        _ conversions: $baboonAbstractConversions,
                              |        _ from: $tin
                              |    ) -> $tout {
                              |        ${assigns.joinN().shift(8).trim}
                              |        return $tout(
                              |            ${ctorArgs.join(",\n").shift(12).trim}
                              |        )
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"""register("${conv.targetTpe.toString}", { $className.instance })"""
            List(RenderedConversion(fname, tools.inLib(classDef), Some(regtree), None))
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }

  private def swapCollType(fieldRef: TextTree[SwValue], op: FieldOp.SwapCollectionType, depth: Int): TextTree[SwValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"e"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"""$fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }.map { v in [v] } ?? []"""
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"""$fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }.map { v in Set([v]) } ?? Set()"""
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"""$fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"""$fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"""Set($fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} })"""

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"""Array($fieldRef).map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"""Set($fieldRef.map { $tmp in ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} })"""

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val kv = q"entry.key"
        val vv = q"entry.value"
        q"""Dictionary(uniqueKeysWithValues: $fieldRef.map { entry in (${transfer(newArgs.head, kv, 1, Some(oldArgs.head))}, ${transfer(
            newArgs.last,
            vv,
            1,
            Some(oldArgs.last),
          )}) })"""
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }
}

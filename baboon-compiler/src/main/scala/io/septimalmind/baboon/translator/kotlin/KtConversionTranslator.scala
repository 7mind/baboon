package io.septimalmind.baboon.translator.kotlin

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.kotlin.KtBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.translator.kotlin.KtValue.KtPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object KtConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: KtPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): KtConversionTranslator[F]
  }
}

class KtConversionTranslator[F[+_, +_]: Error2](
  trans: KtTypeTranslator,
  pkg: KtPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: KtTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def transfer(newTpe: TypeRef, oldRef: TextTree[KtValue], depth: Int, maybeOldTpe: Option[TypeRef] = None): TextTree[KtValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = trans.asKtRef(newTpe, domain, evo)
    val oldTypeRefTree = trans.asKtRef(oldTpe, srcDom, evo).fullyQualified

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, depth + 1, Some(s))
        c.id match {
          case TypeId.Builtins.opt =>
            q"$headTransfer"
          case TypeId.Builtins.lst =>
            q"listOf($headTransfer)"
          case TypeId.Builtins.set =>
            q"setOf($headTransfer)"
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
    oldRef: TextTree[KtValue],
    depth: Int,
    cn: TypeRef.Constructor,
    co: TypeRef.Constructor,
  ): TextTree[KtValue] = {
    val tmp = q"e${depth.toString}"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"$oldRef.map { $tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))} }"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"$tmp.key"
        val vv       = q"$tmp.value"

        q"$oldRef.map { $tmp -> (${transfer(keyRef, kv, depth + 1, Some(co.args.head))}) to (${transfer(valueRef, vv, depth + 1, Some(co.args.last))}) }.toMap()"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"$oldRef.map { $tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))} }.toSet()"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        q"$oldRef?.let { $tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))} }"
      case c =>
        throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(
    oldRef: TextTree[KtValue],
    newTypeRefTree: TextTree[KtValue],
    oldTypeRefTree: TextTree[KtValue],
    s: TypeRef.Scalar,
    os: TypeRef.Scalar,
  ): TextTree[KtValue] = {
    val direct =
      if (s == os) oldRef
      else {
        builtinConversion(s.id, os.id) match {
          case Some(method) => q"$oldRef.$method"
          case None         => q"($oldRef as $newTypeRefTree)"
        }
      }

    val baboonGenerated = iBaboonGenerated
    val conv =
      q"conversions.convertWithContext<C>(context, $oldRef as $baboonGenerated, $oldTypeRefTree::class.java, $newTypeRefTree::class.java) as $newTypeRefTree"

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
      case (_, TypeId.Builtins.i16)  => Some("toShort()")
      case (_, TypeId.Builtins.i32)  => Some("toInt()")
      case (_, TypeId.Builtins.i64)  => Some("toLong()")
      case (_, TypeId.Builtins.u16)  => Some("toUShort()")
      case (_, TypeId.Builtins.u32)  => Some("toUInt()")
      case (_, TypeId.Builtins.u64)  => Some("toULong()")
      case (_, TypeId.Builtins.f64)  => Some("toDouble()")
      case (_, TypeId.Builtins.f128) => Some("toBigDecimal()")
      case _                         => None
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
          s"${conv.sourceTpe.name.name}.kt"
        )).mkString("-")

        val tin  = trans.asKtType(conv.sourceTpe, srcDom, evo).fullyQualified
        def tout = trans.asKtType(conv.targetTpe, domain, evo)

        val meta = q"""override val versionFrom: String = "${srcVer.v.toString}"
                      |override val versionTo: String = "${domain.version.v.toString}"
                      |override val typeId: String = "${conv.sourceTpe.toString}"
                      """.stripMargin.trim

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""|abstract class $className
                               |  : $baboonAbstractConversion<$tin, $tout>($tin::class.java, $tout::class.java) {
                               |    abstract override fun <C> doConvert(
                               |      context: C?,
                               |      conversions: $baboonAbstractConversions,
                               |      from: $tin
                               |    ): $tout
                               |    ${meta.shift(4).trim}
                               |}
                  """.stripMargin.trim

            val convMethodName = makeName("conversion", conv)

            List(
              RenderedConversion(
                fname,
                tools.inPkg(pkg.parts.toSeq, classDef),
                Some(q"register(required.$convMethodName())"),
                Some(q"fun $convMethodName(): $baboonAbstractConversion<$tin, $tout>"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                s""""$fromName" to "$toName""""
            }
            val mappedExpr =
              if (mappingEntries.isEmpty) {
                q"from.name"
              } else {
                val mappingLiteral = s"mapOf(${mappingEntries.mkString(", ")})"
                q"$mappingLiteral.getOrDefault(from.name, from.name)"
              }
            val classDef = q"""|object $className
                               |  : $baboonAbstractConversion<$tin, $tout>($tin::class.java, $tout::class.java) {
                               |    override fun <C> doConvert(
                               |      context: C?,
                               |      conversions: $baboonAbstractConversions,
                               |      from: $tin
                               |    ): $tout = $tout.parse($mappedExpr) ?: throw $javaIllegalArgumentException("Cannot parse enum: " + from)
                               |    ${meta.shift(4).trim}
                               |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inPkg(pkg.parts.toSeq, classDef), Some(regtree), None))

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT  = trans.asKtType(oldId, srcDom, evo).fullyQualified
                val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
                q"is $oldT -> ${transfer(TypeRef.Scalar(newId), q"x", 1, Some(TypeRef.Scalar(oldId)))}"
            } :+ q"else -> throw $javaIllegalArgumentException(\"Bad input: \" + from)"

            val classDef = q"""
                              |object $className
                              |  : $baboonAbstractConversion<$tin, $tout>($tin::class.java, $tout::class.java) {
                              |    override fun <C> doConvert(
                              |      context: C?,
                              |      conversions: $baboonAbstractConversions,
                              |      from: $tin
                              |    ): $tout = when (val x = from) {
                              |      ${cases.joinN().shift(6).trim}
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inPkg(pkg.parts.toSeq, classDef), Some(regtree), None))

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
                  case o: FieldOp.Transfer => transfer(o.targetField.tpe, q"_from.$fld", 1)
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.lst => q"emptyList()"
                          case TypeId.Builtins.set => q"emptySet()"
                          case TypeId.Builtins.map => q"emptyMap()"
                          case TypeId.Builtins.opt => q"null"
                          case _ => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }

                  case _: FieldOp.WrapIntoCollection => q"listOf(_from.$fld) as ${trans.asKtRef(f.tpe, domain, evo)}"
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"_from.$fld", 1, Some(o.oldTpe))
                  case o: FieldOp.SwapCollectionType  => swapCollType(q"_from.$fld", o, 0)
                  case o: FieldOp.Rename             => transfer(o.targetField.tpe, q"_from.${o.sourceFieldName.name}", 1)
                  case o: FieldOp.Redef =>
                    val srcFieldRef = q"_from.${o.sourceFieldName.name}"
                    o.modify match {
                      case _: FieldOp.WrapIntoCollection =>
                        q"listOf($srcFieldRef) as ${trans.asKtRef(f.tpe, domain, evo)}"
                      case m: FieldOp.ExpandPrecision =>
                        transfer(m.newTpe, srcFieldRef, 1, Some(m.oldTpe))
                      case m: FieldOp.SwapCollectionType =>
                        swapCollType(srcFieldRef, m, 0)
                    }
                }
                q"val ${f.name.name.toLowerCase}: ${trans.asKtRef(f.tpe, domain, evo)} = $expr"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name} = ${f.name.name.toLowerCase}")
            val classDef = q"""
                              |object $className
                              |  : $baboonAbstractConversion<$tin, $tout>($tin::class.java, $tout::class.java) {
                              |    override fun <C> doConvert(
                              |      context: C?,
                              |      conversions: $baboonAbstractConversions,
                              |      _from: $tin
                              |    ): $tout {
                              |      ${assigns.joinN().shift(6).trim}
                              |      return $tout(
                              |        ${ctorArgs.join(",\n").shift(8).trim}
                              |      )
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inPkg(pkg.parts.toSeq, classDef), Some(regtree), None))
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }

  }

  private def swapCollType(fieldRef: TextTree[KtValue], op: FieldOp.SwapCollectionType, depth: Int): TextTree[KtValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"e"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"""listOfNotNull($fieldRef?.let { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} })"""
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"""setOfNotNull($fieldRef?.let { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} })"""
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"""$fieldRef?.let { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"""$fieldRef.map { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"""$fieldRef.map { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }.toSet()"""

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"""$fieldRef.map { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }"""
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"""$fieldRef.map { e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))} }.toSet()"""

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val keyRef = q"k"
        val valRef = q"v"
        q"""$fieldRef.map { (k, v) -> (${transfer(newArgs.head, keyRef, 1, Some(oldArgs.head))}) to (${transfer(
            newArgs.last,
            valRef,
            1,
            Some(oldArgs.last),
          )}) }.toMap()"""
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }
}

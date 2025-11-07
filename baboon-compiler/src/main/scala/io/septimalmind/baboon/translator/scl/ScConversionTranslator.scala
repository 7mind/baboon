package io.septimalmind.baboon.translator.scl

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.scl.ScBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.translator.scl.ScValue.ScPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object ScConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: ScPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): ScConversionTranslator[F]
  }
}

class ScConversionTranslator[F[+_, +_]: Error2](
  trans: ScTypeTranslator,
  pkg: ScPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: ScTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def transfer(newTpe: TypeRef, oldRef: TextTree[ScValue], depth: Int, maybeOldTpe: Option[TypeRef] = None): TextTree[ScValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = trans.asScRef(newTpe, domain, evo)
    val oldTypeRefTree = trans.asScRef(oldTpe, srcDom, evo).fullyQualified

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, depth + 1, Some(s))
        c.id match {
          case TypeId.Builtins.opt =>
            q"$scOption($headTransfer)"
          case TypeId.Builtins.lst =>
            q"$scList($headTransfer)"
          case TypeId.Builtins.set =>
            q"$scSet($headTransfer)"
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

  private def transferConstructor(oldRef: TextTree[ScValue], depth: Int, cn: TypeRef.Constructor, co: TypeRef.Constructor) = {
    val tmp = q"e${depth.toString}"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"$oldRef.map($tmp => ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).toList"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"$tmp._1"
        val vv       = q"$tmp._2"

        q"$oldRef.map($tmp => (${transfer(keyRef, kv, depth + 1, Some(co.args.head))}, ${transfer(valueRef, vv, depth + 1, Some(co.args.last))})).toMap"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"$oldRef.map($tmp => ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).toSet"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        q"$oldRef.map($tmp => ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))})"
      case c =>
        throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(oldRef: TextTree[ScValue], newTypeRefTree: TextTree[ScValue], oldTypeRefTree: TextTree[ScValue], s: TypeRef.Scalar, os: TypeRef.Scalar) = {
    val direct = if (s == os) {
      oldRef
    } else {
      q"($oldRef.asInstanceOf[$newTypeRefTree])"
    }

    val conv =
      q"conversions.convertWithContext[C, $oldTypeRefTree, $newTypeRefTree](context, $oldRef)"

    s.id match {
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
  }

  def makeConvs(): Out[List[RenderedConversion]] = {
    def makeName(prefix: String, conv: Conversion): String =
      (Seq(prefix) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
        conv.sourceTpe.name.name,
        "From",
        srcVer.version.replace('.', '_'),
      )).mkString("__")

    F.flatTraverseAccumErrors(rules.conversions) {
      conv =>
        val className = makeName("Convert", conv)
        val fname = (Seq("from", srcVer.version) ++ conv.sourceTpe.owner.asPseudoPkg ++ Seq(
          s"${conv.sourceTpe.name.name}.scala"
        )).mkString("-")

        val tin  = trans.asScType(conv.sourceTpe, srcDom, evo).fullyQualified
        def tout = trans.asScType(conv.sourceTpe, domain, evo)

        val meta = q"""override def versionFrom: String = "${srcVer.version}"
                      |override def versionTo:   String = "${domain.version.version}"
                      |override def typeId:      String = "${conv.sourceTpe.toString}"
                      """.stripMargin.trim

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""|abstract class $className
                               |  extends $abstractBaboonConversion[$tin, $tout] {
                               |    def doConvert[C](
                               |      context: C,
                               |      conversions: $abstractBaboonConversions,
                               |      from: $tin
                               |    ): $tout
                               |    ${meta.shift(4).trim}
                               |}
                  """.stripMargin.trim

            val convMethodName = makeName("conversion", conv)

            List(
              RenderedConversion(
                fname,
                tools.inNs(pkg.parts.toSeq, classDef),
                Some(q"register(required.$convMethodName())"),
                Some(q"def $convMethodName(): $abstractBaboonConversion[$tin, $tout]"),
              )
            )

          case _: Conversion.RemovedTypeNoConversion =>
            Nil
          case _: Conversion.NonDataTypeTypeNoConversion =>
            Nil

          case _: Conversion.CopyEnumByName =>
            val classDef = q"""|object $className
                               |  extends $abstractBaboonConversion[$tin, $tout] {
                               |    override def doConvert[C](
                               |      context: C,
                               |      conversions: $abstractBaboonConversions,
                               |      from: $tin
                               |    ): $tout = $tout.parse(from.toString).get
                               |    ${meta.shift(4).trim}
                               |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), Some(regtree), None))

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT = trans.asScType(oldId, srcDom, evo).fullyQualified
                q"case x: $oldT => ${transfer(TypeRef.Scalar(oldId), q"x", 1)}"
            } :+ q"case other => throw new IllegalArgumentException(s\"Bad input: $$other\")"

            val classDef = q"""
                              |object $className
                              |  extends $abstractBaboonConversion[$tin, $tout] {
                              |    override def doConvert[C](
                              |      context: C,
                              |      conversions: $abstractBaboonConversions,
                              |      from: $tin
                              |    ): $tout = from match {
                              |      ${cases.join("\n").shift(6).trim}
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), Some(regtree), None))

          case c: Conversion.DtoConversion =>
            val dto = domain.defs.meta.nodes(c.sourceTpe) match {
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
                      case TypeRef.Constructor(id, args) =>
                        id match {
                          case TypeId.Builtins.lst =>
                            q"$scList.empty[${trans.asScRef(args.head, domain, evo)}]"
                          case TypeId.Builtins.set =>
                            q"$scSet.empty[${trans.asScRef(args.head, domain, evo)}]"
                          case TypeId.Builtins.map =>
                            q"$scMap.empty[${trans.asScRef(args.head, domain, evo)}, ${trans.asScRef(args.last, domain, evo)}]"
                          case TypeId.Builtins.opt =>
                            q"$scOption.empty[${trans.asScRef(args.head, domain, evo)}]"
                          case _ => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }

                  case _: FieldOp.WrapIntoCollection => q"$scList(_from.$fld).asInstanceOf[${trans.asScRef(f.tpe, domain, evo)}]"
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"_from.$fld", 1)
                  case o: FieldOp.SwapCollectionType => swapCollType(q"_from.$fld", o, 0)
                }
                q"val ${f.name.name.toLowerCase}: ${trans.asScRef(f.tpe, domain, evo)} = $expr"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name.toLowerCase}")
            val classDef = q"""
                              |object $className
                              |  extends $abstractBaboonConversion[$tin, $tout] {
                              |    override def doConvert[C](
                              |      context: C,
                              |      conversions: $abstractBaboonConversions,
                              |      _from: $tin
                              |    ): $tout = {
                              |      ${assigns.join("\n").shift(6).trim}
                              |      $tout(
                              |              ${ctorArgs.join(",\n").shift(14).trim}
                              |           )
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className)"
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), Some(regtree), None))
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }

  }

  private def swapCollType(fieldRef: TextTree[ScValue], op: FieldOp.SwapCollectionType, depth: Int): TextTree[ScValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"e"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList"""
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toSet"""
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))})"""

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList"""
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toSet"""

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList"""
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"""$fieldRef.map(e => ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toSet"""

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val keyRef = q"k"
        val valRef = q"v"
        q"""$fieldRef.map { case ($keyRef, $valRef) => (${transfer(newArgs.head, keyRef, 1, Some(oldArgs.head))}, ${transfer(
            newArgs.last,
            valRef,
            1,
            Some(oldArgs.last),
          )}) }.toMap"""
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }
}

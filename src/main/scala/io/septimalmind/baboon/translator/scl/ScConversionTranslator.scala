package io.septimalmind.baboon.translator.scl

import distage.Id
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
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
  type Out[T] = F[NEList[BaboonIssue.TranslationIssue], T]

  private def transfer(tpe: TypeRef, ref: TextTree[ScValue], depth: Int): TextTree[ScValue] = {
    val cnew =
      trans.asScRef(tpe, domain, evo)
    import io.septimalmind.baboon.translator.FQNSymbol.*
    val cold = trans.asScRef(tpe, srcDom, evo).fullyQualified

    val direct = q"($ref.asInstanceOf[$cnew])"
    tpe match {
      case TypeRef.Scalar(id) =>
        val conv =
          q"conversions.convertWithContext[C, $cold, $cnew](context, $ref.asInstanceOf[$cold])"
        id match {
          case _: TypeId.Builtin =>
            direct
          case id: TypeId.User =>
            domain.defs.meta.nodes(id) match {
              case DomainMember.User(_, _: Typedef.Foreign, _) =>
                direct
              case _ =>
                conv
            }
        }

      case c: TypeRef.Constructor =>
        val tmp = q"e${depth.toString}"
        c match {
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
            q"$ref.map($tmp => ${transfer(c.args.head, tmp, depth + 1)}).toList"

          case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
            val keyRef   = c.args.head
            val valueRef = c.args.last
            val kv       = q"$tmp._1"
            val vv       = q"$tmp._2"

            q"$ref.map($tmp => (${transfer(keyRef, kv, depth + 1)}, ${transfer(valueRef, vv, depth + 1)})).toMap"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
            q"$ref.map($tmp => ${transfer(c.args.head, tmp, depth + 1)}).toSet"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
            q"$ref.map($tmp => ${transfer(c.args.head, tmp, depth + 1)})"
          case _ =>
            ???
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

        // common version/typeId metadata block
        val meta = q"""override def versionFrom: String = "${srcVer.version}"
                      |override def versionTo:   String = "${domain.version.version}"
                      |override def typeId:      String = "${conv.sourceTpe.toString}"
                      """.stripMargin.trim

        // render each case
        val rendered = conv match {
          // 1) Custom conversion: abstract stub
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

            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))

          // 2) Removed/no‐op conversions
          case _: Conversion.RemovedTypeNoConversion =>
            Nil
          case _: Conversion.NonDataTypeTypeNoConversion =>
            Nil

          // 3) Enum‐by‐name conversion
          case _: Conversion.CopyEnumByName =>
            val classDef = q"""|final class $className
                               |  extends $abstractBaboonConversion[$tin, $tout] {
                               |    override def doConvert[C](
                               |      context: C,
                               |      conversions: $abstractBaboonConversions,
                               |      from: $tin
                               |    ): $tout = $tout.parse(from.toString).get
                               |    ${meta.shift(4).trim}
                               |}
                  """.stripMargin.trim
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))

          // 4) ADT‐branch conversion
          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT = trans.asScType(oldId, srcDom, evo).fullyQualified
                q"case x: $oldT => ${transfer(TypeRef.Scalar(oldId), q"x", 1)}"
            } :+ q"case other => throw new IllegalArgumentException(s\"Bad input: $$other\")"

            val classDef = q"""
                              |final class $className
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
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))

          // 5) DTO conversion
          case c: Conversion.DtoConversion =>
            val dto = domain.defs.meta.nodes(c.sourceTpe) match {
              case DomainMember.User(_, d: Typedef.Dto, _) => d
              case _                                       => throw new IllegalStateException("DTO expected")
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
                          case _ => ???
                        }
                      case _ => ???
                    }

                  case _: FieldOp.WrapIntoCollection => q"$scList(_from.$fld).asInstanceOf[${trans.asScRef(f.tpe, domain, evo)}]"
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"_from.$fld", 1)
                  case o: FieldOp.SwapCollectionType => transfer(o.newTpe, q"_from.$fld", 1)
                }
                q"val ${f.name.name.toLowerCase}: ${trans.asScRef(f.tpe, domain, evo)} = $expr"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name.toLowerCase}")
            val classDef = q"""
                              |final class $className
                              |  extends $abstractBaboonConversion[$tin, $tout] {
                              |    override def doConvert[C](
                              |      context: C,
                              |      conversions: $abstractBaboonConversions,
                              |      _from: $tin
                              |    ): $tout = {
                              |      ${assigns.join("\n").shift(6).trim}
                              |      $tout(${ctorArgs.join(", ").shift(6).trim})
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))
        }

        if (false) {
          F.fail(NEList(BaboonIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }
}

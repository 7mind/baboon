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

    val direct = q"(($cnew) $ref)"
    tpe match {
      case TypeRef.Scalar(id) =>
        val conv =
          q"conversions.ConvertWithContext<C, $cold, $cnew>(context, ($cold) $ref)"

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
            q"(from $tmp in $ref select ${transfer(c.args.head, tmp, depth + 1)}).toList"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
            val keyRef   = c.args.head
            val valueRef = c.args.last
            q"(from $tmp in $ref select new TODO<${trans
                .asScRef(keyRef, domain, evo)}, ${trans
                .asScRef(valueRef, domain, evo)}>(${transfer(
                c.args.head,
                q"$tmp.Key",
                depth + 1,
              )}, ${transfer(c.args.last, q"$tmp.Value", depth + 1)})).toMap"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
            q"(from $tmp in $ref select ${transfer(c.args.head, tmp, depth + 1)}).toSet"
          case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
            val underlyingTpe = c.args.head
            val recConv =
              transfer(underlyingTpe, ref, depth + 1)

            q"($ref == null ? null : $recConv)"

          case _ =>
            ???
        }
    }
  }

  def makeConvs(): Out[List[RenderedConversion]] = {
    // build a unique class name for each conversion rule
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
        val meta = q"""
                override def versionFrom: String = \"${srcVer.version}\"
                override def versionTo:   String = \"${domain.version.version}\"
                override def typeId:      String = \"${conv.sourceTpe.toString}\"
              """

        // render each case
        val rendered = conv match {
          // 1) Custom conversion: abstract stub
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""
                    abstract class $className
                      extends $abstractBaboonConversion[$tin, $tout] {
                        def doConvert[C](
                          context: C,
                          conversions: $abstractBaboonConversions,
                          from: $tin
                        ): $tout
                        $meta
                    }
                  """
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))

          // 2) Removed/no‐op conversions
          case _: Conversion.RemovedTypeNoConversion =>
            Nil
          case _: Conversion.NonDataTypeTypeNoConversion =>
            Nil

          // 3) Enum‐by‐name conversion
          case _: Conversion.CopyEnumByName =>
            val classDef = q"""
                    final class $className
                      extends $abstractBaboonConversion[$tin, $tout] {
                        override def doConvert[C](
                          context: C,
                          conversions: $abstractBaboonConversions,
                          from: $tin
                        ): $tout = $tout.withName(from.toString)
                        $meta
                    }
                  """
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))

          // 4) ADT‐branch conversion
          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT = trans.asScType(oldId, srcDom, evo).fullyQualified
                q"case x: $oldT => ${transfer(TypeRef.Scalar(oldId), q"x", 1)}"
            } :+ q"case other => throw new IllegalArgumentException(s\"Bad input: $$other\")"

            val classDef = q"""
                    final class $className
                      extends $abstractBaboonConversion[$tin, $tout] {
                        override def doConvert[C](
                          context: C,
                          conversions: $abstractBaboonConversions,
                          from: $tin
                        ): $tout = from match {
                          ${cases.join("\n")}
                        }
                        $meta
                    }
                  """
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
                val fld = f.name.name.capitalize
                val expr = op match {
                  case o: FieldOp.Transfer              => transfer(o.targetField.tpe, q"_from.$fld", 1)
                  case _: FieldOp.InitializeWithDefault => q"default[${trans.asScRef(f.tpe, domain, evo)}]"
                  case _: FieldOp.WrapIntoCollection    => q"List(_from.$fld).asInstanceOf[${trans.asScRef(f.tpe, domain, evo)}]"
                  case o: FieldOp.ExpandPrecision       => transfer(o.newTpe, q"_from.$fld", 1)
                  case o: FieldOp.SwapCollectionType    => transfer(o.newTpe, q"_from.$fld", 1)
                }
                q"val ${f.name.name.toLowerCase}: ${trans.asScRef(f.tpe, domain, evo)} = $expr"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name.toLowerCase}")
            val classDef = q"""
                    final class $className
                      extends $abstractBaboonConversion[$tin, $tout] {
                        override def doConvert[C](
                          context: C,
                          conversions: $abstractBaboonConversions,
                          _from: $tin
                        ): $tout = {
                          ${assigns.join("\n")}
                          new $tout(${ctorArgs.join(", ")})
                        }
                        $meta
                    }
                  """
            List(RenderedConversion(fname, tools.inNs(pkg.parts.toSeq, classDef), None, None))
        }

        if (false) {
          F.fail(NEList(BaboonIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }

//  private def swapCollType(
//    ftNewInit: TextTree[ScValue],
//    base: String,
//    fieldRef: TextTree[Nothing],
//    oldId: TypeId.BuiltinCollection,
//    newId: TypeId.BuiltinCollection,
//    newCollArgs: NEList[TypeRef],
//  ): F[NEList[BaboonIssue.TranslationBug], Seq[TextTree[ScValue]]] = {
//    val collCsType = trans.asScRef(newCollArgs.head)
//
//    val collInit =
//      q"(new $ftNewInit(from e in $fieldRef select ($collCsType)e))"
//
//    oldId match {
//      case TypeId.Builtins.opt =>
//        val tmp = q"_${base.toLowerCase}_tmp"
//
//        val recConv =
//          transfer(newCollArgs.head, tmp, 0)
//
//        newId match {
//          case TypeId.Builtins.lst =>
//            F.pure(
//              Seq(
//                q"var $tmp = $fieldRef",
//                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).toList",
//              )
//            )
//          case TypeId.Builtins.set =>
//            F.pure(
//              Seq(
//                q"var $tmp = $fieldRef",
//                q"( ($tmp != null) ? new $ftNewInit { $recConv } : new $ftNewInit() ).toSet",
//              )
//            )
//          case TypeId.Builtins.opt =>
//            F.pure(
//              Seq(
//                q"var $tmp = $fieldRef",
//                q"( ($tmp != null) ? $recConv : null )",
//              )
//            )
//          case _ =>
//            F.fail(NEList(BaboonIssue.TranslationBug()))
//        }
//      case TypeId.Builtins.lst =>
//        newId match {
//          case TypeId.Builtins.set =>
//            F.pure(Seq(q"$collInit.toSet"))
//
//          case TypeId.Builtins.lst =>
//            F.pure(Seq(q"$collInit.toList"))
//          case _ =>
//            F.fail(NEList(BaboonIssue.TranslationBug()))
//        }
//      case TypeId.Builtins.set =>
//        newId match {
//          case TypeId.Builtins.set =>
//            F.pure(Seq(q"$collInit.toSet"))
//          case TypeId.Builtins.lst =>
//            F.pure(Seq(q"$collInit.toList"))
//          case _ =>
//            F.fail(NEList(BaboonIssue.TranslationBug()))
//        }
//      case TypeId.Builtins.map =>
//        newId match {
//          case TypeId.Builtins.map =>
//            val kt = trans.asScRef(newCollArgs.head)
//            val vt = trans.asScRef(newCollArgs.last)
//            F.pure(
//              Seq(
//                q"(from e in $fieldRef select new $csKeyValuePair<$kt, $vt>(($kt)e.Key, ($vt)e.Value)).toMap"
//              )
//            )
//          case _ =>
//            F.fail(NEList(BaboonIssue.TranslationBug()))
//        }
//      case _ =>
//        F.fail(NEList(BaboonIssue.TranslationBug()))
//    }
//  }

}

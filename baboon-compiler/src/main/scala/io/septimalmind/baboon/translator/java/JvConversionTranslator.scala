package io.septimalmind.baboon.translator.java

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.java.JvBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.translator.java.JvValue.JvPackageId
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object JvConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      pkg: JvPackageId,
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evo: BaboonEvolution,
    ): JvConversionTranslator[F]
  }
}

class JvConversionTranslator[F[+_, +_]: Error2](
  trans: JvTypeTranslator,
  pkg: JvPackageId,
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  tools: JvTreeTools,
  evo: BaboonEvolution,
) {
  private val srcVer = srcDom.version
  type Out[T] = F[NEList[BaboonIssue], T]

  private def transfer(newTpe: TypeRef, oldRef: TextTree[JvValue], depth: Int, maybeOldTpe: Option[TypeRef] = None): TextTree[JvValue] = {
    import io.septimalmind.baboon.translator.FQNSymbol.*

    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = trans.asJvRef(newTpe, domain, evo)
    val oldTypeRefTree = trans.asJvRef(oldTpe, srcDom, evo).fullyQualified

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, depth + 1, Some(s))
        c.id match {
          case TypeId.Builtins.opt =>
            q"$jvOptional.of($headTransfer)"
          case TypeId.Builtins.lst =>
            q"$jvList.of($headTransfer)"
          case TypeId.Builtins.set =>
            q"$jvSet.of($headTransfer)"
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
    oldRef: TextTree[JvValue],
    depth: Int,
    cn: TypeRef.Constructor,
    co: TypeRef.Constructor,
  ): TextTree[JvValue] = {
    val tmp = q"e${depth.toString}"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"$oldRef.stream().map($tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).toList()"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"$tmp.getKey()"
        val vv       = q"$tmp.getValue()"

        q"$oldRef.entrySet().stream().collect(java.util.stream.Collectors.toMap($tmp -> ${transfer(keyRef, kv, depth + 1, Some(co.args.head))}, $tmp -> ${transfer(valueRef, vv, depth + 1, Some(co.args.last))}))"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"$oldRef.stream().map($tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))}).collect(java.util.stream.Collectors.toSet())"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        q"$oldRef.map($tmp -> ${transfer(c.args.head, tmp, depth + 1, Some(co.args.head))})"
      case c =>
        throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(
    oldRef: TextTree[JvValue],
    newTypeRefTree: TextTree[JvValue],
    oldTypeRefTree: TextTree[JvValue],
    s: TypeRef.Scalar,
    os: TypeRef.Scalar,
  ): TextTree[JvValue] = {
    val direct =
      if (s == os) oldRef
      else {
        builtinConversion(s.id, os.id) match {
          case Some(method) => q"$method($oldRef)"
          case None         => q"($newTypeRefTree) $oldRef"
        }
      }

    val conv =
      q"($newTypeRefTree) conversions.convertWithContext(context, $oldRef, $oldTypeRefTree.class, $newTypeRefTree.class)"

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
      case _ if from == to           => None
      case (_, TypeId.Builtins.i16)  => Some("(short)")
      case (_, TypeId.Builtins.i32)  => Some("(int)")
      case (_, TypeId.Builtins.i64)  => Some("(long)")
      case (_, TypeId.Builtins.u16)  => Some("(int)")
      case (_, TypeId.Builtins.u32)  => Some("(long)")
      case (_, TypeId.Builtins.u64)  => Some("(long)")
      case (_, TypeId.Builtins.f64)  => Some("(double)")
      case (_, TypeId.Builtins.f128) => Some("java.math.BigDecimal.valueOf")
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
        val fname     = s"$className.java"

        val tin  = trans.asJvType(conv.sourceTpe, srcDom, evo).fullyQualified
        def tout = trans.asJvType(conv.targetTpe, domain, evo)

        val meta = q"""@Override public String versionFrom() { return "${srcVer.v.toString}"; }
                      |@Override public String versionTo() { return "${domain.version.v.toString}"; }
                      |@Override public String typeId() { return "${conv.sourceTpe.toString}"; }
                      """.stripMargin.trim

        val rendered = conv match {
          case _: Conversion.CustomConversionRequired =>
            val classDef = q"""public abstract class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    public $className() { super($tin.class, $tout.class); }
                              |    @Override public abstract <C> $tout doConvert(
                              |      C context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from
                              |    );
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim

            val convMethodName = makeName("conversion", conv)

            List(
              RenderedConversion(
                fname,
                tools.inPkg(pkg.parts.toSeq, classDef),
                Some(q"""register(required.$convMethodName());"""),
                Some(q"$baboonAbstractConversion<$tin, $tout> $convMethodName();"),
              )
            )

          case c: Conversion.CopyEnumByName =>
            val mappingEntries = c.memberMapping.map {
              case (fromName, toName) =>
                q"""case "$fromName" -> "$toName";"""
            }.toList
            val mappedExpr =
              if (mappingEntries.isEmpty) {
                q"from.name()"
              } else {
                q"""switch (from.name()) {
                   |  ${mappingEntries.joinN().shift(2).trim}
                   |  default -> from.name();
                   |}""".stripMargin
              }
            val classDef = q"""public final class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    public static final $className INSTANCE = new $className();
                              |    private $className() { super($tin.class, $tout.class); }
                              |    @Override public <C> $tout doConvert(
                              |      C context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from
                              |    ) {
                              |      String mapped = $mappedExpr;
                              |      return $tout.parse(mapped);
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.INSTANCE);"
            List(RenderedConversion(fname, tools.inPkg(pkg.parts.toSeq, classDef), Some(regtree), None))

          case c: Conversion.CopyAdtBranchByName =>
            val cases = c.oldDefn.dataMembers(srcDom).map {
              oldId =>
                val oldT  = trans.asJvType(oldId, srcDom, evo).fullyQualified
                val newId = c.branchMapping.getOrElse(oldId.name.name, oldId)
                q"if (from instanceof $oldT x) { return ${transfer(TypeRef.Scalar(newId), q"x", 1, Some(TypeRef.Scalar(oldId)))}; }"
            }

            val classDef = q"""public final class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    public static final $className INSTANCE = new $className();
                              |    private $className() { super($tin.class, $tout.class); }
                              |    @Override public <C> $tout doConvert(
                              |      C context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from
                              |    ) {
                              |      ${cases.joinN().shift(6).trim}
                              |      throw new $javaIllegalArgumentException("Bad input: " + from);
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.INSTANCE);"
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
                  case o: FieldOp.Transfer => transfer(o.targetField.tpe, q"from.$fld()", 1)
                  case o: FieldOp.InitializeWithDefault =>
                    o.targetField.tpe match {
                      case TypeRef.Constructor(id, args) =>
                        id match {
                          case TypeId.Builtins.lst => q"$jvList.<${trans.asJvBoxedRef(args.head, domain, evo)}>of()"
                          case TypeId.Builtins.set => q"$jvSet.<${trans.asJvBoxedRef(args.head, domain, evo)}>of()"
                          case TypeId.Builtins.map => q"$jvMap.<${trans.asJvBoxedRef(args.head, domain, evo)}, ${trans.asJvBoxedRef(args.last, domain, evo)}>of()"
                          case TypeId.Builtins.opt => q"$jvOptional.<${trans.asJvBoxedRef(args.head, domain, evo)}>empty()"
                          case _                   => throw new IllegalStateException(s"Unsupported constructor type: $id")
                        }
                      case _ => throw new IllegalStateException("Unsupported target field type")
                    }

                  case _: FieldOp.WrapIntoCollection =>
                    f.tpe match {
                      case TypeRef.Constructor(id, _) =>
                        id match {
                          case TypeId.Builtins.opt => q"$jvOptional.of(from.$fld())"
                          case TypeId.Builtins.lst => q"$jvList.of(from.$fld())"
                          case TypeId.Builtins.set => q"$jvSet.of(from.$fld())"
                          case _                   => throw new IllegalStateException(s"Unsupported collection wrap type: $id")
                        }
                      case _ => throw new IllegalStateException("WrapIntoCollection target must be a constructor type")
                    }
                  case o: FieldOp.ExpandPrecision    => transfer(o.newTpe, q"from.$fld()", 1, Some(o.oldTpe))
                  case o: FieldOp.SwapCollectionType => swapCollType(q"from.$fld()", o, 0)
                  case o: FieldOp.Rename             => transfer(o.targetField.tpe, q"from.${o.sourceFieldName.name}()", 1)
                  case o: FieldOp.Redef =>
                    val srcFieldRef = q"from.${o.sourceFieldName.name}()"
                    o.modify match {
                      case _: FieldOp.WrapIntoCollection =>
                        o.targetField.tpe match {
                          case TypeRef.Constructor(id, _) =>
                            id match {
                              case TypeId.Builtins.opt => q"$jvOptional.of($srcFieldRef)"
                              case TypeId.Builtins.lst => q"$jvList.of($srcFieldRef)"
                              case TypeId.Builtins.set => q"$jvSet.of($srcFieldRef)"
                              case _                   => throw new IllegalStateException(s"Unsupported collection wrap type: $id")
                            }
                          case _ => throw new IllegalStateException("WrapIntoCollection target must be a constructor type")
                        }
                      case m: FieldOp.ExpandPrecision =>
                        transfer(m.newTpe, srcFieldRef, 1, Some(m.oldTpe))
                      case m: FieldOp.SwapCollectionType =>
                        swapCollType(srcFieldRef, m, 0)
                    }
                }
                q"var ${f.name.name.toLowerCase} = $expr;"
            }
            val ctorArgs = dto.fields.map(f => q"${f.name.name.toLowerCase}")
            val classDef = q"""public final class $className
                              |  extends $baboonAbstractConversion<$tin, $tout> {
                              |    public static final $className INSTANCE = new $className();
                              |    private $className() { super($tin.class, $tout.class); }
                              |    @Override public <C> $tout doConvert(
                              |      C context,
                              |      $baboonAbstractConversions conversions,
                              |      $tin from
                              |    ) {
                              |      ${assigns.joinN().shift(6).trim}
                              |      return new $tout(
                              |        ${ctorArgs.join(",\n").shift(8).trim}
                              |      );
                              |    }
                              |    ${meta.shift(4).trim}
                              |}
                  """.stripMargin.trim
            val regtree = q"register($className.INSTANCE);"
            List(RenderedConversion(fname, tools.inPkg(pkg.parts.toSeq, classDef), Some(regtree), None))
        }

        if (false) {
          F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
        } else {
          F.pure(rendered)
        }
    }
  }

  private def swapCollType(fieldRef: TextTree[JvValue], op: FieldOp.SwapCollectionType, depth: Int): TextTree[JvValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"e"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"""$fieldRef.map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).stream().toList()"""
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"""$fieldRef.map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).stream().collect(java.util.stream.Collectors.toSet())"""
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"""$fieldRef.map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))})"""

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"""$fieldRef.stream().map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList()"""
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"""$fieldRef.stream().map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).collect(java.util.stream.Collectors.toSet())"""

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"""$fieldRef.stream().map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).toList()"""
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"""$fieldRef.stream().map(e -> ${transfer(newArgs.head, tmp, depth, Some(oldArgs.head))}).collect(java.util.stream.Collectors.toSet())"""

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val kv = q"entry.getKey()"
        val vv = q"entry.getValue()"
        q"""$fieldRef.entrySet().stream().collect(java.util.stream.Collectors.toMap(entry -> ${transfer(newArgs.head, kv, 1, Some(oldArgs.head))}, entry -> ${transfer(
            newArgs.last,
            vv,
            1,
            Some(oldArgs.last),
          )}))"""
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }
}

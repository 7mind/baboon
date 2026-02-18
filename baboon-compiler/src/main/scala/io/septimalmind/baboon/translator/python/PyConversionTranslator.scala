package io.septimalmind.baboon.translator.python

import distage.Id
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.python.PyBaboonTranslator.RenderedConversion
import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

import scala.util.chaining.scalaUtilChainingOps

object PyConversionTranslator {
  trait Factory[F[+_, +_]] {
    def apply(
      srcDom: Domain @Id("source"),
      domain: Domain @Id("current"),
      rules: BaboonRuleset,
      evolution: BaboonEvolution,
    ): PyConversionTranslator[F]
  }
}

final class PyConversionTranslator[F[+_, +_]: Error2](
  srcDom: Domain @Id("source"),
  domain: Domain @Id("current"),
  rules: BaboonRuleset,
  evolution: BaboonEvolution,
  typeTranslator: PyTypeTranslator,
  pyFileTools: PyFileTools,
) {
  private val sourceVersion  = srcDom.version
  private val currentVersion = domain.version

  private val sourceVersionString  = sourceVersion.format(delimiter = "_")
  private val currentVersionString = currentVersion.format(prefix = "v", delimiter = "_")

  type Out[T] = F[NEList[BaboonIssue], T]

  def makeConversions: Out[List[RenderedConversion]] = {
    F.flatTraverseAccumErrors(rules.conversions) {
      case _: Conversion.RemovedTypeNoConversion     => F.pure(Nil)
      case _: Conversion.NonDataTypeTypeNoConversion => F.pure(Nil)
      case conversion: TargetedConversion =>
        val convType = conversionType(conversion)
        val fileName = s"${convType.moduleId.module}.py"

        val typeFrom = typeTranslator.asPyTypeVersioned(conversion.sourceTpe, srcDom, evolution, pyFileTools.definitionsBasePkg)
        def typeTo   = typeTranslator.asPyType(conversion.targetTpe, domain, evolution, pyFileTools.definitionsBasePkg)

        val meta =
          q"""@$pyStaticMethod
             |def version_from() -> str:
             |    return "${sourceVersion.v.toString}"
             |
             |@$pyStaticMethod
             |def version_to() -> str:
             |    return "${domain.version.v.toString}"
             |
             |@$pyStaticMethod
             |def type_id() -> str:
             |    return "${conversion.sourceTpe.toString}"
             |
             |@$pyClassMethod
             |@$pyCache
             |def instance (cls):
             |    return cls()
             |""".stripMargin

        val convTree           = genConversionTree(conversion, typeFrom, typeTo, convType, meta)
        val registerTree       = genRegisterTree(conversion, typeFrom, typeTo, convType)
        val abstractConversion = genAbstractConversion(conversion, convType)

        convTree
          .zip(registerTree)
          .fold(F.pure(List.empty[RenderedConversion])) { case (conv, reg) => F.pure(List(RenderedConversion(fileName, conv, reg, abstractConversion))) }
          .catchSome {
            case _ => F.fail(BaboonIssue.of(TranslationIssue.TranslationBug()))
          }
    }
  }

  private def genRegisterTree(
    conversion: Conversion,
    typeFrom: PyType,
    typeTo: PyType,
    convType: PyType,
  ): Option[TextTree[PyType]] = {
    conversion match {
      case _: Conversion.CustomConversionRequired => Some(q"self.register(required.${convType.name}(), $typeFrom, $typeTo)")
      case _: Conversion.CopyEnumByName | _: Conversion.CopyAdtBranchByName | _: Conversion.DtoConversion =>
        Some(q"self.register($convType.instance(), $typeFrom, $typeTo)")

      case _ => None
    }
  }

  private def genAbstractConversion(conversion: Conversion, convType: PyType): Option[TextTree[PyType]] = {
    conversion match {
      case _: Conversion.CustomConversionRequired =>
        Some(
          q"""@$pyAbstractMethod
             |def ${convType.name}(self):
             |    raise NotImplementedError
             |""".stripMargin
        )
      case _ => None
    }
  }

  private def genConversionTree(
    conversion: Conversion,
    typeFrom: PyType,
    typeTo: PyType,
    convType: PyType,
    meta: TextTree[PyType],
  ): Option[TextTree[PyValue]] = {
    conversion match {
      case c: Conversion.CopyEnumByName =>
        val mappingEntries = c.memberMapping.map {
          case (fromName, toName) =>
            s""""$fromName": "$toName""""
        }
        val mappedExpr =
          if (mappingEntries.isEmpty) {
            q"_from.name"
          } else {
            val mappingLiteral = s"{${mappingEntries.mkString(", ")}}"
            q"$mappingLiteral.get(_from.name, _from.name)"
          }
        Some(q"""class ${convType.name}($abstractConversion[$typeFrom, $typeTo]):
                |    @$pyOverride
                |    def do_convert(self, ctx, conversions, _from: $typeFrom) -> $typeTo:
                |        return $typeTo[$mappedExpr]
                |
                |    ${meta.shift(4).trim}
                |""".stripMargin)

      case c: Conversion.CopyAdtBranchByName =>
        val memberCases = c.oldDefn
          .dataMembers(srcDom)
          .map(tid => tid -> typeTranslator.asPyTypeVersioned(tid, srcDom, evolution, pyFileTools.definitionsBasePkg))
          .map {
            case (oldTypeId, oldType) =>
              val newTypeId = c.branchMapping.getOrElse(oldTypeId.name.name, oldTypeId)
              q"""case $oldType():
                 |    return ${transfer(TypeRef.Scalar(newTypeId), q"_from", Some(TypeRef.Scalar(oldTypeId)))}
                 |""".stripMargin
          }
        val defaultCase = q"""case other:
                             |    raise ValueError(f"Bad input: {other}")
                             |""".stripMargin

        val cases = memberCases :+ defaultCase

        Some(q"""class ${convType.name}($abstractConversion[$typeFrom, $typeTo]):
                |    @$pyOverride
                |    def do_convert(self, ctx, conversions: $baboonAbstractConversions, _from: $typeFrom) -> $typeTo:
                |        match _from:
                |            ${cases.joinN().shift(12).trim}
                |
                |    ${meta.shift(4).trim}
                |""".stripMargin)

      case c: Conversion.DtoConversion =>
        val dtoDefn = domain.defs.meta.nodes(c.targetTpe) match {
          case DomainMember.User(_, d: Typedef.Dto, _, _) => d
          case _                                          => throw new IllegalStateException("DTO expected")
        }
        val ops = c.ops.map(o => o.targetField -> o).toMap
        val assigns = dtoDefn.fields.map {
          field =>
            val op        = ops(field)
            val fieldName = field.name.name
            val fieldRef  = q"_from.$fieldName"
            val expr = op match {
              case o: FieldOp.Transfer => transfer(o.targetField.tpe, fieldRef)

              case o: FieldOp.InitializeWithDefault =>
                o.targetField.tpe match {
                  case TypeRef.Constructor(id, args) =>
                    id match {
                      case TypeId.Builtins.lst =>
                        q"$pyList[${typeTranslator.asPyRef(args.head, domain, evolution)}]()"
                      case TypeId.Builtins.set =>
                        q"$pySet[${typeTranslator.asPyRef(args.head, domain, evolution)}]()"
                      case TypeId.Builtins.map =>
                        q"$pyDict[${typeTranslator.asPyRef(args.head, domain, evolution)}, ${typeTranslator.asPyRef(args.last, domain, evolution)}]()"
                      case TypeId.Builtins.opt => q"None"
                      case _                   => throw new IllegalStateException(s"Unsupported constructor type: $id")
                    }
                  case _ => throw new IllegalStateException("Unsupported target field type")
                }

              case o: FieldOp.WrapIntoCollection =>
                o.newTpe.id match {
                  case TypeId.Builtins.opt => fieldRef
                  case TypeId.Builtins.set => q"{$fieldRef}"
                  case TypeId.Builtins.lst => q"[$fieldRef]"
                  case _                   => throw new Exception()
                }

              case o: FieldOp.ExpandPrecision => transfer(o.newTpe, q"$fieldRef")

              case o: FieldOp.SwapCollectionType => swapCollType(q"$fieldRef", o)

              case o: FieldOp.Rename =>
                val srcFieldRef = q"_from.${o.sourceFieldName.name}"
                transfer(o.targetField.tpe, srcFieldRef)

              case o: FieldOp.Redef =>
                val srcFieldRef = q"_from.${o.sourceFieldName.name}"
                o.modify match {
                  case m: FieldOp.WrapIntoCollection =>
                    m.newTpe.id match {
                      case TypeId.Builtins.opt => srcFieldRef
                      case TypeId.Builtins.set => q"{$srcFieldRef}"
                      case TypeId.Builtins.lst => q"[$srcFieldRef]"
                      case _                   => throw new IllegalStateException(s"Unsupported collection type: ${m.newTpe.id}")
                    }
                  case m: FieldOp.ExpandPrecision    => transfer(m.newTpe, srcFieldRef)
                  case m: FieldOp.SwapCollectionType => swapCollType(srcFieldRef, m)
                }
            }
            val fieldType = asVersionedIfUserTpe(field.tpe)
            q"${field.name.name.toLowerCase}: $fieldType = $expr"
        }
        val ctorArgs = dtoDefn.fields.map(f => q"${f.name.name.toLowerCase}")
        Some(q"""class ${convType.name}($abstractConversion[$typeFrom, $typeTo]):
                |    @$pyOverride
                |    def do_convert(self, ctx, conversions: $baboonAbstractConversions, _from: $typeFrom) -> $typeTo:
                |        ${assigns.join("\n").shift(8).trim}
                |        return $typeTo(
                |            ${ctorArgs.zip(dtoDefn.fields).map { case (a, f) => q"${f.name.name}=$a" }.join(",\n").shift(12).trim}
                |        )
                |
                |    ${meta.shift(4).trim}
                |""".stripMargin.trim)

      case _: Conversion.CustomConversionRequired =>
        Some(q"""class ${convType.name}($abstractConversion[$typeFrom, $typeTo]):
                |
                |    ${meta.shift(4).trim}
                |""".stripMargin)

      case _ => None
    }
  }

  private def swapCollType(fieldRef: TextTree[PyValue], op: FieldOp.SwapCollectionType): TextTree[PyValue] = {
    val TypeRef.Constructor(oldId, oldArgs) = op.oldTpe
    val TypeRef.Constructor(newId, newArgs) = op.newTpe
    val tmp                                 = q"v"

    (oldId, newId) match {
      case (TypeId.Builtins.opt, TypeId.Builtins.lst) =>
        q"[] if $fieldRef is None else [${transfer(newArgs.head, fieldRef, Some(oldArgs.head))}]"
      case (TypeId.Builtins.opt, TypeId.Builtins.set) =>
        q"{} if $fieldRef is None else {${transfer(newArgs.head, fieldRef, Some(oldArgs.head))}}"
      case (TypeId.Builtins.opt, TypeId.Builtins.opt) =>
        q"None if $fieldRef is None else ${transfer(newArgs.head, fieldRef, Some(oldArgs.head))}"

      case (TypeId.Builtins.lst, TypeId.Builtins.lst) =>
        q"[${transfer(newArgs.head, tmp, Some(oldArgs.head))} for v in range(len($fieldRef))]"
      case (TypeId.Builtins.lst, TypeId.Builtins.set) =>
        q"{${transfer(newArgs.head, tmp, Some(oldArgs.head))} for v in range(len($fieldRef))}"

      case (TypeId.Builtins.set, TypeId.Builtins.lst) =>
        q"[${transfer(newArgs.head, tmp, Some(oldArgs.head))} for v in range(len($fieldRef))]"
      case (TypeId.Builtins.set, TypeId.Builtins.set) =>
        q"{${transfer(newArgs.head, tmp, Some(oldArgs.head))} for v in range(len($fieldRef))}"

      case (TypeId.Builtins.map, TypeId.Builtins.map) =>
        val keyRef = q"k"
        val valRef = q"v"
        q"{${transfer(newArgs.head, keyRef, Some(oldArgs.head))}: ${transfer(newArgs.last, valRef, Some(oldArgs.last))} for k,v in $fieldRef.items()}"
      case _ =>
        throw new IllegalStateException("Unsupported collection swap")
    }
  }

  private def transfer(
    newTpe: TypeRef,
    oldRef: TextTree[PyValue],
    maybeOldTpe: Option[TypeRef] = None,
  ): TextTree[PyValue] = {
    val oldTpe         = maybeOldTpe.getOrElse(newTpe)
    val newTypeRefTree = typeTranslator.asPyRef(newTpe, domain, evolution, pyFileTools.definitionsBasePkg)
    val oldTypeRefTree = asVersionedIfUserTpe(oldTpe)

    (newTpe, oldTpe) match {
      case (c: TypeRef.Constructor, s: TypeRef.Scalar) =>
        val headTransfer = transfer(c.args.head, oldRef, Some(s))
        c.id match {
          case TypeId.Builtins.opt => q"$headTransfer"
          case TypeId.Builtins.lst => q"[$headTransfer]"
          case TypeId.Builtins.set => q"{$headTransfer}"
          case _                   => throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
        }

      case (ns: TypeRef.Scalar, os: TypeRef.Scalar) =>
        transferScalar(oldRef, newTypeRefTree, oldTypeRefTree, ns, os)
      case (TypeRef.Scalar(_), c: TypeRef.Constructor) =>
        throw new IllegalStateException(s"Unsupported scalar to constructor conversion: ${c.id}")
      case (cn: TypeRef.Constructor, co: TypeRef.Constructor) =>
        transferConstructor(oldRef, cn, co)

    }
  }

  private def transferConstructor(
    oldRef: TextTree[PyValue],
    cn: TypeRef.Constructor,
    co: TypeRef.Constructor,
  ): TextTree[PyValue] = {
    val tmp = q"v"
    cn match {
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.lst =>
        q"[${transfer(c.args.head, tmp, Some(co.args.head))} for v in $oldRef]"

      case c: TypeRef.Constructor if c.id == TypeId.Builtins.map =>
        val keyRef   = c.args.head
        val valueRef = c.args.last
        val kv       = q"k"
        val vv       = q"v"

        q"{${transfer(keyRef, kv, Some(co.args.head))}: ${transfer(valueRef, vv, Some(co.args.last))} for k,v in $oldRef.items()}"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.set =>
        q"{${transfer(c.args.head, tmp, Some(co.args.head))} for v in $oldRef}"
      case c: TypeRef.Constructor if c.id == TypeId.Builtins.opt =>
        q"None if $oldRef is None else ${transfer(c.args.head, oldRef, Some(co.args.head))}"
      case c => throw new IllegalStateException(s"Unsupported constructor type: ${c.id}")
    }
  }

  private def transferScalar(
    oldRef: TextTree[PyValue],
    newTypeRefTree: TextTree[PyValue],
    oldTypeRefTree: TextTree[PyValue],
    newScalar: TypeRef.Scalar,
    oldScalar: TypeRef.Scalar,
  ): TextTree[PyValue] = {
    val direct = if (newScalar == oldScalar) oldRef else q"$newTypeRefTree($oldRef)"
    val conv   = q"conversions.convert_by_type(ctx, $oldRef, $oldTypeRefTree, $newTypeRefTree)"

    newScalar.id match {
      case _: TypeId.Builtin => direct
      case id: TypeId.User =>
        domain.defs.meta.nodes(id) match {
          case DomainMember.User(_, _: Typedef.Foreign, _, _) => direct
          case _                                              => conv
        }
    }
  }

  private def asVersionedIfUserTpe(typeRef: TypeRef) = {
    typeRef.id match {
      case u: TypeId.User => typeTranslator.asPyTypeVersioned(u, srcDom, evolution, pyFileTools.definitionsBasePkg).pipe(t => q"$t")
      case _              => typeTranslator.asPyRef(typeRef, domain, evolution, pyFileTools.definitionsBasePkg)
    }
  }

  private def makeName(conversion: Conversion): String = {
    val nameParts =
      Seq("Convert") ++
      conversion.sourceTpe.owner.asPseudoPkg ++
      Seq(conversion.sourceTpe.name.name.capitalize, "From", sourceVersionString)

    nameParts.mkString("__")
  }

  private def conversionType(conversion: Conversion): PyType = {
    val className = makeName(conversion)

    val moduleName = Seq(
      "from",
      sourceVersionString,
      conversion.sourceTpe.owner.asPseudoPkg.mkString("_"),
      s"${conversion.sourceTpe.name.name.capitalize}",
    ).mkString("_")

    val versionPathPart = if (domain.version == evolution.latest) Nil else List(currentVersionString)
    val convModuleId = PyModuleId(
      NEList.unsafeFrom(
        pyFileTools.definitionsBasePkg ++
        domain.id.path ++
        versionPathPart :+ moduleName
      )
    )
    PyType(convModuleId, className)
  }
}

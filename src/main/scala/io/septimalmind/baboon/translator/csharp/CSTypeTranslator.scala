package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType, CSTypeName}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSTypeTranslator(target: CSTarget) {

  def toCsPkg(p: Pkg, version: Version, evolution: BaboonEvolution): CSPackageId = {
    toCsPkg(
      p,
      version,
      target.language.omitMostRecentVersionSuffixFromNamespaces && version == evolution.latest,
    )
  }

  def toCsPkg(p: Pkg, version: Version, omitVersion: Boolean): CSPackageId = {
    val verString = "v" + version.version
      .split('.')
      .mkString("_")

    val base = p.path.map(_.capitalize)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    CSPackageId(segments)
  }

  def toCsTypeRefDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): CSType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        val fe    = defn.bindings("cs")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        CSType(CSPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        toCsTypeRefNoDeref(tid, domain, evolution)
    }
  }

  def toCsTypeRefNoDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): CSType = {
    val version = domain.version
    val pkg     = toCsPkg(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val fullPrefix    = pkg.parts ++ ownerAsPrefix

    val fullPkg = tid.owner match {
      case Owner.Adt(_) =>
        val static = if (target.language.useCompactAdtForm) true else false
        CSPackageId(pkg.parts ++ ownerAsPrefix, isStatic = static)
      case _ =>
        CSPackageId(fullPrefix)
    }
    CSType(fullPkg, tid.name.name.capitalize, fq = false)
  }

  def adtNsName(id: TypeId.User): String = {
    if (target.language.useCompactAdtForm) {
      id.name.name
    } else {
      id.name.name.toLowerCase
    }
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel =>
        Seq.empty
      case Owner.Ns(path) =>
        path.map(_.name)
      case Owner.Adt(id) =>
        val sub = renderOwner(id.owner) :+ adtNsName(id)
        sub
    }
  }

  private def asCsTypeScalar(scalar: TypeId.BuiltinScalar): CSType = scalar match {
    case TypeId.Builtins.bit => CSTypes.csBoolean

    case TypeId.Builtins.i08 => CSTypes.csSByte
    case TypeId.Builtins.i16 => CSTypes.csInt16
    case TypeId.Builtins.i32 => CSTypes.csInt32
    case TypeId.Builtins.i64 => CSTypes.csInt64

    case TypeId.Builtins.u08 => CSTypes.csByte
    case TypeId.Builtins.u16 => CSTypes.csUInt16
    case TypeId.Builtins.u32 => CSTypes.csUInt32
    case TypeId.Builtins.u64 => CSTypes.csUInt64

    case TypeId.Builtins.f32  => CSTypes.csSingle
    case TypeId.Builtins.f64  => CSTypes.csDouble
    case TypeId.Builtins.f128 => CSTypes.csDecimal

    case TypeId.Builtins.str                       => CSTypes.csString
    case TypeId.Builtins.uid                       => CSTypes.csGuid
    case TypeId.Builtins.tso | TypeId.Builtins.tsu => CSTypes.rpDateTime

    case other => throw new IllegalArgumentException(s"Unexpected: $other")
  }

  def asCsType(tpe: TypeId, domain: Domain, evolution: BaboonEvolution, mut: Boolean = false): CSType = tpe match {
    case scalar: TypeId.BuiltinScalar => asCsTypeScalar(scalar)

    case TypeId.Builtins.map if mut => CSTypes.csDictionary
    case TypeId.Builtins.map        => CSTypes.csIReadOnlyDictionary

    case TypeId.Builtins.lst if mut => CSTypes.csList
    case TypeId.Builtins.lst        => CSTypes.csIReadOnlyList

    case TypeId.Builtins.set if mut => CSTypes.csSet
    case TypeId.Builtins.set        => CSTypes.csImmutableHashSet // IReadonlySet not available on netstandard2.1

    case user: TypeId.User => toCsTypeRefDeref(user, domain, evolution)

    case other => throw new IllegalArgumentException(s"Unexpected: $other")
  }

  def asCsRef(tpe: TypeRef, domain: Domain, evolution: BaboonEvolution, fullyQualified: Boolean = false, mut: Boolean = false): TextTree[CSValue] = {
    val out = tpe match {
      case TypeRef.Scalar(id) =>
        q"${asCsType(id, domain, evolution, mut)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        q"${asCsRef(args.head, domain, evolution)}?"

      case TypeRef.Constructor(id, args) =>
        val tpe   = asCsType(id, domain, evolution, mut)
        val targs = args.map(asCsRef(_, domain, evolution))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }

    if (fullyQualified) {
      out.map {
        case t: CSType     => t.fullyQualified
        case n: CSTypeName => n
      }
    } else {
      out
    }
  }

  def deNull(tpe: TypeRef, domain: Domain, ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.str)     => ref
      case TypeRef.Scalar(_: TypeId.BuiltinScalar) => q"$ref.Value"
      case _ if isEnum(tpe, domain)                => q"$ref.Value"
      case _ if isCSValueType(tpe, domain)         => q"$ref.Value"
      case _                                       => q"$ref!"
    }
  }

  def isCSValueType(tpe: TypeRef, domain: Domain): Boolean = {
    // TODO: c# rules are complex, probably we have some issues here
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit  => true
              case TypeId.Builtins.i08  => true
              case TypeId.Builtins.i16  => true
              case TypeId.Builtins.i32  => true
              case TypeId.Builtins.i64  => true
              case TypeId.Builtins.u08  => true
              case TypeId.Builtins.u16  => true
              case TypeId.Builtins.u32  => true
              case TypeId.Builtins.u64  => true
              case TypeId.Builtins.f32  => true
              case TypeId.Builtins.f64  => true
              case TypeId.Builtins.f128 => true
              case TypeId.Builtins.uid  => true
              case TypeId.Builtins.tsu  => true
              case TypeId.Builtins.tso  => true
              case _                    => false
            }
          case _ =>
            isEnum(tpe, domain) || foreignTypeIsValueType(id, domain)
        }
      case _ =>
        foreignTypeIsValueType(tpe.id, domain)
    }
  }

  private def foreignTypeIsValueType(id: TypeId, domain: Domain): Boolean = {
    domain.defs.meta.nodes(id) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        defn
          .bindings("cs").attrs.attrs
          .find(_.name == "value-type")
          .exists(a => a.value.toLowerCase == "yes" || a.value.toLowerCase == "true")
      case _ =>
        false
    }
  }

  def isEnum(tpe: TypeRef, domain: Domain): Boolean = {
    domain.defs.meta.nodes.get(tpe.id).exists {
      case DomainMember.User(_, _: Typedef.Enum, _) => true
      case _                                        => false
    }
  }
}

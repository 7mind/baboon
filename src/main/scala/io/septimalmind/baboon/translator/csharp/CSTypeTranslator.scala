package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType, CSTypeOrigin}
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSTypeTranslator(target: CSTarget, enquiries: BaboonEnquiries, info: CSTypeInfo) {
  def asCsRef(tpe: TypeRef, domain: Domain, evolution: BaboonEvolution, mutableCollections: Boolean = false): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asCsType(id, domain, evolution, mutableCollections)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        q"${asCsRef(args.head, domain, evolution)}?"

      case TypeRef.Constructor(id, args) =>
        val tpe   = asCsType(id, domain, evolution, mutableCollections)
        val targs = args.map(asCsRef(_, domain, evolution))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }
  }

  def asCsType(tpe: TypeId, domain: Domain, evolution: BaboonEvolution, mutableCollections: Boolean = false): CSType = tpe match {
    case scalar: TypeId.BuiltinScalar =>
      scalar match {
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

    case TypeId.Builtins.map if mutableCollections => CSTypes.csDictionary
    case TypeId.Builtins.map                       => CSTypes.csIReadOnlyDictionary

    case TypeId.Builtins.lst if mutableCollections => CSTypes.csList
    case TypeId.Builtins.lst                       => CSTypes.csIReadOnlyList

    case TypeId.Builtins.set if mutableCollections => CSTypes.csSet
    case TypeId.Builtins.set                       => CSTypes.csImmutableHashSet // IReadonlySet not available on netstandard2.1

    case user: TypeId.User => asCsTypeDerefForeigns(user, domain, evolution)

    case other => throw new IllegalArgumentException(s"Unexpected: $other")
  }

  private def asCsTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): CSType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("cs")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        CSType(CSPackageId(NEList.unsafeFrom(pkg)), id, fq = false, CSTypeOrigin.TypeInDomain(tid, domain.id, domain.version))
      case _ =>
        asCsTypeKeepForeigns(tid, domain, evolution)
    }
  }

  def asCsTypeKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): CSType = {
    val version = domain.version
    val pkg     = toCsPkg(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val fullPrefix    = pkg.parts ++ ownerAsPrefix

    val fullPkg = tid.owner match {
      case Owner.Adt(_) =>
        CSPackageId(pkg.parts ++ ownerAsPrefix, isStatic = true)
      case _ =>
        CSPackageId(fullPrefix)
    }
    CSType(fullPkg, tid.name.name.capitalize, fq = false, CSTypeOrigin.TypeInDomain(tid, domain.id, domain.version))
  }

  def toCsPkg(p: Pkg, version: Version, evolution: BaboonEvolution): CSPackageId = {
    toCsPkg(
      p,
      version,
      target.language.omitMostRecentVersionSuffixFromNamespaces && version == evolution.latest,
    )
  }

  private def toCsPkg(p: Pkg, version: Version, omitVersion: Boolean): CSPackageId = {
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

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel =>
        Seq.empty
      case Owner.Ns(path) =>
        path.map(_.name)
      case Owner.Adt(id) =>
        val sub = renderOwner(id.owner) :+ info.adtNsName(id)
        sub
    }
  }

  def deNull(tpe: TypeRef, domain: Domain, ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.str)     => ref
      case TypeRef.Scalar(_: TypeId.BuiltinScalar) => q"$ref.Value"
      case _ if enquiries.isEnum(tpe, domain)      => q"$ref.Value"
      case _ if info.isCSValueType(tpe, domain)    => q"$ref.Value"
      case _                                       => q"$ref!"
    }
  }

}

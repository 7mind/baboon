package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.csharp.CSTypes
import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class ScTypeTranslator(
  target: ScTarget,
  domain: Domain,
  evo: BaboonEvolution,
) {

  def asScRef(tpe: TypeRef, anotherDom: Domain, fullyQualified: Boolean): TextTree[ScValue] = {
    ???
  }

  def asScRef(tpe: TypeRef): TextTree[ScValue] = tpe match {
    case TypeRef.Scalar(id: TypeId.User) =>
      q"${toScTypeRefDeref(id, domain, evo)}"
    case TypeRef.Scalar(b: TypeId.BuiltinScalar) =>
      b match {
        case TypeId.Builtins.i08 | TypeId.Builtins.u08 => q"$scByte"
        case TypeId.Builtins.i16 | TypeId.Builtins.u16 => q"$scShort"
        case TypeId.Builtins.i32 | TypeId.Builtins.u32 => q"$scInt"
        case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"$scLong"
        case TypeId.Builtins.f32                       => q"$scFloat"
        case TypeId.Builtins.f64                       => q"$scDouble"
        case TypeId.Builtins.f128                      => q"$scBigDecimal"
        case TypeId.Builtins.str                       => q"$scString"
        case TypeId.Builtins.uid                       => q"$scUid"
        case TypeId.Builtins.tso | TypeId.Builtins.tsu => q"$scTime"
        case TypeId.Builtins.bit                       => q"$scBoolean"

        case other => throw new IllegalArgumentException(s"Unexpected: $other")
      }
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
      q"$scOption[${asScRef(args.head)}]"
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.lst =>
      q"$scList[${args.map(asScRef).toSeq.join(", ")}]"
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.set =>
      q"$scSet[${asScRef(args.head)}]"
    case TypeRef.Constructor(id, args) if id == TypeId.Builtins.map =>
      q"$scMap[${asScRef(args.head)}, ${asScRef(args.tail.head)}]"
    case other =>
      other.id match {
        case uid: TypeId.User => q"${toScTypeRefDeref(uid, domain, evo)}"
        case _                => q"${other.toString}"
      }
  }

  def toScPkg(p: Pkg, version: Version, evolution: BaboonEvolution): ScPackageId = {
    toScPkg(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }
  def toScPkg(p: Pkg, version: Version, omitVersion: Boolean): ScPackageId = {
    val verString = "v" + version.version
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    ScPackageId(segments)
  }

  def toScTypeRefDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        val fe    = defn.bindings("scala")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        ScType(ScPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        toScTypeRefNoDeref(tid, domain, evolution)
    }
  }

  def toScTypeRefNoDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
    val version = domain.version
    val pkg     = toScPkg(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val fullPrefix    = pkg.parts ++ ownerAsPrefix

    val fullPkg = tid.owner match {
      case Owner.Adt(_) =>
        ScPackageId(pkg.parts ++ ownerAsPrefix)
      case _ =>
        ScPackageId(fullPrefix)
    }
    ScType(fullPkg, tid.name.name.capitalize, fq = false)
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

  def adtNsName(id: TypeId.User): String = {
    id.name.name
  }
}

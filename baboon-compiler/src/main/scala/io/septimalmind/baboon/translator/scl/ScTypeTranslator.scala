package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class ScTypeTranslator(
  scTypeInfo: ScTypeInfo
) {
  def asScRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[ScValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asScType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) =>
        val tpe   = asScType(id, domain, evo)
        val targs = args.map(asScRef(_, domain, evo))
        q"$tpe[${targs.toSeq.join(", ")}]"
    }
  }

  def asScType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): ScType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i08 | TypeId.Builtins.u08 => scByte
          case TypeId.Builtins.i16 | TypeId.Builtins.u16 => scShort
          case TypeId.Builtins.i32 | TypeId.Builtins.u32 => scInt
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => scLong
          case TypeId.Builtins.f32                       => scFloat
          case TypeId.Builtins.f64                       => scDouble
          case TypeId.Builtins.f128                      => scBigDecimal
          case TypeId.Builtins.str                       => scString
          case TypeId.Builtins.bytes                     => scByteString
          case TypeId.Builtins.uid                       => scUid
          case TypeId.Builtins.tso | TypeId.Builtins.tsu => scTime
          case TypeId.Builtins.bit                       => scBoolean

          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => ScTypes.scMap
      case TypeId.Builtins.lst => ScTypes.scList
      case TypeId.Builtins.set => ScTypes.scSet
      case TypeId.Builtins.opt => ScTypes.scOption
      case uid: TypeId.User    => asScTypeDerefForeigns(uid, domain, evo)
      case other               => throw new IllegalArgumentException(s"Unexpected: $other")
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

  private def asScTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("scala")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        ScType(ScPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        toScTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toScTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
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
        path.map(_.name.toLowerCase)
      case Owner.Adt(id) =>
        val sub = renderOwner(id.owner) :+ scTypeInfo.adtNsName(id)
        sub
    }
  }
}

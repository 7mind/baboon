package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.translator.java.JvValue.{JvPackageId, JvType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class JvTypeTranslator {
  def asJvRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[JvValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asJvType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        val inner = asJvBoxedRef(args.head, domain, evo)
        q"$jvOptional<$inner>"
      case TypeRef.Constructor(id, args) =>
        val tpe   = asJvType(id, domain, evo)
        val targs = args.map(asJvBoxedRef(_, domain, evo))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }
  }

  /** Like asJvRef but always uses boxed types for primitives (needed inside generics). */
  def asJvBoxedRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[JvValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asJvBoxedType(id, domain, evo)}"
      case other =>
        asJvRef(other, domain, evo)
    }
  }

  def asJvType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): JvType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit  => jvBoolean
          case TypeId.Builtins.i08  => jvByte
          case TypeId.Builtins.i16  => jvShort
          case TypeId.Builtins.i32  => jvInt
          case TypeId.Builtins.i64  => jvLong
          case TypeId.Builtins.u08  => jvShort   // next-wider signed
          case TypeId.Builtins.u16  => jvInt     // next-wider signed
          case TypeId.Builtins.u32  => jvLong    // next-wider signed
          case TypeId.Builtins.u64  => jvLong    // same width, use unsigned utility methods
          case TypeId.Builtins.f32  => jvFloat
          case TypeId.Builtins.f64  => jvDouble
          case TypeId.Builtins.f128 => jvBigDecimal
          case TypeId.Builtins.str  => jvString
          case TypeId.Builtins.bytes => jvByteString
          case TypeId.Builtins.uid   => jvUid
          case TypeId.Builtins.tso | TypeId.Builtins.tsu => jvOffsetDateTime
          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => jvMap
      case TypeId.Builtins.lst => jvList
      case TypeId.Builtins.set => jvSet
      case TypeId.Builtins.opt =>
        throw new IllegalArgumentException("opt should be handled via Optional<T>, not as a raw type constructor")
      case uid: TypeId.User => asJvTypeDerefForeigns(uid, domain, evo)
      case other            => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  /** Returns the boxed version of a type (for use inside generics like Optional<Integer>). */
  def asJvBoxedType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): JvType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.bit  => jvBoxedBoolean
          case TypeId.Builtins.i08  => jvBoxedByte
          case TypeId.Builtins.i16  => jvBoxedShort
          case TypeId.Builtins.i32  => jvBoxedInteger
          case TypeId.Builtins.i64  => jvBoxedLong
          case TypeId.Builtins.u08  => jvBoxedShort
          case TypeId.Builtins.u16  => jvBoxedInteger
          case TypeId.Builtins.u32  => jvBoxedLong
          case TypeId.Builtins.u64  => jvBoxedLong
          case TypeId.Builtins.f32  => jvBoxedFloat
          case TypeId.Builtins.f64  => jvBoxedDouble
          case other                => asJvType(tpe, domain, evo) // non-primitive builtins are already boxed
        }
      case _ => asJvType(tpe, domain, evo)
    }
  }

  def toJvPkg(p: Pkg, version: Version, evolution: BaboonEvolution): JvPackageId = {
    toJvPkg(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }

  def toJvPkg(p: Pkg, version: Version, omitVersion: Boolean): JvPackageId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    JvPackageId(segments)
  }

  private def asJvTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): JvType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("java")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        JvType(JvPackageId(NEList.unsafeFrom(pkg)), id)
      case _ =>
        toJvTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toJvTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): JvType = {
    val version = domain.version
    val pkg     = toJvPkg(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val fullPrefix    = pkg.parts ++ ownerAsPrefix

    val fullPkg = tid.owner match {
      case Owner.Adt(_) =>
        JvPackageId(pkg.parts ++ ownerAsPrefix)
      case _ =>
        JvPackageId(fullPrefix)
    }
    JvType(fullPkg, tid.name.name.capitalize)
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name
    }
  }

  def effectiveJvPkg(owner: Owner, domain: Domain, evo: BaboonEvolution): JvPackageId = {
    val basePkg = toJvPkg(domain.id, domain.version, evo)
    owner match {
      case Owner.Toplevel  => basePkg
      case Owner.Ns(path)  => JvPackageId(NEList.unsafeFrom(basePkg.parts.toList ++ path.map(_.name.toLowerCase)))
      case Owner.Adt(id)   => effectiveJvPkg(id.owner, domain, evo)
    }
  }
}

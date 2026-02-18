package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.translator.kotlin.KtValue.{KtPackageId, KtType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

class KtTypeTranslator {
  def asKtRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[KtValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asKtType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) if id == TypeId.Builtins.opt =>
        val inner = asKtRef(args.head, domain, evo)
        q"$inner?"
      case TypeRef.Constructor(id, args) =>
        val tpe   = asKtType(id, domain, evo)
        val targs = args.map(asKtRef(_, domain, evo))
        q"$tpe<${targs.toSeq.join(", ")}>"
    }
  }

  def asKtNullableRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[KtValue] = {
    tpe match {
      case TypeRef.Constructor(id, args) if id.name.name == "opt" =>
        val inner = asKtRef(args.head, domain, evo)
        q"$inner?"
      case other =>
        asKtRef(other, domain, evo)
    }
  }

  def asKtType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): KtType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i08                       => ktByte
          case TypeId.Builtins.u08                       => ktUByte
          case TypeId.Builtins.i16                       => ktShort
          case TypeId.Builtins.u16                       => ktUShort
          case TypeId.Builtins.i32                       => ktInt
          case TypeId.Builtins.u32                       => ktUInt
          case TypeId.Builtins.i64                       => ktLong
          case TypeId.Builtins.u64                       => ktULong
          case TypeId.Builtins.f32                       => ktFloat
          case TypeId.Builtins.f64                       => ktDouble
          case TypeId.Builtins.f128                      => ktBigDecimal
          case TypeId.Builtins.str                       => ktString
          case TypeId.Builtins.bytes                     => ktByteString
          case TypeId.Builtins.uid                       => ktUid
          case TypeId.Builtins.tso | TypeId.Builtins.tsu => ktTime
          case TypeId.Builtins.bit                       => ktBoolean

          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => ktMap
      case TypeId.Builtins.lst => ktList
      case TypeId.Builtins.set => ktSet
      case TypeId.Builtins.opt =>
        throw new IllegalArgumentException("opt should be handled as nullable in Kotlin, not as a type constructor")
      case uid: TypeId.User => asKtTypeDerefForeigns(uid, domain, evo)
      case other            => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  def toKtPkg(p: Pkg, version: Version, evolution: BaboonEvolution): KtPackageId = {
    toKtPkg(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }

  def toKtPkg(p: Pkg, version: Version, omitVersion: Boolean): KtPackageId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    KtPackageId(segments)
  }

  private def asKtTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): KtType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        val fe    = defn.bindings("kotlin")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        KtType(KtPackageId(NEList.unsafeFrom(pkg)), id)
      case _ =>
        toKtTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toKtTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): KtType = {
    val version = domain.version
    val pkg     = toKtPkg(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)
    val fullPrefix    = pkg.parts ++ ownerAsPrefix

    val fullPkg = tid.owner match {
      case Owner.Adt(_) =>
        KtPackageId(pkg.parts ++ ownerAsPrefix)
      case _ =>
        KtPackageId(fullPrefix)
    }
    KtType(fullPkg, tid.name.name.capitalize)
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(_.name.toLowerCase)
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name
    }
  }
}

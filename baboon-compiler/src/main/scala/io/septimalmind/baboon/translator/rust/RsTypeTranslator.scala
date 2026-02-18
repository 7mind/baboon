package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{escapeRustKeyword, toSnakeCaseRaw}
import io.septimalmind.baboon.translator.rust.RsTypes.*
import io.septimalmind.baboon.translator.rust.RsValue.{RsCrateId, RsType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

object RsTypeTranslator {
  sealed trait HexSerdeKind
  object HexSerdeKind {
    case object Direct extends HexSerdeKind
    case object Optional extends HexSerdeKind
  }
}

class RsTypeTranslator {
  import RsTypeTranslator.HexSerdeKind

  def asRsRef(tpe: TypeRef, domain: Domain, evo: BaboonEvolution): TextTree[RsValue] = {
    tpe match {
      case TypeRef.Scalar(uid: TypeId.User) =>
        domain.defs.meta.nodes(uid) match {
          case DomainMember.User(_, f: Typedef.Foreign, _, _) =>
            f.bindings.get(BaboonLang.Rust) match {
              case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.BaboonRef(aliasedRef))) =>
                asRsRef(aliasedRef, domain, evo)
              case _ =>
                q"${asRsType(uid, domain, evo)}"
            }
          case _ =>
            q"${asRsType(uid, domain, evo)}"
        }

      case TypeRef.Scalar(id) =>
        q"${asRsType(id, domain, evo)}"

      case TypeRef.Constructor(id, args) =>
        id match {
          case TypeId.Builtins.opt =>
            val inner = asRsRef(args.head, domain, evo)
            q"Option<$inner>"
          case TypeId.Builtins.lst =>
            val inner = asRsRef(args.head, domain, evo)
            q"Vec<$inner>"
          case TypeId.Builtins.set =>
            val inner = asRsRef(args.head, domain, evo)
            q"$rsBTreeSet<$inner>"
          case TypeId.Builtins.map =>
            val key   = asRsRef(args.head, domain, evo)
            val value = asRsRef(args.last, domain, evo)
            q"$rsBTreeMap<$key, $value>"
          case o => throw new IllegalArgumentException(s"Unexpected collection type: $o")
        }
    }
  }

  def asRsType(tpe: TypeId, domain: Domain, evo: BaboonEvolution): RsType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i08   => rsI8
          case TypeId.Builtins.i16   => rsI16
          case TypeId.Builtins.i32   => rsI32
          case TypeId.Builtins.i64   => rsI64
          case TypeId.Builtins.u08   => rsU8
          case TypeId.Builtins.u16   => rsU16
          case TypeId.Builtins.u32   => rsU32
          case TypeId.Builtins.u64   => rsU64
          case TypeId.Builtins.f32   => rsF32
          case TypeId.Builtins.f64   => rsF64
          case TypeId.Builtins.f128  => rsDecimal
          case TypeId.Builtins.str   => rsString
          case TypeId.Builtins.bytes =>
            // Vec<u8> - but we handle serde annotation at field level
            RsType(stdCrate, "Vec<u8>", predef = true)
          case TypeId.Builtins.uid => rsUuid
          case TypeId.Builtins.tsu =>
            RsType(chronoCrate, "DateTime<chrono::Utc>", fq = true)
          case TypeId.Builtins.tso =>
            RsType(chronoCrate, "DateTime<chrono::FixedOffset>", fq = true)
          case TypeId.Builtins.bit => rsBool
          case other               => throw new IllegalArgumentException(s"Unexpected builtin scalar: $other")
        }
      case TypeId.Builtins.map => rsBTreeMap
      case TypeId.Builtins.lst => RsType(stdCrate, "Vec", predef = true)
      case TypeId.Builtins.set => rsBTreeSet
      case TypeId.Builtins.opt => rsOption
      case uid: TypeId.User    => asRsTypeDerefForeigns(uid, domain, evo)
      case other               => throw new IllegalArgumentException(s"Unexpected type: $other")
    }
  }

  def toRsCrate(p: Pkg, version: Version, evolution: BaboonEvolution): RsCrateId = {
    toRsCrate(
      p,
      version,
      omitVersion = version == evolution.latest,
    )
  }

  def toRsCrate(p: Pkg, version: Version, omitVersion: Boolean): RsCrateId = {
    val verString = "v" + version.v.toString
      .split('.')
      .mkString("_")

    val base = p.path.map(_.toLowerCase)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    RsCrateId(NEList.unsafeFrom(("crate" +: segments).toList))
  }

  private def asRsTypeDerefForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): RsType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _) =>
        defn.bindings.get(BaboonLang.Rust) match {
          case Some(Typedef.ForeignEntry(_, Typedef.ForeignMapping.Custom(decl, _))) =>
            val parts = decl.split("::").toList
            assert(parts.length > 1)
            val pkg = parts.init
            val id  = parts.last
            RsType(RsCrateId(NEList.unsafeFrom(pkg)), id)
          case _ =>
            toRsTypeRefKeepForeigns(tid, domain, evolution)
        }
      case _ =>
        toRsTypeRefKeepForeigns(tid, domain, evolution)
    }
  }

  def toRsTypeRefKeepForeigns(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): RsType = {
    val version = domain.version
    val crate   = toRsCrate(tid.pkg, version, evolution)

    val ownerAsPrefix = renderOwner(tid.owner)

    val fullCrate = tid.owner match {
      case Owner.Adt(_) =>
        // ADT branches are defined inline in the ADT file, so the path is just the ADT's module
        RsCrateId(NEList.unsafeFrom((crate.parts ++ ownerAsPrefix).toList))
      case _ =>
        // Each type has its own file/module, so add the module name (snake_case of type name)
        val moduleName = escapeRustKeyword(toSnakeCaseRaw(tid.name.name))
        RsCrateId(NEList.unsafeFrom((crate.parts ++ ownerAsPrefix :+ moduleName).toList))
    }
    RsType(fullCrate, tid.name.name.capitalize)
  }

  private def renderOwner(owner: Owner): Seq[String] = {
    owner match {
      case Owner.Toplevel => Seq.empty
      case Owner.Ns(path) => path.map(s => escapeRustKeyword(s.name.toLowerCase))
      case Owner.Adt(id)  => renderOwner(id.owner) :+ escapeRustKeyword(toSnakeCaseRaw(id.name.name))
    }
  }

  /** Check if a TypeRef contains bytes (Vec<u8>) that needs hex serde */
  def needsHexSerde(tpe: TypeRef): Option[HexSerdeKind] = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.bytes) => Some(HexSerdeKind.Direct)
      case TypeRef.Constructor(TypeId.Builtins.opt, args) if args.exists {
            case TypeRef.Scalar(TypeId.Builtins.bytes) => true
            case _                                     => false
          } =>
        Some(HexSerdeKind.Optional)
      case _ => None
    }
  }

  /** Check if a TypeRef contains Decimal that needs number serde */
  def needsDecimalSerde(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.f128) => true
      case _                                    => false
    }
  }

  /** Check if a TypeRef contains i64/u64 that needs lenient deserialization (accepts both numbers and strings) */
  def needsLenientSerde(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.i64) => true
      case TypeRef.Scalar(TypeId.Builtins.u64) => true
      case TypeRef.Constructor(_, args)        => args.exists(needsLenientSerde)
      case _                                   => false
    }
  }

  /** Check if a TypeRef is a timestamp type */
  def isTimestamp(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.tsu) | TypeRef.Scalar(TypeId.Builtins.tso) => true
      case _                                                                         => false
    }
  }

  /** Check if a TypeRef is tsu */
  def isTsu(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(TypeId.Builtins.tsu) => true
      case _                                   => false
    }
  }
}

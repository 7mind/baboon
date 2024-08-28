package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.BaboonCompiler.CompilerOptions
import io.septimalmind.baboon.translator.csharp.CSBaboonTranslator.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

class CSTypeTranslator() {

  def toCsPkg(p: Pkg,
              version: Version,
              evolution: BaboonEvolution,
              options: CompilerOptions): CSPackageId = {
    toCsPkg(
      p,
      version,
      options.omitMostRecentVersionSuffixFromNamespaces && version == evolution.latest
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

  def adtNsName(id: TypeId.User): String = {
    id.name.name.toLowerCase
  }

  def toCsTypeRefDeref(tid: TypeId.User,
                       domain: Domain,
                       evolution: BaboonEvolution,
                       options: CompilerOptions): CSType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        val fid = defn.bindings("cs")
        val parts = fid.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id = parts.last
        CSType(CSPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        toCsTypeRefNoDeref(tid, domain, evolution, options)
    }
  }

  def toCsTypeRefNoDeref(tid: TypeId.User,
                         domain: Domain,
                         evolution: BaboonEvolution,
                         options: CompilerOptions): CSType = {
    val version = domain.version
    val pkg = toCsPkg(tid.pkg, version, evolution, options)
    val fullPkg = tid.owner match {
      case Owner.Toplevel => pkg
      case Owner.Adt(id)  => CSPackageId(pkg.parts :+ adtNsName(id))
    }
    CSType(fullPkg, tid.name.name.capitalize, fq = false)
  }

  private def asCsTypeScalar(b: TypeId.BuiltinScalar) = {
    b match {
      case TypeId.Builtins.bit =>
        CSValue.CSType(csSystemPkg, "Boolean", fq = false)

      case TypeId.Builtins.i08 =>
        CSValue.CSType(csSystemPkg, "SByte", fq = false)
      case TypeId.Builtins.i16 =>
        CSValue.CSType(csSystemPkg, "Int16", fq = false)
      case TypeId.Builtins.i32 =>
        CSValue.CSType(csSystemPkg, "Int32", fq = false)
      case TypeId.Builtins.i64 =>
        CSValue.CSType(csSystemPkg, "Int64", fq = false)

      case TypeId.Builtins.u08 =>
        CSValue.CSType(csSystemPkg, "Byte", fq = false)
      case TypeId.Builtins.u16 =>
        CSValue.CSType(csSystemPkg, "UInt16", fq = false)
      case TypeId.Builtins.u32 =>
        CSValue.CSType(csSystemPkg, "UInt32", fq = false)
      case TypeId.Builtins.u64 =>
        CSValue.CSType(csSystemPkg, "UInt64", fq = false)

      case TypeId.Builtins.f32 =>
        CSValue.CSType(csSystemPkg, "Single", fq = false)
      case TypeId.Builtins.f64 =>
        CSValue.CSType(csSystemPkg, "Double", fq = false)
      case TypeId.Builtins.f128 =>
        CSValue.CSType(csSystemPkg, "Decimal", fq = false)

      case TypeId.Builtins.str =>
        CSValue.CSType(csSystemPkg, "String", fq = false)
      case TypeId.Builtins.uid =>
        CSValue.CSType(csSystemPkg, "Guid", fq = false)
      case TypeId.Builtins.tso | TypeId.Builtins.tsu =>
        CSValue.CSType(baboonTimePkg, "RpDateTime", fq = false)
      case _ =>
        throw new IllegalArgumentException(s"Unexpected: $b")
    }
  }

  def asCsType(tpe: TypeId,
               domain: Domain,
               evolution: BaboonEvolution,
               options: CompilerOptions,
               mut: Boolean = false): TextTree[CSValue] = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        q"${asCsTypeScalar(b)}"

      case b: TypeId.BuiltinCollection =>
        val ref = if (!mut) {

          b match {
            case TypeId.Builtins.map =>
              CSValue.CSType(csCollectionsImmutablePkg, "ImmutableDictionary", fq = false)
            case TypeId.Builtins.lst =>
              CSValue.CSType(csCollectionsImmutablePkg, "ImmutableList", fq = false)
            case TypeId.Builtins.set =>
              CSValue.CSType(csCollectionsImmutablePkg, "ImmutableHashSet", fq = false)
            case _ =>
              throw new IllegalArgumentException(s"Unexpected: $b")
          }
        } else {
          b match {
            case TypeId.Builtins.map =>
              CSValue.CSType(csCollectionsGenericPkg, "Dictionary", fq = false)
            case TypeId.Builtins.lst =>
              CSValue.CSType(csCollectionsGenericPkg, "List", fq = false)
            case TypeId.Builtins.set =>
              CSValue.CSType(csCollectionsGenericPkg, "HashSet", fq = false)
            case _ =>
              throw new IllegalArgumentException(s"Unexpected: $b")
          }
        }
        q"$ref"
      case u: TypeId.User =>
        q"${toCsTypeRefDeref(u, domain, evolution, options)}"
    }
  }

  def asCsRef(tpe: TypeRef,
              domain: Domain,
              evolution: BaboonEvolution,
              options: CompilerOptions,
              fullyQualified: Boolean = false,
              mut: Boolean = false,
  ): TextTree[CSValue] = {
    val out = tpe match {
      case TypeRef.Scalar(id) =>
        asCsType(id, domain, evolution, options, mut)

      case TypeRef.Constructor(id, args) =>
        if (id == TypeId.Builtins.opt) {
          q"${asCsRef(args.head, domain, evolution, options)}?"
        } else {
          val tpe = asCsType(id, domain, evolution, options, mut)
          val targs = args.map(asCsRef(_, domain, evolution, options))
          q"$tpe<${targs.toSeq.join(", ")}>"
        }

    }

    if (fullyQualified) {
      out.map {
        case t: CSType => t.fullyQualified
      }
    } else {
      out
    }
  }

  def deNull(tpe: TypeRef, ref: TextTree[CSValue]): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.str =>
                ref
              case _ =>
                q"$ref.Value"
            }
          case _ =>
            q"$ref!"
        }
      case _ =>
        q"$ref!"
    }
  }

  def isCSValueType(tpe: TypeRef): Boolean = {
    tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case s: TypeId.BuiltinScalar =>
            s match {
              case TypeId.Builtins.bit =>
                true
              case TypeId.Builtins.i08 =>
                true
              case TypeId.Builtins.i16 =>
                true
              case TypeId.Builtins.i32 =>
                true
              case TypeId.Builtins.i64 =>
                true
              case TypeId.Builtins.u08 =>
                true
              case TypeId.Builtins.u16 =>
                true
              case TypeId.Builtins.u32 =>
                true
              case TypeId.Builtins.u64 =>
                true
              case TypeId.Builtins.f32 =>
                true
              case TypeId.Builtins.f64 =>
                true
              case TypeId.Builtins.f128 =>
                true
              case TypeId.Builtins.uid =>
                true
              case TypeId.Builtins.tsu =>
                true
              case TypeId.Builtins.tso =>
                true
              case _ =>
                false
            }
          case _ => false
        }
      case _ => false
    }
  }
}

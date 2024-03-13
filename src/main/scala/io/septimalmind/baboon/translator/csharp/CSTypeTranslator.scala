package io.septimalmind.baboon.translator.csharp

import izumi.fundamentals.platform.strings.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSTypeTranslator.{
  generics,
  immutable,
  system
}
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.{
  Domain,
  DomainMember,
  Owner,
  Pkg,
  TypeId,
  TypeRef,
  Typedef,
  Version
}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree

class CSTypeTranslator() {

  def toCsPkg(p: Pkg, version: Version): CSPackageId = {
    val verString = "v" + version.version
      .split('.')
      .mkString("_")

    CSPackageId(p.path.map(_.capitalize) :+ verString)
  }

  def adtNsName(id: TypeId.User): String = {
    id.name.name.toLowerCase
  }

  def toCsVal(tid: TypeId.User, domain: Domain): CSType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        val fid = defn.bindings("cs")
        val parts = fid.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id = parts.last
        CSType(CSPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        val version = domain.version
        val pkg = toCsPkg(tid.pkg, version)
        val fullPkg = tid.owner match {
          case Owner.Toplevel => pkg
          case Owner.Adt(id)  => CSPackageId(pkg.parts :+ adtNsName(id))
        }
        CSType(fullPkg, tid.name.name.capitalize, fq = false)
    }

  }

  private def asCsTypeScalar(b: TypeId.BuiltinScalar) = {
    b match {
      case TypeId.Builtins.bit =>
        CSValue.CSType(system, "Boolean", fq = false)

      case TypeId.Builtins.i08 =>
        CSValue.CSType(system, "SByte", fq = false)
      case TypeId.Builtins.i16 =>
        CSValue.CSType(system, "Int16", fq = false)
      case TypeId.Builtins.i32 =>
        CSValue.CSType(system, "Int32", fq = false)
      case TypeId.Builtins.i64 =>
        CSValue.CSType(system, "Int64", fq = false)

      case TypeId.Builtins.u08 =>
        CSValue.CSType(system, "Byte", fq = false)
      case TypeId.Builtins.u16 =>
        CSValue.CSType(system, "UInt16", fq = false)
      case TypeId.Builtins.u32 =>
        CSValue.CSType(system, "UInt32", fq = false)
      case TypeId.Builtins.u64 =>
        CSValue.CSType(system, "UInt64", fq = false)

      case TypeId.Builtins.f32 =>
        CSValue.CSType(system, "Single", fq = false)
      case TypeId.Builtins.f64 =>
        CSValue.CSType(system, "Double", fq = false)
      case TypeId.Builtins.f128 =>
        CSValue.CSType(system, "Decimal", fq = false)

      case TypeId.Builtins.str =>
        CSValue.CSType(system, "String", fq = false)
      case TypeId.Builtins.uid =>
        CSValue.CSType(system, "Guid", fq = false)
      case TypeId.Builtins.tso =>
        CSValue.CSType(system, "DateTime", fq = false)
      case TypeId.Builtins.tsu =>
        CSValue.CSType(system, "DateTime", fq = false)
      case _ =>
        throw new IllegalArgumentException(s"Unexpected: $b")
    }
  }

  def asCsType(tpe: TypeId,
               domain: Domain,
               mut: Boolean = false): TextTree[CSValue] = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        q"${asCsTypeScalar(b)}"

      case b: TypeId.BuiltinCollection =>
        val ref = if (!mut) {

          b match {
            case TypeId.Builtins.map =>
              CSValue.CSType(immutable, "ImmutableDictionary", fq = false)
            case TypeId.Builtins.lst =>
              CSValue.CSType(immutable, "ImmutableList", fq = false)
            case TypeId.Builtins.set =>
              CSValue.CSType(immutable, "ImmutableHashSet", fq = false)
            case _ =>
              throw new IllegalArgumentException(s"Unexpected: $b")
          }
        } else {
          b match {
            case TypeId.Builtins.map =>
              CSValue.CSType(generics, "Dictionary", fq = false)
            case TypeId.Builtins.lst =>
              CSValue.CSType(generics, "List", fq = false)
            case TypeId.Builtins.set =>
              CSValue.CSType(generics, "HashSet", fq = false)
            case _ =>
              throw new IllegalArgumentException(s"Unexpected: $b")
          }
        }
        q"${ref}"
      case u: TypeId.User =>
        q"${toCsVal(u, domain)}"
    }
  }

  def asCsRef(tpe: TypeRef,
              domain: Domain,
              fullyQualified: Boolean = false,
              mut: Boolean = false,
  ): TextTree[CSValue] = {
    val out = tpe match {
      case TypeRef.Scalar(id) =>
        asCsType(id, domain, mut)

      case TypeRef.Constructor(id, args) =>
        if (id == TypeId.Builtins.opt) {
          q"${asCsRef(args.head, domain)}?"
        } else {
          val tpe = asCsType(id, domain, mut)
          val targs = args.map(asCsRef(_, domain))
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
}

object CSTypeTranslator {
  private val system = CSValue.CSPackageId(NEList("System"))
  private val generics =
    CSValue.CSPackageId(NEList("System", "Collections", "Generic"))
  private val immutable =
    CSValue.CSPackageId(NEList("System", "Collections", "Immutable"))
}

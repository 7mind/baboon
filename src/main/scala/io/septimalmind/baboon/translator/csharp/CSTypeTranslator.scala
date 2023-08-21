package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSTypeTranslator.{
  generics,
  immutable,
  system
}
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeRef, Version}
import izumi.fundamentals.collections.nonempty.NonEmptyList

class CSTypeTranslator() {

  def toCsPkg(p: Pkg, version: Version): CSPackageId = {
    val verString = "v" + version.version
      .split('.')
      .mkString("_")

    CSPackageId(p.path.map(_.capitalize) :+ verString)
  }

  def toCsVal(tid: TypeId.User, version: Version): CSType = {
    val pkg = toCsPkg(tid.pkg, version)
    val fullPkg = tid.owner match {
      case Owner.Toplevel => pkg
      case Owner.Adt(id)  => CSPackageId(pkg.parts :+ id.name.name.toLowerCase)
    }
    CSType(fullPkg, tid.name.name.capitalize, fq = false)
  }

  def asCsType(tpe: TypeId,
               version: Version,
               mut: Boolean = false): TextTree[CSValue] = {
    tpe match {
      case b: TypeId.Builtin =>
        val ref = if (!mut) {

          b match {
            case TypeId.Builtins.i08 =>
              CSValue.CSType(system, "Int16", fq = false)
            case TypeId.Builtins.i32 =>
              CSValue.CSType(system, "Int32", fq = false)
            case TypeId.Builtins.i64 =>
              CSValue.CSType(system, "Int64", fq = false)
            case TypeId.Builtins.str =>
              CSValue.CSType(system, "String", fq = false)
            case TypeId.Builtins.tso =>
              CSValue.CSType(system, "DateTime", fq = false)
            case TypeId.Builtins.tsu =>
              CSValue.CSType(system, "DateTime", fq = false)
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
            case TypeId.Builtins.i08 =>
              CSValue.CSType(system, "Int16", fq = false)
            case TypeId.Builtins.i32 =>
              CSValue.CSType(system, "Int32", fq = false)
            case TypeId.Builtins.i64 =>
              CSValue.CSType(system, "Int64", fq = false)
            case TypeId.Builtins.str =>
              CSValue.CSType(system, "String", fq = false)
            case TypeId.Builtins.tso =>
              CSValue.CSType(system, "DateTime", fq = false)
            case TypeId.Builtins.tsu =>
              CSValue.CSType(system, "DateTime", fq = false)
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
        q"${toCsVal(u, version)}"
    }
  }

  def asCsRef(tpe: TypeRef,
              version: Version,
              fullyQualified: Boolean = false,
              mut: Boolean = false,
  ): TextTree[CSValue] = {
    val out = tpe match {
      case TypeRef.Scalar(id) =>
        asCsType(id, version, mut)

      case TypeRef.Constructor(id, args) =>
        if (id == TypeId.Builtins.opt) {
          q"${asCsRef(args.head, version)}?"
        } else {
          val tpe = asCsType(id, version, mut)
          val targs = args.map(asCsRef(_, version))
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
  private val system = CSValue.CSPackageId(NonEmptyList("System"))
  private val generics =
    CSValue.CSPackageId(NonEmptyList("System", "Collections", "Generic"))
  private val immutable =
    CSValue.CSPackageId(NonEmptyList("System", "Collections", "Immutable"))
}

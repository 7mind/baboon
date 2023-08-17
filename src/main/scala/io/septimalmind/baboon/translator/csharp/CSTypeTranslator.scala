package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.TextTree.*
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeRef, Version}
import izumi.fundamentals.collections.nonempty.NonEmptyList

class CSTypeTranslator() {
  private val system = CSValue.CSPackageId(NonEmptyList("System"))
  private val generics =
    CSValue.CSPackageId(NonEmptyList("System", "Collections", "Generic"))

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

  def asCsType(tpe: TypeId, version: Version): TextTree[CSValue] = {

    tpe match {
      case b: TypeId.Builtin =>
        val ref = b match {
          case TypeId.Builtins.map =>
            CSValue.CSType(generics, "Dictionary", fq = false)
          case TypeId.Builtins.i08 =>
            CSValue.CSType(system, "Int16", fq = false)
          case TypeId.Builtins.i32 =>
            CSValue.CSType(system, "Int32", fq = false)
          case TypeId.Builtins.i64 =>
            CSValue.CSType(system, "Int64", fq = false)
          case TypeId.Builtins.lst =>
            CSValue.CSType(generics, "List", fq = false)
          case TypeId.Builtins.set =>
            CSValue.CSType(generics, "HashSet", fq = false)
          case TypeId.Builtins.str =>
            CSValue.CSType(system, "String", fq = false)
          case TypeId.Builtins.tso =>
            CSValue.CSType(system, "DateTime", fq = false)
          case TypeId.Builtins.tsu =>
            CSValue.CSType(system, "DateTime", fq = false)
          case _ =>
            throw new IllegalArgumentException(s"Unexpected: $b")
        }
        q"${ref}"
      case u: TypeId.User =>
        q"${toCsVal(u, version)}"
    }
  }

  def asCsType(tpe: TypeRef, version: Version): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        asCsType(id, version)

      case TypeRef.Constructor(id, args) =>
        if (id == TypeId.Builtins.opt) {
          q"${asCsType(args.head, version)}?"
        } else {
          val tpe = asCsType(id, version)
          val targs = args.map(asCsType(_, version))
          q"$tpe<${targs.toSeq.join(", ")}>"
        }

    }
  }
}

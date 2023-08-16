package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.TextTree
import io.septimalmind.baboon.translator.csharp.CSValue.{CSPackageId, CSType}
import io.septimalmind.baboon.typer.model.{
  Domain,
  Pkg,
  TypeId,
  TypeRef,
  Version
}
import izumi.fundamentals.collections.nonempty.NonEmptyList
import io.septimalmind.baboon.translator.TextTree.*

class CSTypeTranslator(domain: Domain) {
  private val system = CSValue.CSPackageId(NonEmptyList("System"))
  private val generics =
    CSValue.CSPackageId(NonEmptyList("System", "Collections", "Generic"))

  def toCsPkg(p: Pkg, version: Option[Version] = None): CSPackageId = {
    val verString = "v" + version
      .getOrElse(domain.version)
      .version
      .split('.')
      .mkString("_")

    assert(p == domain.id)

    CSPackageId(p.path.map(_.capitalize) :+ verString)
  }

  def toCsVal(tid: TypeId.User, version: Option[Version] = None): CSType = {
    val pkg = toCsPkg(tid.pkg, version)
    CSType(pkg, tid.name.name.capitalize)
  }

  def asCsType(tpe: TypeId): TextTree[CSValue] = {

    tpe match {
      case b: TypeId.Builtin =>
        val ref = b match {
          case TypeId.Builtins.map => CSValue.CSType(generics, "Dictionary")
          case TypeId.Builtins.i08 => CSValue.CSType(system, "Int16")
          case TypeId.Builtins.i32 => CSValue.CSType(system, "Int32")
          case TypeId.Builtins.i64 => CSValue.CSType(system, "Int64")
          case TypeId.Builtins.lst => CSValue.CSType(generics, "List")
          case TypeId.Builtins.set => CSValue.CSType(generics, "HashSet")
          case TypeId.Builtins.str => CSValue.CSType(system, "String")
          case TypeId.Builtins.tso => CSValue.CSType(system, "DateTime")
          case TypeId.Builtins.tsu => CSValue.CSType(system, "DateTime")
          case _ =>
            throw new IllegalArgumentException(s"Unexpected: $b")
        }
        q"${ref}"
      case u: TypeId.User =>
        q"${toCsVal(u)}"
    }
  }

  def asCsType(tpe: TypeRef): TextTree[CSValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        asCsType(id)

      case TypeRef.Constructor(id, args) =>
        if (id == TypeId.Builtins.opt) {
          q"${asCsType(args.head)}?"
        } else {
          val tpe = asCsType(id)
          val targs = args.map(asCsType)
          q"$tpe<${targs.toSeq.join(", ")}>"
        }

    }
  }
}

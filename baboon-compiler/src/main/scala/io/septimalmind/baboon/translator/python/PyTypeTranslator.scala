package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.python.PyTypes.*
import io.septimalmind.baboon.translator.python.PyValue.{PyModuleId, PyType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

final class PyTypeTranslator {
  def asPyRef(
    tpe: TypeRef,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): TextTree[PyValue] = {
    tpe match {
      case TypeRef.Scalar(id) =>
        q"${asPyType(id, domain, evolution, pkgBase)}"
      case TypeRef.Constructor(id, args) =>
        val tpe   = asPyType(id, domain, evolution, pkgBase)
        val targs = args.map(asPyRef(_, domain, evolution, pkgBase))
        q"$tpe[${targs.toSeq.join(", ")}]"
    }
  }

  def asPyType(
    tpe: TypeId,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): PyType = {
    tpe match {
      case b: TypeId.BuiltinScalar =>
        b match {
          case TypeId.Builtins.i08 | TypeId.Builtins.u08 => pyInt
          case TypeId.Builtins.i16 | TypeId.Builtins.u16 => pyInt
          case TypeId.Builtins.i32 | TypeId.Builtins.u32 => pyInt
          case TypeId.Builtins.i64 | TypeId.Builtins.u64 => pyInt
          case TypeId.Builtins.f32                       => pyFloat
          case TypeId.Builtins.f64                       => pyFloat
          case TypeId.Builtins.f128                      => pyDecimal
          case TypeId.Builtins.str                       => pyStr
          case TypeId.Builtins.uid                       => pyUuid
          case TypeId.Builtins.tso | TypeId.Builtins.tsu => pyDateTime
          case TypeId.Builtins.bit                       => pyBool
          case TypeId.Builtins.bytes                     => pyBytes

          case other => throw new IllegalArgumentException(s"Unexpected: $other")
        }
      case TypeId.Builtins.map => pyDict
      case TypeId.Builtins.lst => pyList
      case TypeId.Builtins.set => pySet
      case TypeId.Builtins.opt => pyOpt
      case uid: TypeId.User    => asPyTypeDerefForeign(uid, domain, evolution, pkgBase)
      case other               => throw new IllegalArgumentException(s"Unexpected: $other")
    }
  }

  def asPyTypeDerefForeign(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): PyType = {
    derefForeign(tid, domain).getOrElse(asPyTypeKeepForeigns(tid, domain, evolution, pkgBase))
  }

  def asPyTypeKeepForeigns(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String] = Nil,
  ): PyType = {
    val module = toPyModule(tid, domain.version, evolution, pkgBase)
    PyType(module, s"${tid.name.name.capitalize}")
  }

  def asPyTypeVersioned(
    tid: TypeId.User,
    domain: Domain,
    evolution: BaboonEvolution,
    pkgBase: List[String],
  ): PyType = {
    derefForeign(tid, domain).getOrElse {
      val moduleId      = toPyModule(tid, domain.version, evolution, pkgBase)
      val versionString = domain.version.format(prefix = "v", delimiter = "_")
      val ownerPath     = renderOwner(tid.owner)
      val moduleName = typeModuleName(tid) match {
        case Some(name) => ownerPath :+ name
        case None       => ownerPath
      }
      val typeName     = List(tid.name.name)
      val fullTypeName = (List(versionString) ++ moduleName ++ typeName).mkString(".")
      PyType(moduleId, fullTypeName, versioned = true)
    }
  }

  def toPyModule(
    tid: TypeId.User,
    version: Version,
    evolution: BaboonEvolution,
    pkgBase: List[String],
  ): PyModuleId = {
    val pathToModule    = tid.pkg.path.toList
    val versionPathPart = if (version != evolution.latest) List(version.format(prefix = "v", delimiter = "_")) else Nil
    val ownerPath       = renderOwner(tid.owner)
    val name            = typeModuleName(tid).toList
    val fullPath        = pkgBase ++ pathToModule ++ versionPathPart ++ ownerPath ++ name
    PyModuleId(NEList.unsafeFrom(fullPath), if (version == evolution.latest) None else Some(version))
  }

  def toPyModule(pkg: Pkg): PyModuleId = {
    PyModuleId(NEList.unsafeFrom(pkg.path.toList))
  }

  private def derefForeign(
    tid: TypeId.User,
    domain: Domain,
  ): Option[PyType] = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _, _, _) =>
        val foreign = defn.bindings("py")
        val parts   = foreign.decl.split('.').toList
        assert(parts.length > 1)
        val module = parts.init
        val id     = parts.last
        Some(PyType(PyModuleId(NEList.unsafeFrom(module)), id))
      case _ => None
    }
  }

  private def typeModuleName(tid: TypeId.User): Option[String] = {
    tid.owner match {
      case _: Owner.Adt => None
      case _            => Some(tid.name.name)
    }
  }

  private def renderOwner(owner: Owner): List[String] = {
    owner match {
      case Owner.Toplevel => Nil
      case Owner.Ns(path) => path.map(_.name.toLowerCase).toList
      case Owner.Adt(id)  => renderOwner(id.owner) :+ id.name.name
    }
  }
}

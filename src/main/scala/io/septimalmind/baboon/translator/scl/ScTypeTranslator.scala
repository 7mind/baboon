package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList

class ScTypeTranslator(target: ScTarget) {
  def toScPkg(p: Pkg, version: Version, evolution: BaboonEvolution): ScPackageId = {
    toScPkg(
      p,
      version,
      omitVersion = true,
    )
  }
  def toScPkg(p: Pkg, version: Version, omitVersion: Boolean): ScPackageId = {
    val verString = "v" + version.version
      .split('.')
      .mkString("_")

    val base = p.path.map(_.capitalize)
    val segments = if (omitVersion) {
      base
    } else {
      base :+ verString
    }

    ScPackageId(segments)
  }

  def toScTypeRefDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
    domain.defs.meta.nodes(tid) match {
      case DomainMember.User(_, defn: Typedef.Foreign, _) =>
        val fe    = defn.bindings("scala")
        val parts = fe.decl.split('.').toList
        assert(parts.length > 1)
        val pkg = parts.init
        val id  = parts.last
        ScType(ScPackageId(NEList.unsafeFrom(pkg)), id, fq = false)
      case _ =>
        toScTypeRefNoDeref(tid, domain, evolution)
    }
  }

  def toScTypeRefNoDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
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
        path.map(_.name)
      case Owner.Adt(id) =>
        val sub = renderOwner(id.owner) :+ adtNsName(id)
        sub
    }
  }

  def adtNsName(id: TypeId.User): String = {
    id.name.name
  }
}

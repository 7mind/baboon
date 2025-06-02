package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import io.septimalmind.baboon.typer.model.*

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
    ???
  }

  def toScTypeRefNoDeref(tid: TypeId.User, domain: Domain, evolution: BaboonEvolution): ScType = {
    ???
  }
}

package io.septimalmind.baboon.explore

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.{BaboonEnquiries, BaboonRuntimeCodec}
import izumi.fundamentals.collections.nonempty.NEList

class ExploreContext(
  val family: BaboonFamily,
  val enquiries: BaboonEnquiries,
  private val codec: BaboonRuntimeCodec[Lambda[(`+e`, `+a`) => Either[e, a]]]
) {
  def encode(pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): Either[BaboonIssue, Vector[Byte]] =
    codec.encode(family, pkg, version, idString, json, indexed)

  def decode(pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): Either[BaboonIssue, Json] =
    codec.decode(family, pkg, version, idString, data)

  private var _currentPkg: Option[Pkg] = None
  private var _currentVersion: Option[Version] = None

  def currentPkg: Option[Pkg] = _currentPkg
  def currentVersion: Option[Version] = _currentVersion

  def currentDomain: Option[Domain] = for {
    pkg <- _currentPkg
    ver <- _currentVersion
    lineage <- family.domains.toMap.get(pkg)
    dom <- lineage.versions.toMap.get(ver)
  } yield dom

  def currentLineage: Option[BaboonLineage] =
    _currentPkg.flatMap(family.domains.toMap.get)

  def switchTo(pkg: Pkg, version: Option[Version]): Either[String, Unit] = {
    family.domains.toMap.get(pkg) match {
      case None => Left(s"Domain not found: ${pkg.path.mkString(".")}")
      case Some(lineage) =>
        _currentPkg = Some(pkg)
        version match {
          case Some(v) if lineage.versions.toMap.contains(v) =>
            _currentVersion = Some(v)
            Right(())
          case Some(v) =>
            Left(s"Version not found: $v")
          case None =>
            _currentVersion = Some(lineage.evolution.latest)
            Right(())
        }
    }
  }

  def allDomains: Seq[Pkg] =
    family.domains.toMap.keys.toSeq.sortBy(_.path.mkString("."))

  def allVersions: Seq[Version] =
    currentLineage.toSeq.flatMap(_.versions.toMap.keys).sorted

  def allTypeIds: Seq[TypeId.User] =
    currentDomain.toSeq.flatMap { dom =>
      dom.defs.meta.nodes.values.collect { case u: DomainMember.User => u.id }
    }.sortBy(_.name.name)

  def findType(name: String): Option[DomainMember.User] = {
    currentDomain.flatMap { dom =>
      dom.defs.meta.nodes.values.collectFirst {
        case u: DomainMember.User if u.id.name.name == name => u
        case u: DomainMember.User if u.id.toString == name => u
      }
    }
  }

  def findTypeId(name: String): Option[TypeId.User] =
    findType(name).map(_.id)

  def parsePkg(pkgStr: String): Option[Pkg] = {
    val parts = pkgStr.split("\\.").toList
    NEList.from(parts).map(Pkg.apply)
  }

  def parseVersion(verStr: String): Option[Version] = {
    scala.util.Try(Version.parse(verStr)).toOption
  }
}

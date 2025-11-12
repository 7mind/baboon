package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, DomainMember, Pkg, TypeId, Version}
import izumi.functional.bio.Error2

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2](
  ) extends BaboonRuntimeCodec[F] {

    override def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      ???
    }

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      ???
    }

    private def getDef(dom: Domain, idString: String): DomainMember = {
      dom.defs.meta.nodes.map { case (k, v) => (k.toString, v) }(idString)
    }

    private def getDom(family: BaboonFamily, pkg: Pkg, version: Version) = {
      family.domains(pkg).versions(version)
    }
  }

}

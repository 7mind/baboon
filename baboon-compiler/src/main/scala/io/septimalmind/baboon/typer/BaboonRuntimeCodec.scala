package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, TypeId, Version}
import izumi.functional.bio.Error2

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2](
  ) extends BaboonRuntimeCodec[F] {

    override def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json] = ???

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]] = ???
  }

}

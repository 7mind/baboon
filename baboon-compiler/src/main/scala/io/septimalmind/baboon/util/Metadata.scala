package io.septimalmind.baboon.util

import io.circe.*
import io.circe.syntax.*
import izumi.fundamentals.collections.nonempty.NEList

case class VersionMeta(model: String, version: String)

object VersionMeta {
  implicit val codec: Codec.AsObject[VersionMeta] =
    io.circe.generic.semiauto.deriveCodec[VersionMeta]
}

object BaboonDomainCodecs {

  import io.circe.generic.semiauto.deriveEncoder
  import io.septimalmind.baboon.typer.model.*

  implicit lazy val versionKeyEncoder: KeyEncoder[Version] = KeyEncoder.encodeKeyString.contramap(_.v.toString)
  implicit lazy val versionEncoder: Encoder[Version]       = Encoder.encodeString.contramap(_.v.toString)

//  implicit lazy val unmodifiedSince: Encoder[UnmodifiedSince] = Encoder.encodeList[String].contramap(_.sameIn.toList.map(_.version))
  implicit lazy val unmodifiedSince: Encoder[UnmodifiedSince] = deriveEncoder

  implicit def nelist[T: Encoder]: Encoder[NEList[T]] = Encoder.encodeList[T].contramap(_.toList)

  implicit lazy val fieldEncoder: Encoder[Field] = Encoder.encodeJson.contramap {
    f =>
      Json.obj("field" -> Json.fromString(f.name.name), "type" -> Json.fromString(f.tpe.toString))
  }

  implicit lazy val derivationFailure_EnumBranchRemoved: Encoder[DerivationFailure.EnumBranchRemoved] = Encoder.encodeList[String].contramap(r => r.op.map(_.m.name))
  implicit lazy val derivationFailure_AdtBranchRemoved: Encoder[DerivationFailure.AdtBranchRemoved] =
    Encoder.encodeList[Json].contramap(r => r.op.map(b => (b.id: TypeId).asJson))
  implicit lazy val derivationFailure_IncompatibleFields: Encoder[DerivationFailure.IncompatibleFields] = new Encoder[DerivationFailure.IncompatibleFields] {
    override def apply(a: DerivationFailure.IncompatibleFields): Json =
      Json.obj(
        "changes" -> a.incompatibleChanges
          .map(f => Json.obj("field" -> f.f.asJson, "newType" -> Json.fromString(f.newType.toString))).asJson,
        "additions" -> a.incompatibleAdditions.map(_.f).asJson,
      )
  }

  implicit lazy val derivationFailure_IncompatibleRenames: Encoder[DerivationFailure.IncompatibleRenames] = new Encoder[DerivationFailure.IncompatibleRenames] {
    override def apply(a: DerivationFailure.IncompatibleRenames): Json =
      Json.obj(
        "renames" -> a.incompatibleRenames
          .map(r =>
            Json.obj(
              "oldField" -> r.oldField.asJson,
              "newField" -> r.newField.asJson,
            )
          ).asJson
      )
  }

  implicit lazy val derivationFailure_Foreign: Encoder[DerivationFailure.Foreign.type] = deriveEncoder
  implicit lazy val derivationFailureEncoder: Encoder[DerivationFailure]               = deriveEncoder

  implicit lazy val pkgKeyEncoder: KeyEncoder[Pkg] = KeyEncoder.encodeKeyString.contramap(_.toString)

  implicit lazy val typeIdEncoder: Encoder[TypeId]       = Encoder.encodeString.contramap(_.toString)
  implicit lazy val typeIdKeyEncoder: KeyEncoder[TypeId] = KeyEncoder.encodeKeyString.contramap(_.toString)

  implicit lazy val typeRefEncoder: Encoder[TypeRef] = Encoder.encodeString.contramap(_.toString)

}

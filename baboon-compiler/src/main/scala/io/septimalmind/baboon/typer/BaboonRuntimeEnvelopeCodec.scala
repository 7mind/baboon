package io.septimalmind.baboon.typer

import baboon.runtime.shared.{BaboonTypeMeta, BaboonTypeMetaCodec, LEDataInputStream, LEDataOutputStream}
import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, RuntimeCodecIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, EOFException}
import scala.util.{Failure, Success, Try}

/** Binary layout of the top-level envelope a JSON envelope is converted into (docs/spec/codec-envelope.md § 2.1, § 2.1.3). */
sealed trait BinEnvelopeVersion
object BinEnvelopeVersion {
  /** Single bound, written under `ForwardWritePolicy.Strict` (the facade default). */
  case object V1 extends BinEnvelopeVersion
  /** Both bounds: byte-identical `minCompat` and the index mode's prefix `readableMin`. */
  case object V2 extends BinEnvelopeVersion
}

final case class BinEnvelopeOptions(envelopeVersion: BinEnvelopeVersion, indexed: Boolean)

/** Format conversion between the JSON and UEBA forms of a top-level `BaboonTypeMeta` envelope,
  * driven by the interpreted [[BaboonRuntimeCodec]] over a loaded model.
  *
  * This is not schema migration. The writer's domain, version and type identifier are copied
  * verbatim; the payload is read with the writer's exact version, or — for a writer newer than
  * every loaded version — with the newest loaded version only when the envelope proves the payload
  * byte-identical to it (`ForwardReadPolicy.Lossless`). Compatibility bounds are recomputed from the
  * loaded model for the output format, because JSON `$rv` (json-additive) and binary `readableMin`
  * (prefix-compact / prefix-any-mode) promise different things.
  */
trait BaboonRuntimeEnvelopeCodec[F[+_, +_]] {
  def jsonToUeba(family: BaboonFamily, envelope: Json, options: BinEnvelopeOptions): F[BaboonIssue, Vector[Byte]]
  def uebaToJson(family: BaboonFamily, envelope: Vector[Byte]): F[BaboonIssue, Json]
}

object BaboonRuntimeEnvelopeCodec {

  private val ContentKey: String        = "$c"
  private val EnvelopeKeys: Set[String] = Set("$mv", "$d", "$v", "$t", "$uv", "$rv", ContentKey)

  /** The envelope's bounds as versions; invariant `readableMin <= minCompat <= version`. */
  private final case class EnvelopeBounds(version: Version, minCompat: Version, readableMin: Version)

  /** Where the payload is read: `readVersion` equals the writer's version unless a lossless forward read applies. */
  private final case class ReadTarget(lineage: BaboonLineage, readVersion: Version, typeId: TypeId.User)

  class BaboonRuntimeEnvelopeCodecImpl[F[+_, +_]: Error2](codec: BaboonRuntimeCodec[F]) extends BaboonRuntimeEnvelopeCodec[F] {
    private val F = Error2[F]

    override def jsonToUeba(family: BaboonFamily, envelope: Json, options: BinEnvelopeOptions): F[BaboonIssue, Vector[Byte]] = {
      for {
        obj <- F.fromOption(invalid("a JSON envelope must be a JSON object"))(envelope.asObject)
        _ <- obj.keys.filterNot(EnvelopeKeys.contains).toList match {
          case Nil     => F.unit
          case unknown => F.fail(invalid(s"unexpected JSON envelope keys: ${unknown.mkString(", ")}"))
        }
        content <- F.fromOption(invalid(s"the JSON envelope has no '$ContentKey' content"))(obj(ContentKey))
        meta <- F.fromOption(
          invalid(
            "malformed JSON envelope metadata: '$d', '$v' and '$t' must be strings, '$uv' and '$rv' strings when present, and '$mv' the number 1 (or the legacy string \"1\") when present"
          )
        )(BaboonTypeMetaCodec.readMeta(envelope))
        bounds  <- parseBounds(meta)
        target  <- resolve(family, meta, bounds, minCompatIsByteIdentical = true)
        payload <- guarded(codec.encodeComplete(family, target.lineage.pkg, target.readVersion, meta.typeIdentifier, content, options.indexed))
        outMeta <- binaryMeta(target, meta, options)
      } yield {
        val out    = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(out)
        BaboonTypeMetaCodec.writeBin(outMeta, writer)
        writer.flush()
        Vector.from(out.toByteArray) ++ payload
      }
    }

    override def uebaToJson(family: BaboonFamily, envelope: Vector[Byte]): F[BaboonIssue, Json] = {
      val input  = new ByteArrayInputStream(envelope.toArray)
      val reader = new LEDataInputStream(input)
      for {
        metaVersion <- F.fromOption(invalid("empty input: a binary envelope starts with its metaVersion byte"))(envelope.headOption.map(_ & 0xFF))
        meta <- Try(BaboonTypeMetaCodec.readMeta(reader)) match {
          case Success(Some(meta)) => F.pure(meta)
          case Success(None) if metaVersion == 1 || metaVersion == 2 =>
            F.fail(invalid(s"binary envelope v$metaVersion has an illegal flags byte"))
          case Success(None) => F.fail(invalid(s"unsupported binary envelope metaVersion $metaVersion (supported: 1, 2)"))
          case Failure(_)    => F.fail(invalid("truncated binary envelope header"))
        }
        payload = envelope.drop(envelope.length - input.available())
        bounds <- parseBounds(meta)
        // only v2 guarantees that minCompat is the byte-identical bound; a v1 slot may carry a
        // Tolerant writer's prefix bound (docs/spec/codec-envelope.md § 2.1.2)
        target  <- resolve(family, meta, bounds, minCompatIsByteIdentical = meta.metaVersion == BaboonTypeMetaCodec.META_VERSION_2)
        content <- guarded(codec.decodeComplete(family, target.lineage.pkg, target.readVersion, meta.typeIdentifier, payload))
        outMeta <- jsonMeta(target, meta)
      } yield {
        BaboonTypeMetaCodec.writeJson(outMeta).mapObject(_.add(ContentKey, content))
      }
    }

    private def parseBounds(meta: BaboonTypeMeta): F[BaboonIssue, EnvelopeBounds] = {
      for {
        version     <- parseVersion("domain version", meta.domainVersion)
        minCompat   <- parseVersion("minimal compatible version", meta.domainVersionMinCompat)
        readableMin <- parseVersion("minimal readable version", meta.domainVersionReadableMin)
        _ <-
          if (readableMin <= minCompat && minCompat <= version) F.unit
          else
            F.fail(
              invalid(s"bounds must satisfy readableMin <= minCompat <= version, got readableMin=$readableMin, minCompat=$minCompat, version=$version")
            )
      } yield EnvelopeBounds(version, minCompat, readableMin)
    }

    /** Writers emit canonical versions; `Version.parse` accepts anything, so the round trip is the check. */
    private def parseVersion(what: String, raw: String): F[BaboonIssue, Version] = {
      val parsed = Version.parse(raw)
      parsed.v match {
        case _: izumi.fundamentals.platform.versions.Version.Unknown => F.fail(invalid(s"$what '$raw' is not a version"))
        case _ if parsed.toString != raw                             => F.fail(invalid(s"$what '$raw' is not in canonical form '$parsed'"))
        case _                                                       => F.pure(parsed)
      }
    }

    private def resolve(family: BaboonFamily, meta: BaboonTypeMeta, bounds: EnvelopeBounds, minCompatIsByteIdentical: Boolean): F[BaboonIssue, ReadTarget] = {
      val pkg = Pkg(NEList.unsafeFrom(meta.domainIdentifier.split("\\.", -1).toList))
      for {
        lineage <- F.fromOption(unknown(s"domain '${meta.domainIdentifier}' is not loaded"))(family.domains.toMap.get(pkg))
        loaded   = lineage.versions.toMap.keys.toList.sorted
        newest   = loaded.last
        readVersion <-
          if (loaded.contains(bounds.version)) {
            F.pure(bounds.version)
          } else if (bounds.version > newest) {
            if (!minCompatIsByteIdentical) {
              F.fail(
                lossy(
                  s"${meta.domainIdentifier} ${meta.domainVersion} is newer than every loaded version (newest: $newest) and a binary v1 envelope cannot prove its payload byte-identical to one: its single bound may be a ForwardWritePolicy.Tolerant prefix bound. Load ${meta.domainVersion}, or have the writer emit binary v2"
                )
              )
            } else if (bounds.minCompat <= newest) {
              F.pure(newest)
            } else {
              F.fail(
                lossy(
                  s"${meta.domainIdentifier} ${meta.domainVersion} is newer than every loaded version (newest: $newest) and its byte-identical bound ${bounds.minCompat} reaches none of them; only a forward read that drops data would decode it"
                )
              )
            }
          } else {
            F.fail(unknown(s"domain version ${meta.domainIdentifier} ${meta.domainVersion} is not loaded (loaded: ${loaded.mkString(", ")})"))
          }
        domain = lineage.versions.toMap(readVersion)
        member <- F.fromOption(unknown(s"type '${meta.typeIdentifier}' does not exist in ${meta.domainIdentifier} $readVersion")) {
          domain.defs.meta.nodes.collectFirst {
            case (id: TypeId.User, member: DomainMember.User) if id.toString == meta.typeIdentifier => (id, member)
          }
        }
        typeId = member._1
        _ <- member._2.defn match {
          case _: Typedef.Dto | _: Typedef.Adt | _: Typedef.Enum => F.unit
          case other =>
            F.fail(RuntimeCodecIssue.CannotEncodeType(typeId, s"${other.getClass.getSimpleName.toLowerCase} types never travel in a top-level envelope"): BaboonIssue)
        }
        sameIn = lineage.evolution.typesUnchangedSince(readVersion)(typeId).sameIn.toList
        // an envelope claiming byte-identity across a version in which the loaded model changes the
        // type was written against a different schema
        contradicted = loaded.filter(v => v >= bounds.minCompat && v <= readVersion && !sameIn.contains(v))
        _ <-
          if (!minCompatIsByteIdentical || contradicted.isEmpty) F.unit
          else
            F.fail(
              invalid(
                s"${meta.typeIdentifier} is declared byte-identical since ${bounds.minCompat}, but in the loaded model its ${contradicted.mkString(", ")} definition differs from $readVersion"
              )
            )
      } yield ReadTarget(lineage, readVersion, typeId)
    }

    private def binaryMeta(target: ReadTarget, meta: BaboonTypeMeta, options: BinEnvelopeOptions): F[BaboonIssue, BaboonTypeMeta] = {
      val minCompat = byteIdenticalBound(target)
      options.envelopeVersion match {
        case BinEnvelopeVersion.V1 =>
          F.pure(BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION_1, meta.domainIdentifier, meta.domainVersion, minCompat, meta.typeIdentifier))
        case BinEnvelopeVersion.V2 =>
          val tier = if (options.indexed) ForwardCompatTier.PrefixAnyMode else ForwardCompatTier.PrefixCompact
          readerBound(target, tier).map {
            readableMin =>
              BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION_2, meta.domainIdentifier, meta.domainVersion, minCompat, meta.typeIdentifier, readableMin)
          }
      }
    }

    private def jsonMeta(target: ReadTarget, meta: BaboonTypeMeta): F[BaboonIssue, BaboonTypeMeta] = {
      readerBound(target, ForwardCompatTier.JsonAdditive).map {
        readableMin =>
          BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION_1, meta.domainIdentifier, meta.domainVersion, byteIdenticalBound(target), meta.typeIdentifier, readableMin)
      }
    }

    private def byteIdenticalBound(target: ReadTarget): String = {
      target.lineage.evolution.typesUnchangedSince(target.readVersion)(target.typeId).sameIn.head.toString
    }

    private def readerBound(target: ReadTarget, tier: ForwardCompatTier): F[BaboonIssue, String] = {
      F.fromOption(RuntimeCodecIssue.CannotEncodeType(target.typeId, s"the loaded model publishes no '${tier.wireName}' bound"): BaboonIssue)(
        target.lineage.evolution.minReaders(target.readVersion, target.typeId).get(tier).map(_.toString)
      )
    }

    /** The interpreter signals malformed payloads (e.g. a truncated stream) by throwing. */
    private def guarded[A](action: => F[BaboonIssue, A]): F[BaboonIssue, A] = {
      Try(action) match {
        case Success(result)          => result
        case Failure(_: EOFException) => F.fail(invalid("truncated payload"))
        case Failure(e)               => F.fail(invalid(s"payload conversion failed: ${e.getMessage}"))
      }
    }

    private def invalid(reason: String): BaboonIssue = RuntimeCodecIssue.InvalidEnvelope(reason)
    private def unknown(reason: String): BaboonIssue = RuntimeCodecIssue.UnknownEnvelopeIdentity(reason)
    private def lossy(reason: String): BaboonIssue   = RuntimeCodecIssue.LossyEnvelopeConversion(reason)
  }
}

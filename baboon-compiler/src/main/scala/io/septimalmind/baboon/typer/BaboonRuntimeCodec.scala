package io.septimalmind.baboon.typer

import io.circe.{Json, JsonObject}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, RuntimeCodecIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import baboon.runtime.shared.{BaboonBinTools, ByteString, LEDataInputStream, LEDataOutputStream}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.time.format.DateTimeFormatterBuilder
import java.time.OffsetDateTime
import java.util.UUID
import scala.util.Try

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]]

  /** [[decode]] that fails when bytes remain after the value instead of ignoring them. */
  def decodeComplete(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]

  /** [[encode]] that fails on JSON object keys a DTO does not declare instead of dropping them. */
  def encodeComplete(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  private final case class EncodeMode(indexed: Boolean, rejectUnknownFields: Boolean)

  /** UEBA byte width and the exact accepted range of an integer builtin. */
  private final case class IntegerKind(width: Int, min: BigInt, max: BigInt) {
    def contains(value: BigInt): Boolean = value >= min && value <= max
  }

  private val Wide: Int = 8

  private val IntegerKinds: Map[TypeId.BuiltinScalar, IntegerKind] = Map(
    TypeId.Builtins.i08 -> IntegerKind(1, BigInt(Byte.MinValue), BigInt(Byte.MaxValue)),
    TypeId.Builtins.u08 -> IntegerKind(1, BigInt(0), BigInt(0xFF)),
    TypeId.Builtins.i16 -> IntegerKind(2, BigInt(Short.MinValue), BigInt(Short.MaxValue)),
    TypeId.Builtins.u16 -> IntegerKind(2, BigInt(0), BigInt(0xFFFF)),
    TypeId.Builtins.i32 -> IntegerKind(4, BigInt(Int.MinValue), BigInt(Int.MaxValue)),
    TypeId.Builtins.u32 -> IntegerKind(4, BigInt(0), BigInt(0xFFFFFFFFL)),
    TypeId.Builtins.i64 -> IntegerKind(Wide, BigInt(Long.MinValue), BigInt(Long.MaxValue)),
    TypeId.Builtins.u64 -> IntegerKind(Wide, BigInt(0), (BigInt(1) << 64) - 1),
  )

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2]() extends BaboonRuntimeCodec[F] {
    private val F         = Error2[F]
    private val enquiries = new BaboonEnquiries.BaboonEnquiriesImpl()

    // ISO 8601 formatters for timestamps with exactly 3 fractional digits (matching C# format).
    // M28-N01 (PR-28.3): tso = "±HH:MM" always (UTC = "+00:00", NOT "Z"); tsu retains "Z" for UTC.
    // PR-29P.1 round-2: previously a single shared formatter used appendOffset("+HH:MM", "Z"),
    // which collapsed UTC-zero offset to "Z" for both tso and tsu — wrong for tso.
    // Mirrors the BaboonTools.scala (generated runtime) split landed in PR-28.3 (commit b85fc30).
    // tsu: zero-offset literal "Z" — UTC renders as "...Z".
    private val tsuFormatter = new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
      .appendOffset("+HH:MM", "Z")
      .toFormatter()
    // tso: zero-offset literal "+00:00" — UTC renders as "...+00:00".
    private val tsoFormatter = new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
      .appendOffset("+HH:MM", "+00:00")
      .toFormatter()

    override def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json] = {
      decodeWith(family, pkg, version, idString, data, requireCompleteInput = false)
    }

    override def decodeComplete(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json] = {
      decodeWith(family, pkg, version, idString, data, requireCompleteInput = true)
    }

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]] = {
      encodeWith(family, pkg, version, idString, json, EncodeMode(indexed, rejectUnknownFields = false))
    }

    override def encodeComplete(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]] = {
      encodeWith(family, pkg, version, idString, json, EncodeMode(indexed, rejectUnknownFields = true))
    }

    private def decodeWith(
      family: BaboonFamily,
      pkg: Pkg,
      version: Version,
      idString: String,
      data: Vector[Byte],
      requireCompleteInput: Boolean,
    ): F[BaboonIssue, Json] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      val bytes   = new ByteArrayInputStream(data.toArray)
      val input   = new LEDataInputStream(bytes)

      typedef match {
        case u: DomainMember.User =>
          decodeUserType(dom, u.defn, input).flatMap {
            json =>
              val remaining = bytes.available()
              if (requireCompleteInput && remaining > 0) F.fail(RuntimeCodecIssue.TrailingBytes(u.id, remaining)) else F.pure(json)
          }
        case _ => F.fail(RuntimeCodecIssue.UnexpectedDomainMemberType(typedef.id, s"Expected User type, got: ${typedef.getClass.getSimpleName}"))
      }
    }

    private def encodeWith(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, mode: EncodeMode): F[BaboonIssue, Vector[Byte]] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      val output  = new ByteArrayOutputStream()
      val writer  = new LEDataOutputStream(output)

      typedef match {
        case u: DomainMember.User =>
          encodeUserType(dom, u.defn, json, writer, mode).map {
            _ =>
              writer.flush()
              Vector.from(output.toByteArray)
          }
        case _ => F.fail(RuntimeCodecIssue.UnexpectedDomainMemberType(typedef.id, s"Expected User type, got: ${typedef.getClass.getSimpleName}"))
      }
    }

    private def getDef(dom: Domain, idString: String): DomainMember = {
      dom.defs.meta.nodes.map { case (k, v) => (k.toString, v) }(idString)
    }

    private def getDom(family: BaboonFamily, pkg: Pkg, version: Version): Domain = {
      family.domains(pkg).versions(version)
    }

    // Encode user-defined types
    private def encodeUserType(dom: Domain, typedef: Typedef.User, json: Json, writer: LEDataOutputStream, mode: EncodeMode): F[BaboonIssue, Unit] = {
      typedef match {
        case dto: Typedef.Dto   => encodeDto(dom, dto, json, writer, mode)
        case enum: Typedef.Enum => encodeEnum(dom, enum, json, writer)
        case adt: Typedef.Adt   => encodeAdt(dom, adt, json, writer, mode)
        case f: Typedef.Foreign =>
          f.runtimeMapping match {
            case Some(typeRef) => encodeTypeRef(dom, typeRef, json, writer, mode)
            case None          => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Foreign types without rt binding cannot be encoded"))
          }
        case _: Typedef.Service  => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Service types cannot be encoded"))
        case _: Typedef.Contract => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Contract types cannot be encoded"))
      }
    }

    // Decode user-defined types
    private def decodeUserType(dom: Domain, typedef: Typedef.User, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      typedef match {
        case dto: Typedef.Dto   => decodeDto(dom, dto, reader)
        case enum: Typedef.Enum => decodeEnum(dom, enum, reader)
        case adt: Typedef.Adt   => decodeAdt(dom, adt, reader)
        case f: Typedef.Foreign =>
          f.runtimeMapping match {
            case Some(typeRef) => decodeTypeRef(dom, typeRef, reader)
            case None          => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Foreign types without rt binding cannot be decoded"))
          }
        case _: Typedef.Service  => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Service types cannot be decoded"))
        case _: Typedef.Contract => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Contract types cannot be decoded"))
      }
    }

    // DTO encoding/decoding
    private def encodeDto(dom: Domain, dto: Typedef.Dto, json: Json, writer: LEDataOutputStream, mode: EncodeMode): F[BaboonIssue, Unit] = {
      json.asObject match {
        case None => F.fail(RuntimeCodecIssue.ExpectedJsonObject(dto.id.toString, json))
        case Some(obj) if mode.rejectUnknownFields && undeclaredFields(dto, obj).nonEmpty =>
          F.fail(RuntimeCodecIssue.UnknownJsonFields(dto.id, undeclaredFields(dto, obj)))
        case Some(obj) =>
          // Write header byte
          val header: Byte = if (mode.indexed) 1 else 0
          writer.writeByte(header.toInt)

          if (mode.indexed) {
            // In indexed mode: collect field data in buffer, write index, then data
            val fieldDataBuffer = new ByteArrayOutputStream()
            val fieldDataWriter = new LEDataOutputStream(fieldDataBuffer)
            val indexEntries    = scala.collection.mutable.ArrayBuffer[(Int, Int)]()

            F.foldLeft(dto.fields)(0) {
              case (offset, field) =>
                val fieldJson = obj(field.name.name).getOrElse(Json.Null)

                if (enquiries.uebaLen(dom.defs.meta.nodes, field.tpe).isVariable) {
                  // Variable-length field: record position, write to buffer, record length
                  val before = fieldDataBuffer.size()
                  encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, mode).map {
                    _ =>
                      val after  = fieldDataBuffer.size()
                      val length = after - before
                      indexEntries += ((offset, length))
                      after
                  }
                } else {
                  // Fixed-length field: just write to buffer
                  encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, mode).map {
                    _ => fieldDataBuffer.size()
                  }
                }
            }.flatMap {
                _ =>
                  fieldDataWriter.flush()

                  // Write index entries (offset and length for each variable-length field)
                  indexEntries.foreach {
                    case (off, len) =>
                      writer.writeInt(off)
                      writer.writeInt(len)
                  }

                  // Write all field data
                  writer.write(fieldDataBuffer.toByteArray)
                  F.unit
              }
          } else {
            // Compact mode: write fields directly
            F.traverse_(dto.fields) {
              field =>
                val fieldJson = obj(field.name.name).getOrElse(Json.Null)
                encodeTypeRef(dom, field.tpe, fieldJson, writer, mode)
            }
          }
      }
    }

    private def undeclaredFields(dto: Typedef.Dto, obj: JsonObject): List[String] = {
      obj.keys.filterNot(key => dto.fields.exists(_.name.name == key)).toList
    }

    private def decodeDto(dom: Domain, dto: Typedef.Dto, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      // Read header byte
      val header = reader.readByte()

      // Check if indices are used (bit 0 set)
      val useIndices = (header & 1) != 0

      if (useIndices) {
        // Read and skip index entries
        val varLenFieldCount = dto.fields.count(f => enquiries.uebaLen(dom.defs.meta.nodes, f.tpe).isVariable)
        (0 until varLenFieldCount).foreach {
          _ =>
            reader.readInt() // offset (ignore)
            reader.readInt() // length (ignore)
        }
      }

      // Decode each field (data is in the same order regardless of indexed mode)
      F.traverse(dto.fields) {
        field =>
          decodeTypeRef(dom, field.tpe, reader).map {
            value => (field.name.name, value)
          }
      }.map(fields => Json.obj(fields*))
    }

    // Enum encoding/decoding
    private def encodeEnum(@annotation.unused dom: Domain, `enum`: Typedef.Enum, json: Json, writer: LEDataOutputStream): F[BaboonIssue, Unit] = {
      json.asString match {
        case None => F.fail(RuntimeCodecIssue.ExpectedJsonString(enum.id.toString, json))
        case Some(s) =>
          val str = s.trim
          val idx = enum.members.toList.indexWhere(m => EnumWireStyle.wireName(m.name) == str)
          if (idx < 0) {
            F.fail(RuntimeCodecIssue.UnknownEnumValue(enum.id, str, enum.members.toList.map(m => EnumWireStyle.wireName(m.name))))
          } else {
            writer.writeByte(idx)
            F.unit
          }
      }
    }

    private def decodeEnum(@annotation.unused dom: Domain, `enum`: Typedef.Enum, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      val idx = reader.readByte() & 0xFF

      if (idx >= enum.members.size) {
        F.fail(RuntimeCodecIssue.InvalidEnumIndex(enum.id, idx, enum.members.size - 1))
      } else {
        F.pure(Json.fromString(EnumWireStyle.wireName(enum.members.toList(idx).name)))
      }
    }

    // ADT encoding/decoding
    private def encodeAdt(dom: Domain, adt: Typedef.Adt, json: Json, writer: LEDataOutputStream, mode: EncodeMode): F[BaboonIssue, Unit] = {
      json.asObject match {
        case None      => F.fail(RuntimeCodecIssue.ExpectedJsonObject(adt.id.toString, json))
        case Some(obj) =>
          // Find the branch - the object should have exactly one key
          val branches = obj.toList
          if (branches.length != 1) {
            F.fail(RuntimeCodecIssue.InvalidAdtStructure(adt.id, branches.map(_._1)))
          } else {
            val (branchName, branchValue) = branches.head
            val dataMembers               = adt.dataMembers(dom)

            val branchIdx = dataMembers.indexWhere(_.name.name == branchName)
            if (branchIdx < 0) {
              F.fail(RuntimeCodecIssue.UnknownAdtBranch(adt.id, branchName, dataMembers.toList.map(_.name.name)))
            } else {
              // Write discriminator
              writer.writeByte(branchIdx)

              // Encode the branch value
              val branchId  = dataMembers(branchIdx)
              val branchDef = dom.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
              encodeUserType(dom, branchDef, branchValue, writer, mode)
            }
          }
      }
    }

    private def decodeAdt(dom: Domain, adt: Typedef.Adt, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      val discriminator = reader.readByte() & 0xFF
      val dataMembers   = adt.dataMembers(dom)

      if (discriminator >= dataMembers.size) {
        F.fail(RuntimeCodecIssue.InvalidAdtDiscriminator(adt.id, discriminator, dataMembers.size - 1))
      } else {
        val branchId  = dataMembers(discriminator)
        val branchDef = dom.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
        decodeUserType(dom, branchDef, reader).map {
          branchValue => Json.obj(branchId.name.name -> branchValue)
        }
      }
    }

    // TypeRef encoding/decoding
    private def encodeTypeRef(dom: Domain, tpe: TypeRef, json: Json, writer: LEDataOutputStream, mode: EncodeMode): F[BaboonIssue, Unit] = {
      tpe match {
        case TypeRef.Scalar(id)            => encodeScalar(dom, id, json, writer, mode)
        case TypeRef.Constructor(id, args) => encodeConstructor(dom, id, args.toList, json, writer, mode)
        case _: TypeRef.Any                => AnyPlaceholder.notSupportedYet("BaboonRuntimeCodec.encodeTypeRef")
      }
    }

    private def decodeTypeRef(dom: Domain, tpe: TypeRef, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      tpe match {
        case TypeRef.Scalar(id)            => decodeScalar(dom, id, reader)
        case TypeRef.Constructor(id, args) => decodeConstructor(dom, id, args.toList, reader)
        case _: TypeRef.Any                => AnyPlaceholder.notSupportedYet("BaboonRuntimeCodec.decodeTypeRef")
      }
    }

    // Scalar encoding/decoding
    private def encodeScalar(dom: Domain, id: TypeId.Scalar, json: Json, writer: LEDataOutputStream, mode: EncodeMode): F[BaboonIssue, Unit] = {
      id match {
        case s: TypeId.BuiltinScalar => encodeBuiltinScalar(s, json, writer)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          encodeUserType(dom, typedef, json, writer, mode)
      }
    }

    private def decodeScalar(dom: Domain, id: TypeId.Scalar, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      id match {
        case s: TypeId.BuiltinScalar => decodeBuiltinScalar(s, reader)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          decodeUserType(dom, typedef, reader)
      }
    }

    // Builtin scalar encoding/decoding
    private def encodeBuiltinScalar(id: TypeId.BuiltinScalar, json: Json, writer: LEDataOutputStream): F[BaboonIssue, Unit] = {
      id match {
        case TypeId.Builtins.bit =>
          json.asBoolean match {
            case Some(value) =>
              writer.writeBoolean(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonBoolean(json))
          }

        case integer if IntegerKinds.contains(integer) =>
          val kind = IntegerKinds(integer)
          // 64-bit integers arrive as a decimal string, and as a JSON number from a writer older than
          // the string form (docs/json-codecs.md, "64-bit integers")
          val parsed = json.asNumber.flatMap(_.toBigInt).orElse(if (kind.width == Wide) json.asString.flatMap(parseInteger) else None)
          parsed.filter(kind.contains) match {
            case Some(value) =>
              writeInteger(kind, value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber(integer.name.name, json))
          }

        case TypeId.Builtins.f32 =>
          json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble.toFloat)) match {
            case Some(value) =>
              writer.writeFloat(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f32", json))
          }

        case TypeId.Builtins.f64 =>
          json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble)) match {
            case Some(value) =>
              writer.writeDouble(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f64", json))
          }

        case TypeId.Builtins.f128 =>
          json.asNumber.flatMap(_.toBigDecimal) match {
            case Some(value) =>
              BaboonBinTools.writeBigDecimal(writer, value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f128", json))
          }

        case TypeId.Builtins.str =>
          json.asString match {
            case Some(value) =>
              BaboonBinTools.writeString(writer, value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("str", json))
          }

        case TypeId.Builtins.bytes =>
          json.asString match {
            case Some(value) =>
              BaboonBinTools.writeByteString(writer, ByteString.parseHex(value))
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("bytes", json))
          }

        case TypeId.Builtins.uid =>
          json.asString match {
            case Some(str) =>
              F.fromEither(Try(UUID.fromString(str)).toEither.left.map(_ => RuntimeCodecIssue.InvalidUuidString(str): BaboonIssue)).map {
                uuid =>
                  BaboonBinTools.writeUid(writer, uuid)
              }
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("uid", json))
          }

        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          json.asString match {
            case Some(value) =>
              // Parse tolerates both "Z" and "±HH:MM" forms (decoders must accept either per M28-N01
              // forward-compat clause). OffsetDateTime.parse(value) uses ISO_OFFSET_DATE_TIME, which
              // accepts both — no need to use a per-type formatter on the parse side.
              F.fromEither(Try(OffsetDateTime.parse(value)).toEither.left.map(_ => RuntimeCodecIssue.InvalidTimestampString(value): BaboonIssue)).map {
                offsetDt =>
                  BaboonBinTools.writeTimestamp(writer, offsetDt)
              }
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("timestamp", json))
          }

        case other => F.fail(RuntimeCodecIssue.UnsupportedBuiltinScalar(other))
      }
    }

    private def parseInteger(text: String): Option[BigInt] = Try(BigInt(text)).toOption

    /** UEBA stores every integer as its two's-complement low `width` bytes, so unsigned values above the signed range narrow as such. */
    private def writeInteger(kind: IntegerKind, value: BigInt, writer: LEDataOutputStream): Unit = {
      kind.width match {
        case 1    => writer.writeByte(value.toInt)
        case 2    => writer.writeShort(value.toInt)
        case 4    => writer.writeInt(value.toInt)
        case Wide => writer.writeLong(value.longValue)
      }
    }

    private def decodeBuiltinScalar(id: TypeId.BuiltinScalar, reader: LEDataInputStream): F[BaboonIssue, Json] = {
      id match {
        case TypeId.Builtins.bit => F.pure(Json.fromBoolean(reader.readBoolean()))
        case TypeId.Builtins.i08 => F.pure(Json.fromInt(reader.readByte().toInt))
        case TypeId.Builtins.i16 => F.pure(Json.fromInt(reader.readShort().toInt))
        case TypeId.Builtins.i32 => F.pure(Json.fromInt(reader.readInt()))
        // 64-bit integers go on the wire as decimal strings, matching every backend
        // (docs/json-codecs.md, "64-bit integers").
        case TypeId.Builtins.i64 => F.pure(Json.fromString(reader.readLong().toString))
        case TypeId.Builtins.u08 => F.pure(Json.fromInt(reader.readByte() & 0xFF))
        case TypeId.Builtins.u16 => F.pure(Json.fromInt(reader.readShort() & 0xFFFF))
        case TypeId.Builtins.u32 => F.pure(Json.fromLong(reader.readInt() & 0xFFFFFFFFL))
        case TypeId.Builtins.u64 =>
          val value = reader.readLong()
          // Unsigned decimal string: the upper half of the u64 range has no signed 64-bit
          // rendering any other backend would read back as the same value.
          F.pure(Json.fromString((if (value < 0) BigInt(value) + (BigInt(1) << 64) else BigInt(value)).toString))
        case TypeId.Builtins.f32  => F.pure(Json.fromFloatOrString(reader.readFloat()))
        case TypeId.Builtins.f64  => F.pure(Json.fromDoubleOrString(reader.readDouble()))
        case TypeId.Builtins.f128 => F.pure(Json.fromBigDecimal(BaboonBinTools.readBigDecimal(reader)))
        case TypeId.Builtins.str =>
          F.pure(Json.fromString(BaboonBinTools.readString(reader)))
        case TypeId.Builtins.bytes =>
          F.pure(Json.fromString(BaboonBinTools.readByteString(reader).toHexString))
        case TypeId.Builtins.uid =>
          F.pure(Json.fromString(BaboonBinTools.readUid(reader).toString))
        case TypeId.Builtins.tsu =>
          val offsetDt = BaboonBinTools.readTimestamp(reader)
          F.pure(Json.fromString(offsetDt.format(tsuFormatter)))
        case TypeId.Builtins.tso =>
          val offsetDt = BaboonBinTools.readTimestamp(reader)
          F.pure(Json.fromString(offsetDt.format(tsoFormatter)))
        case other => F.fail(RuntimeCodecIssue.UnsupportedBuiltinScalar(other))
      }
    }

    // Constructor (collection) encoding/decoding
    private def encodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], json: Json, writer: LEDataOutputStream, mode: EncodeMode)
      : F[BaboonIssue, Unit] = {
      id match {
        case TypeId.Builtins.opt =>
          if (json.isNull) {
            writer.writeByte(0)
            F.unit
          } else {
            writer.writeByte(1)
            encodeTypeRef(dom, args.head, json, writer, mode)
          }

        case TypeId.Builtins.lst =>
          json.asArray match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonArray("list", json))
            case Some(arr) =>
              writer.writeInt(arr.size)
              F.traverse_(arr)(elem => encodeTypeRef(dom, args.head, elem, writer, mode))
          }

        case TypeId.Builtins.set =>
          json.asArray match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonArray("set", json))
            case Some(arr) =>
              writer.writeInt(arr.size)
              F.traverse_(arr)(elem => encodeTypeRef(dom, args.head, elem, writer, mode))
          }

        case TypeId.Builtins.map =>
          json.asObject match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonObject("map", json))
            case Some(obj) =>
              writer.writeInt(obj.size)
              F.traverse_(obj.toList) {
                case (key, value) =>
                  for {
                    _ <- encodeMapKey(dom, args.head, key, writer)
                    _ <- encodeTypeRef(dom, args.last, value, writer, mode)
                  } yield ()
              }
          }

        case other => F.fail(RuntimeCodecIssue.UnsupportedCollectionType(other))
      }
    }

    private def decodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], reader: LEDataInputStream): F[BaboonIssue, Json] = {
      id match {
        case TypeId.Builtins.opt =>
          val flag = reader.readByte()
          if (flag == 0) {
            F.pure(Json.Null)
          } else {
            decodeTypeRef(dom, args.head, reader)
          }

        case TypeId.Builtins.lst =>
          val count = reader.readInt()
          F.traverse(0 until count)(_ => decodeTypeRef(dom, args.head, reader)).map(elements => Json.arr(elements*))

        case TypeId.Builtins.set =>
          val count = reader.readInt()
          F.traverse(0 until count)(_ => decodeTypeRef(dom, args.head, reader)).map(elements => Json.arr(elements*))

        case TypeId.Builtins.map =>
          val count = reader.readInt()
          F.traverse(0 until count) {
            _ =>
              for {
                key   <- decodeMapKey(dom, args.head, reader)
                value <- decodeTypeRef(dom, args.last, reader)
              } yield (key, value)
          }.map(pairs => Json.obj(pairs*))

        case other => F.fail(RuntimeCodecIssue.UnsupportedCollectionType(other))
      }
    }

    // Map key encoding/decoding - keys need special handling
    private def encodeMapKey(dom: Domain, keyType: TypeRef, key: String, writer: LEDataOutputStream): F[BaboonIssue, Unit] = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              BaboonBinTools.writeString(writer, key)
              F.unit
            case TypeId.Builtins.bytes =>
              BaboonBinTools.writeByteString(writer, ByteString.parseHex(key))
              F.unit
            case TypeId.Builtins.bit =>
              writer.writeBoolean(key.toBoolean)
              F.unit
            case integer if IntegerKinds.contains(integer) =>
              val kind = IntegerKinds(integer)
              parseInteger(key).filter(kind.contains) match {
                case Some(value) =>
                  writeInteger(kind, value, writer)
                  F.unit
                case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber(integer.name.name, Json.fromString(key)))
              }
            case TypeId.Builtins.f32 =>
              writer.writeFloat(key.toFloat)
              F.unit
            case TypeId.Builtins.f64 =>
              writer.writeDouble(key.toDouble)
              F.unit
            case TypeId.Builtins.f128 =>
              BaboonBinTools.writeString(writer, key)
              F.unit
            case TypeId.Builtins.uid =>
              F.fromEither(Try(UUID.fromString(key)).toEither.left.map(_ => RuntimeCodecIssue.InvalidUuidString(key): BaboonIssue)).map {
                uuid => BaboonBinTools.writeUid(writer, uuid)
              }
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Encode timestamp as binary: ticks + offset + kind
              writer.writeLong(0L)
              writer.writeLong(0L)
              writer.writeByte(0)
              F.unit
            case other =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(other)))
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          typedef match {
            case enum: Typedef.Enum =>
              encodeEnum(dom, enum, Json.fromString(key), writer)
            case f: Typedef.Foreign =>
              f.runtimeMapping match {
                case Some(typeRef) => encodeMapKey(dom, typeRef, key, writer)
                case None          =>
                  // M19/Q-M19-7: foreign types with no runtime mapping are accepted at face value;
                  // the user provides the toString/parse codec. The cross-format facade encodes the
                  // JSON-side canonical string verbatim into the UEBA binary.
                  BaboonBinTools.writeString(writer, key)
                  F.unit
              }
            // M19/PR-59: relax to accept eligible single-primitive-field wrappers and `id` types.
            // The validator (`checkUserMapKeysEligibility`) gates eligibility; this arm trusts it.
            case d: Typedef.Dto if d.isIdentifier =>
              if (d.fields.size == 1) {
                // Single-primitive id: recurse on inner field's type. The JSON-side key is the
                // canonical primitive string — no parser machinery needed.
                encodeMapKey(dom, d.fields.head.tpe, key, writer)
              } else {
                // Multi-field id (Q-M19-6): the JSON-side key is the parser-canonical
                // `<Name>:<ver>#fields:values:{nested}` string. The cross-format facade writes
                // it verbatim to UEBA; PR-60 emits the proper per-language parseRepr/toString
                // wiring on the codegen side.
                BaboonBinTools.writeString(writer, key)
                F.unit
              }
            case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
              // Single-field non-id wrapper: recurse on inner field's type.
              encodeMapKey(dom, d.fields.head.tpe, key, writer)
            case _ =>
              // Defensive: validator should have rejected anything else (Q-M19-8).
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(u)))
          }
        case _ =>
          F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(keyType))
      }
    }

    private def decodeMapKey(dom: Domain, keyType: TypeRef, reader: LEDataInputStream): F[BaboonIssue, String] = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              F.pure(BaboonBinTools.readString(reader))
            case TypeId.Builtins.bytes =>
              F.pure(BaboonBinTools.readByteString(reader).toHexString)
            case TypeId.Builtins.bit =>
              F.pure(reader.readBoolean().toString)
            case TypeId.Builtins.i08 =>
              F.pure(reader.readByte().toString)
            case TypeId.Builtins.i16 =>
              F.pure(reader.readShort().toString)
            case TypeId.Builtins.i32 =>
              F.pure(reader.readInt().toString)
            case TypeId.Builtins.i64 =>
              F.pure(reader.readLong().toString)
            case TypeId.Builtins.u08 =>
              F.pure((reader.readByte() & 0xFF).toString)
            case TypeId.Builtins.u16 =>
              F.pure((reader.readShort() & 0xFFFF).toString)
            case TypeId.Builtins.u32 =>
              F.pure((reader.readInt() & 0xFFFFFFFFL).toString)
            case TypeId.Builtins.u64 =>
              val value = reader.readLong()
              F.pure(
                if (value < 0) {
                  BigInt(value).+(BigInt(1) << 64).toString
                } else {
                  value.toString
                }
              )
            case TypeId.Builtins.f32 =>
              F.pure(reader.readFloat().toString)
            case TypeId.Builtins.f64 =>
              F.pure(reader.readDouble().toString)
            case TypeId.Builtins.f128 =>
              F.pure(BaboonBinTools.readString(reader))
            case TypeId.Builtins.uid =>
              F.pure(BaboonBinTools.readUid(reader).toString)
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Read timestamp: ticks + offset + kind = 17 bytes
              reader.readLong() // ticks (unused)
              reader.readLong() // offsetTicks (unused)
              reader.readByte() // kind (unused)
              F.pure("1970-01-01T00:00:00Z")
            case other =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(other)))
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          typedef match {
            case enum: Typedef.Enum =>
              decodeEnum(dom, enum, reader).flatMap {
                json =>
                  json.asString match {
                    case Some(s) => F.pure(s)
                    case None    => F.fail(RuntimeCodecIssue.CannotDecodeType(u, "Failed to decode enum key as string"))
                  }
              }
            case f: Typedef.Foreign =>
              f.runtimeMapping match {
                case Some(typeRef) => decodeMapKey(dom, typeRef, reader)
                case None          =>
                  // M19/Q-M19-7: foreign types with no runtime mapping decode as a verbatim string.
                  F.pure(BaboonBinTools.readString(reader))
              }
            // M19/PR-59 mirror of encodeMapKey relaxation.
            case d: Typedef.Dto if d.isIdentifier =>
              if (d.fields.size == 1) {
                decodeMapKey(dom, d.fields.head.tpe, reader)
              } else {
                // Multi-field id: read the canonical-toString form back as a verbatim string.
                F.pure(BaboonBinTools.readString(reader))
              }
            case d: Typedef.Dto if d.fields.size == 1 && d.contracts.isEmpty =>
              decodeMapKey(dom, d.fields.head.tpe, reader)
            case _ =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(u)))
          }
        case _ =>
          F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(keyType))
      }
    }

  }
}

package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, RuntimeCodecIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.util.Try

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2]() extends BaboonRuntimeCodec[F] {
    private val F = Error2[F]
    private val enquiries = new BaboonEnquiries.BaboonEnquiriesImpl()

    // .NET epoch is 0001-01-01T00:00:00, Unix epoch is 1970-01-01T00:00:00
    // Difference in milliseconds: 62135596800000L
    private val DotNetEpochOffsetMs = 62135596800000L

    // ISO 8601 formatters for timestamps with exactly 3 fractional digits (matching C# format)
    private val isoFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSxxx")

    // Helper methods for .NET decimal encoding/decoding
    private def encodeDecimal(value: BigDecimal, writer: DataOutputStream): Unit = {
      // .NET decimal is 16 bytes: lo (int32), mid (int32), hi (int32), flags (int32)
      // flags contains: sign bit (bit 31) and scale (bits 16-23)
      val unscaled = value.underlying().unscaledValue()
      val scale    = value.scale

      // Get the 96-bit unscaled value as 3 x 32-bit integers
      val lo  = unscaled.intValue()
      val mid = unscaled.shiftRight(32).intValue()
      val hi  = unscaled.shiftRight(64).intValue()

      // Build flags: sign in bit 31, scale in bits 16-23
      val sign  = if (unscaled.signum() < 0) 0x80000000 else 0
      val flags = sign | (scale << 16)

      writeIntLE(lo, writer)
      writeIntLE(mid, writer)
      writeIntLE(hi, writer)
      writeIntLE(flags, writer)
    }

    private def decodeDecimal(reader: DataInputStream): BigDecimal = {
      // Read .NET decimal: lo, mid, hi, flags (all int32 little-endian)
      val lo    = readIntLE(reader)
      val mid   = readIntLE(reader)
      val hi    = readIntLE(reader)
      val flags = readIntLE(reader)

      // Extract scale (bits 16-23) and sign (bit 31)
      val scale    = (flags >> 16) & 0xFF
      val negative = (flags & 0x80000000) != 0

      // Reconstruct the 96-bit unscaled value
      val loLong  = lo.toLong & 0xFFFFFFFFL
      val midLong = mid.toLong & 0xFFFFFFFFL
      val hiLong  = hi.toLong & 0xFFFFFFFFL

      val unscaled = BigInt(hiLong) << 64 | BigInt(midLong) << 32 | BigInt(loLong)
      val value    = if (negative) -unscaled else unscaled

      BigDecimal(value, scale)
    }

    override def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      val input   = new DataInputStream(new ByteArrayInputStream(data.toArray))

      typedef match {
        case u: DomainMember.User => decodeUserType(dom, u.defn, input)
        case _ => F.fail(RuntimeCodecIssue.UnexpectedDomainMemberType(typedef.id, s"Expected User type, got: ${typedef.getClass.getSimpleName}"))
      }
    }

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]] = {
      val dom     = getDom(family, pkg, version)
      val typedef = getDef(dom, idString)
      val output  = new ByteArrayOutputStream()
      val writer  = new DataOutputStream(output)

      typedef match {
        case u: DomainMember.User =>
          encodeUserType(dom, u.defn, json, writer, indexed).map {
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
    private def encodeUserType(dom: Domain, typedef: Typedef.User, json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      typedef match {
        case dto: Typedef.Dto    => encodeDto(dom, dto, json, writer, indexed)
        case enum: Typedef.Enum  => encodeEnum(dom, enum, json, writer)
        case adt: Typedef.Adt    => encodeAdt(dom, adt, json, writer, indexed)
        case _: Typedef.Foreign  => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Foreign types cannot be encoded"))
        case _: Typedef.Service  => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Service types cannot be encoded"))
        case _: Typedef.Contract => F.fail(RuntimeCodecIssue.CannotEncodeType(typedef.id, "Contract types cannot be encoded"))
      }
    }

    // Decode user-defined types
    private def decodeUserType(dom: Domain, typedef: Typedef.User, reader: DataInputStream): F[BaboonIssue, Json] = {
      typedef match {
        case dto: Typedef.Dto    => decodeDto(dom, dto, reader)
        case enum: Typedef.Enum  => decodeEnum(dom, enum, reader)
        case adt: Typedef.Adt    => decodeAdt(dom, adt, reader)
        case _: Typedef.Foreign  => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Foreign types cannot be decoded"))
        case _: Typedef.Service  => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Service types cannot be decoded"))
        case _: Typedef.Contract => F.fail(RuntimeCodecIssue.CannotDecodeType(typedef.id, "Contract types cannot be decoded"))
      }
    }

    // DTO encoding/decoding
    private def encodeDto(dom: Domain, dto: Typedef.Dto, json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      json.asObject match {
        case None => F.fail(RuntimeCodecIssue.ExpectedJsonObject(dto.id.toString, json))
        case Some(obj) =>
          // Write header byte
          val header: Byte = if (indexed) 1 else 0
          writer.writeByte(header)

          if (indexed) {
            // In indexed mode: collect field data in buffer, write index, then data
            val fieldDataBuffer = new ByteArrayOutputStream()
            val fieldDataWriter = new DataOutputStream(fieldDataBuffer)
            val indexEntries = scala.collection.mutable.ArrayBuffer[(Int, Int)]()

            F.foldLeft(dto.fields)(0) {
              case (offset, field) =>
                val fieldJson = obj(field.name.name).getOrElse(Json.Null)

                if (enquiries.uebaLen(dom.defs.meta.nodes, field.tpe).isVariable) {
                  // Variable-length field: record position, write to buffer, record length
                  val before = fieldDataBuffer.size()
                  encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, indexed).map {
                    _ =>
                      val after  = fieldDataBuffer.size()
                      val length = after - before
                      indexEntries += ((offset, length))
                      after
                  }
                } else {
                  // Fixed-length field: just write to buffer
                  encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, indexed).map {
                    _ => fieldDataBuffer.size()
                  }
                }
            }.flatMap {
              _ =>
                fieldDataWriter.flush()

                // Write index entries (offset and length for each variable-length field)
                indexEntries.foreach {
                  case (off, len) =>
                    writeIntLE(off, writer)
                    writeIntLE(len, writer)
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
                encodeTypeRef(dom, field.tpe, fieldJson, writer, indexed)
            }
          }
      }
    }

    private def decodeDto(dom: Domain, dto: Typedef.Dto, reader: DataInputStream): F[BaboonIssue, Json] = {
      // Read header byte
      val header = reader.readByte()

      // Check if indices are used (bit 0 set)
      val useIndices = (header & 1) != 0

      if (useIndices) {
        // Read and skip index entries
        val varLenFieldCount = dto.fields.count(f => enquiries.uebaLen(dom.defs.meta.nodes, f.tpe).isVariable)
        (0 until varLenFieldCount).foreach {
          _ =>
            readIntLE(reader) // offset (ignore)
            readIntLE(reader) // length (ignore)
        }
      }

      // Decode each field (data is in the same order regardless of indexed mode)
      F.traverse(dto.fields) {
        field =>
          decodeTypeRef(dom, field.tpe, reader).map {
            value => (field.name.name, value)
          }
      }.map(fields => Json.obj(fields *))
    }

    // Enum encoding/decoding
    private def encodeEnum(@annotation.unused dom: Domain, enum: Typedef.Enum, json: Json, writer: DataOutputStream): F[BaboonIssue, Unit] = {
      json.asString match {
        case None => F.fail(RuntimeCodecIssue.ExpectedJsonString(enum.id.toString, json))
        case Some(s) =>
          val str = s.toLowerCase.trim
          val idx = enum.members.toList.indexWhere(_.name.toLowerCase == str)
          if (idx < 0) {
            F.fail(RuntimeCodecIssue.UnknownEnumValue(enum.id, str, enum.members.toList.map(_.name)))
          } else {
            writer.writeByte(idx)
            F.unit
          }
      }
    }

    private def decodeEnum(@annotation.unused dom: Domain, enum: Typedef.Enum, reader: DataInputStream): F[BaboonIssue, Json] = {
      val idx = reader.readByte() & 0xFF

      if (idx >= enum.members.size) {
        F.fail(RuntimeCodecIssue.InvalidEnumIndex(enum.id, idx, enum.members.size - 1))
      } else {
        F.pure(Json.fromString(enum.members.toList(idx).name))
      }
    }

    // ADT encoding/decoding
    private def encodeAdt(dom: Domain, adt: Typedef.Adt, json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      json.asObject match {
        case None => F.fail(RuntimeCodecIssue.ExpectedJsonObject(adt.id.toString, json))
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
              encodeUserType(dom, branchDef, branchValue, writer, indexed)
            }
          }
      }
    }

    private def decodeAdt(dom: Domain, adt: Typedef.Adt, reader: DataInputStream): F[BaboonIssue, Json] = {
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
    private def encodeTypeRef(dom: Domain, tpe: TypeRef, json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      tpe match {
        case TypeRef.Scalar(id)            => encodeScalar(dom, id, json, writer, indexed)
        case TypeRef.Constructor(id, args) => encodeConstructor(dom, id, args.toList, json, writer, indexed)
      }
    }

    private def decodeTypeRef(dom: Domain, tpe: TypeRef, reader: DataInputStream): F[BaboonIssue, Json] = {
      tpe match {
        case TypeRef.Scalar(id)            => decodeScalar(dom, id, reader)
        case TypeRef.Constructor(id, args) => decodeConstructor(dom, id, args.toList, reader)
      }
    }

    // Scalar encoding/decoding
    private def encodeScalar(dom: Domain, id: TypeId.Scalar, json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      id match {
        case s: TypeId.BuiltinScalar => encodeBuiltinScalar(s, json, writer)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          encodeUserType(dom, typedef, json, writer, indexed)
      }
    }

    private def decodeScalar(dom: Domain, id: TypeId.Scalar, reader: DataInputStream): F[BaboonIssue, Json] = {
      id match {
        case s: TypeId.BuiltinScalar => decodeBuiltinScalar(s, reader)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          decodeUserType(dom, typedef, reader)
      }
    }

    // Builtin scalar encoding/decoding
    private def encodeBuiltinScalar(id: TypeId.BuiltinScalar, json: Json, writer: DataOutputStream): F[BaboonIssue, Unit] = {
      id match {
        case TypeId.Builtins.bit =>
          json.asBoolean match {
            case Some(value) =>
              writer.writeBoolean(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonBoolean(json))
          }

        case TypeId.Builtins.i08 =>
          json.asNumber.flatMap(_.toLong).map(_.toByte) match {
            case Some(value) =>
              writer.writeByte(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("i08", json))
          }

        case TypeId.Builtins.i16 =>
          json.asNumber.flatMap(_.toLong).map(_.toShort) match {
            case Some(value) =>
              writeShortLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("i16", json))
          }

        case TypeId.Builtins.i32 =>
          json.asNumber.flatMap(_.toInt) match {
            case Some(value) =>
              writeIntLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("i32", json))
          }

        case TypeId.Builtins.i64 =>
          json.asNumber.flatMap(_.toLong) match {
            case Some(value) =>
              writeLongLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("i64", json))
          }

        case TypeId.Builtins.u08 =>
          json.asNumber.flatMap(_.toLong).map(_.toByte) match {
            case Some(value) =>
              writer.writeByte(value)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("u08", json))
          }

        case TypeId.Builtins.u16 =>
          json.asNumber.flatMap(_.toLong).map(_.toShort) match {
            case Some(value) =>
              writeShortLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("u16", json))
          }

        case TypeId.Builtins.u32 =>
          json.asNumber.flatMap(_.toInt) match {
            case Some(value) =>
              writeIntLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("u32", json))
          }

        case TypeId.Builtins.u64 =>
          json.asNumber.flatMap(_.toLong) match {
            case Some(value) =>
              writeLongLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("u64", json))
          }

        case TypeId.Builtins.f32 =>
          json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble.toFloat)) match {
            case Some(value) =>
              writeFloatLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f32", json))
          }

        case TypeId.Builtins.f64 =>
          json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble)) match {
            case Some(value) =>
              writeDoubleLE(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f64", json))
          }

        case TypeId.Builtins.f128 =>
          json.asNumber.flatMap(_.toBigDecimal) match {
            case Some(value) =>
              encodeDecimal(value, writer)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonNumber("f128", json))
          }

        case TypeId.Builtins.str =>
          json.asString match {
            case Some(value) =>
              val bytes = value.getBytes(StandardCharsets.UTF_8)
              write7BitEncodedInt(bytes.length, writer)
              writer.write(bytes)
              F.unit
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("str", json))
          }

        case TypeId.Builtins.uid =>
          json.asString match {
            case Some(str) =>
              F.fromEither(Try(UUID.fromString(str)).toEither.left.map(_ => RuntimeCodecIssue.InvalidUuidString(str): BaboonIssue)).map {
                uuid =>
                  writer.write(toBytes(uuid))
              }
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("uid", json))
          }

        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          json.asString match {
            case Some(value) =>
              F.fromEither(Try(OffsetDateTime.parse(value, isoFormatter)).toEither.left.map(_ => RuntimeCodecIssue.InvalidTimestampString(value): BaboonIssue)).map {
                offsetDt =>
                  val epochMs       = offsetDt.toInstant.toEpochMilli
                  val dotNetTicksMs = epochMs + DotNetEpochOffsetMs
                  val offsetMs      = offsetDt.getOffset.getTotalSeconds * 1000L
                  val kind          = if (offsetDt.getOffset.getTotalSeconds == 0) 1.toByte else 0.toByte
                  writeLongLE(dotNetTicksMs, writer)
                  writeLongLE(offsetMs, writer)
                  writer.writeByte(kind)
              }
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonString("timestamp", json))
          }

        case other => F.fail(RuntimeCodecIssue.UnsupportedBuiltinScalar(other))
      }
    }

    private def decodeBuiltinScalar(id: TypeId.BuiltinScalar, reader: DataInputStream): F[BaboonIssue, Json] = {
      id match {
        case TypeId.Builtins.bit => F.pure(Json.fromBoolean(reader.readBoolean()))
        case TypeId.Builtins.i08 => F.pure(Json.fromInt(reader.readByte()))
        case TypeId.Builtins.i16 => F.pure(Json.fromInt(readShortLE(reader)))
        case TypeId.Builtins.i32 => F.pure(Json.fromInt(readIntLE(reader)))
        case TypeId.Builtins.i64 => F.pure(Json.fromLong(readLongLE(reader)))
        case TypeId.Builtins.u08 => F.pure(Json.fromInt(reader.readByte() & 0xFF))
        case TypeId.Builtins.u16 => F.pure(Json.fromInt(readShortLE(reader) & 0xFFFF))
        case TypeId.Builtins.u32 => F.pure(Json.fromLong(readIntLE(reader) & 0xFFFFFFFFL))
        case TypeId.Builtins.u64 =>
          val value = readLongLE(reader)
          F.pure(
            if (value < 0) {
              // Convert to unsigned BigInt
              Json.fromBigInt(BigInt(value & Long.MaxValue) + BigInt(Long.MaxValue) + 1)
            } else {
              Json.fromLong(value)
            }
          )
        case TypeId.Builtins.f32 => F.pure(Json.fromFloatOrString(readFloatLE(reader)))
        case TypeId.Builtins.f64 => F.pure(Json.fromDoubleOrString(readDoubleLE(reader)))
        case TypeId.Builtins.f128 => F.pure(Json.fromBigDecimal(decodeDecimal(reader)))
        case TypeId.Builtins.str =>
          val length = read7BitEncodedInt(reader)
          val bytes  = new Array[Byte](length)
          reader.readFully(bytes)
          F.pure(Json.fromString(new String(bytes, StandardCharsets.UTF_8)))
        case TypeId.Builtins.uid =>
          val bytes = new Array[Byte](16)
          reader.readFully(bytes)
          F.pure(Json.fromString(fromBytes(bytes).toString))
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          // Read timestamp: 8 bytes (ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
          val dotNetTicksMs = readLongLE(reader)
          val offsetMs      = readLongLE(reader)
          reader.readByte() // kind (unused)

          // Convert .NET ticks (in ms) to Unix epoch milliseconds
          val epochMs       = dotNetTicksMs - DotNetEpochOffsetMs
          val offsetSeconds = (offsetMs / 1000).toInt
          val offset        = ZoneOffset.ofTotalSeconds(offsetSeconds)
          val instant       = Instant.ofEpochMilli(epochMs)
          val offsetDt      = OffsetDateTime.ofInstant(instant, offset)
          F.pure(Json.fromString(offsetDt.format(isoFormatter)))
        case other => F.fail(RuntimeCodecIssue.UnsupportedBuiltinScalar(other))
      }
    }

    // Constructor (collection) encoding/decoding
    private def encodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], json: Json, writer: DataOutputStream, indexed: Boolean): F[BaboonIssue, Unit] = {
      id match {
        case TypeId.Builtins.opt =>
          if (json.isNull) {
            writer.writeByte(0)
            F.unit
          } else {
            writer.writeByte(1)
            encodeTypeRef(dom, args.head, json, writer, indexed)
          }

        case TypeId.Builtins.lst =>
          json.asArray match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonArray("list", json))
            case Some(arr) =>
              writeIntLE(arr.size, writer)
              F.traverse_(arr)(elem => encodeTypeRef(dom, args.head, elem, writer, indexed))
          }

        case TypeId.Builtins.set =>
          json.asArray match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonArray("set", json))
            case Some(arr) =>
              writeIntLE(arr.size, writer)
              F.traverse_(arr)(elem => encodeTypeRef(dom, args.head, elem, writer, indexed))
          }

        case TypeId.Builtins.map =>
          json.asObject match {
            case None => F.fail(RuntimeCodecIssue.ExpectedJsonObject("map", json))
            case Some(obj) =>
              writeIntLE(obj.size, writer)
              F.traverse_(obj.toList) {
                case (key, value) =>
                  for {
                    _ <- encodeMapKey(dom, args.head, key, writer)
                    _ <- encodeTypeRef(dom, args.last, value, writer, indexed)
                  } yield ()
              }
          }

        case other => F.fail(RuntimeCodecIssue.UnsupportedCollectionType(other))
      }
    }

    private def decodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], reader: DataInputStream): F[BaboonIssue, Json] = {
      id match {
        case TypeId.Builtins.opt =>
          val flag = reader.readByte()
          if (flag == 0) {
            F.pure(Json.Null)
          } else {
            decodeTypeRef(dom, args.head, reader)
          }

        case TypeId.Builtins.lst =>
          val count = readIntLE(reader)
          F.traverse(0 until count)(_ => decodeTypeRef(dom, args.head, reader)).map(elements => Json.arr(elements *))

        case TypeId.Builtins.set =>
          val count = readIntLE(reader)
          F.traverse(0 until count)(_ => decodeTypeRef(dom, args.head, reader)).map(elements => Json.arr(elements *))

        case TypeId.Builtins.map =>
          val count = readIntLE(reader)
          F.traverse(0 until count) {
            _ =>
              for {
                key   <- decodeMapKey(dom, args.head, reader)
                value <- decodeTypeRef(dom, args.last, reader)
              } yield (key, value)
          }.map(pairs => Json.obj(pairs *))

        case other => F.fail(RuntimeCodecIssue.UnsupportedCollectionType(other))
      }
    }

    // Map key encoding/decoding - keys need special handling
    private def encodeMapKey(dom: Domain, keyType: TypeRef, key: String, writer: DataOutputStream): F[BaboonIssue, Unit] = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              val bytes = key.getBytes(StandardCharsets.UTF_8)
              write7BitEncodedInt(bytes.length, writer)
              writer.write(bytes)
              F.unit
            case TypeId.Builtins.bit =>
              writer.writeBoolean(key.toBoolean)
              F.unit
            case TypeId.Builtins.i08 =>
              writer.writeByte(key.toByte)
              F.unit
            case TypeId.Builtins.i16 =>
              writeShortLE(key.toShort, writer)
              F.unit
            case TypeId.Builtins.i32 =>
              writeIntLE(key.toInt, writer)
              F.unit
            case TypeId.Builtins.i64 =>
              writeLongLE(key.toLong, writer)
              F.unit
            case TypeId.Builtins.u08 =>
              writer.writeByte(key.toByte)
              F.unit
            case TypeId.Builtins.u16 =>
              writeShortLE(key.toShort, writer)
              F.unit
            case TypeId.Builtins.u32 =>
              writeIntLE(key.toInt, writer)
              F.unit
            case TypeId.Builtins.u64 =>
              writeLongLE(key.toLong, writer)
              F.unit
            case TypeId.Builtins.f32 =>
              writeFloatLE(key.toFloat, writer)
              F.unit
            case TypeId.Builtins.f64 =>
              writeDoubleLE(key.toDouble, writer)
              F.unit
            case TypeId.Builtins.f128 =>
              val bytes = key.getBytes(StandardCharsets.UTF_8)
              write7BitEncodedInt(bytes.length, writer)
              writer.write(bytes)
              F.unit
            case TypeId.Builtins.uid =>
              F.fromEither(Try(UUID.fromString(key)).toEither.left.map(_ => RuntimeCodecIssue.InvalidUuidString(key): BaboonIssue)).map {
                uuid => writer.write(toBytes(uuid))
              }
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Encode timestamp as binary: ticks + offset + kind
              writeLongLE(0L, writer)
              writeLongLE(0L, writer)
              writer.writeByte(0)
              F.unit
            case other =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(other)))
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          // Assume enum or foreign type that can be represented as string
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          typedef match {
            case enum: Typedef.Enum =>
              encodeEnum(dom, enum, Json.fromString(key), writer)
            case _ =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(u)))
          }
        case _ =>
          F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(keyType))
      }
    }

    private def decodeMapKey(dom: Domain, keyType: TypeRef, reader: DataInputStream): F[BaboonIssue, String] = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              val length = read7BitEncodedInt(reader)
              val bytes  = new Array[Byte](length)
              reader.readFully(bytes)
              F.pure(new String(bytes, StandardCharsets.UTF_8))
            case TypeId.Builtins.bit =>
              F.pure(reader.readBoolean().toString)
            case TypeId.Builtins.i08 =>
              F.pure(reader.readByte().toString)
            case TypeId.Builtins.i16 =>
              F.pure(readShortLE(reader).toString)
            case TypeId.Builtins.i32 =>
              F.pure(readIntLE(reader).toString)
            case TypeId.Builtins.i64 =>
              F.pure(readLongLE(reader).toString)
            case TypeId.Builtins.u08 =>
              F.pure((reader.readByte() & 0xFF).toString)
            case TypeId.Builtins.u16 =>
              F.pure((readShortLE(reader) & 0xFFFF).toString)
            case TypeId.Builtins.u32 =>
              F.pure((readIntLE(reader) & 0xFFFFFFFFL).toString)
            case TypeId.Builtins.u64 =>
              val value = readLongLE(reader)
              F.pure(
                if (value < 0) {
                  BigInt(value).+(BigInt(1) << 64).toString
                } else {
                  value.toString
                }
              )
            case TypeId.Builtins.f32 =>
              F.pure(readFloatLE(reader).toString)
            case TypeId.Builtins.f64 =>
              F.pure(readDoubleLE(reader).toString)
            case TypeId.Builtins.f128 =>
              val length = read7BitEncodedInt(reader)
              val bytes  = new Array[Byte](length)
              reader.readFully(bytes)
              F.pure(new String(bytes, StandardCharsets.UTF_8))
            case TypeId.Builtins.uid =>
              val bytes = new Array[Byte](16)
              reader.readFully(bytes)
              F.pure(fromBytes(bytes).toString)
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Read timestamp: ticks + offset + kind = 17 bytes
              readLongLE(reader) // ticks (unused)
              readLongLE(reader) // offsetTicks (unused)
              reader.readByte()  // kind (unused)
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
            case _ =>
              F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(TypeRef.Scalar(u)))
          }
        case _ =>
          F.fail(RuntimeCodecIssue.UnsupportedMapKeyType(keyType))
      }
    }

    // Little-endian reading/writing (matching .NET BinaryWriter/BinaryReader)
    private def writeShortLE(value: Short, writer: DataOutputStream): Unit = {
      writer.writeByte((value & 0xFF).toByte)
      writer.writeByte(((value >> 8) & 0xFF).toByte)
    }

    private def readShortLE(reader: DataInputStream): Short = {
      val b0 = reader.readByte() & 0xFF
      val b1 = reader.readByte() & 0xFF
      ((b0 | (b1 << 8)) & 0xFFFF).toShort
    }

    private def writeIntLE(value: Int, writer: DataOutputStream): Unit = {
      writer.writeByte((value & 0xFF).toByte)
      writer.writeByte(((value >> 8) & 0xFF).toByte)
      writer.writeByte(((value >> 16) & 0xFF).toByte)
      writer.writeByte(((value >> 24) & 0xFF).toByte)
    }

    private def readIntLE(reader: DataInputStream): Int = {
      val b0 = reader.readByte() & 0xFF
      val b1 = reader.readByte() & 0xFF
      val b2 = reader.readByte() & 0xFF
      val b3 = reader.readByte() & 0xFF
      b0 | (b1 << 8) | (b2 << 16) | (b3 << 24)
    }

    private def writeLongLE(value: Long, writer: DataOutputStream): Unit = {
      writer.writeByte((value & 0xFF).toByte)
      writer.writeByte(((value >> 8) & 0xFF).toByte)
      writer.writeByte(((value >> 16) & 0xFF).toByte)
      writer.writeByte(((value >> 24) & 0xFF).toByte)
      writer.writeByte(((value >> 32) & 0xFF).toByte)
      writer.writeByte(((value >> 40) & 0xFF).toByte)
      writer.writeByte(((value >> 48) & 0xFF).toByte)
      writer.writeByte(((value >> 56) & 0xFF).toByte)
    }

    private def readLongLE(reader: DataInputStream): Long = {
      val b0 = reader.readByte() & 0xFFL
      val b1 = reader.readByte() & 0xFFL
      val b2 = reader.readByte() & 0xFFL
      val b3 = reader.readByte() & 0xFFL
      val b4 = reader.readByte() & 0xFFL
      val b5 = reader.readByte() & 0xFFL
      val b6 = reader.readByte() & 0xFFL
      val b7 = reader.readByte() & 0xFFL
      b0 | (b1 << 8) | (b2 << 16) | (b3 << 24) | (b4 << 32) | (b5 << 40) | (b6 << 48) | (b7 << 56)
    }

    private def writeFloatLE(value: Float, writer: DataOutputStream): Unit = {
      writeIntLE(java.lang.Float.floatToRawIntBits(value), writer)
    }

    private def readFloatLE(reader: DataInputStream): Float = {
      java.lang.Float.intBitsToFloat(readIntLE(reader))
    }

    private def writeDoubleLE(value: Double, writer: DataOutputStream): Unit = {
      writeLongLE(java.lang.Double.doubleToRawLongBits(value), writer)
    }

    private def readDoubleLE(reader: DataInputStream): Double = {
      java.lang.Double.longBitsToDouble(readLongLE(reader))
    }

    // 7-bit encoding/decoding (matching .NET BinaryWriter/BinaryReader)
    private def write7BitEncodedInt(value: Int, writer: DataOutputStream): Unit = {
      var v = value
      while (v >= 0x80) {
        writer.writeByte(((v & 0x7F) | 0x80).toByte)
        v >>>= 7
      }
      writer.writeByte((v & 0x7F).toByte)
    }

    private def read7BitEncodedInt(reader: DataInputStream): Int = {
      var result = 0
      var shift  = 0
      var b      = 0
      do {
        b = reader.readByte() & 0xFF
        result |= (b & 0x7F) << shift
        shift += 7
      } while ((b & 0x80) != 0)
      result
    }

    // UUID utilities
    private def toBytes(uuid: UUID): Array[Byte] = {
      val bb = java.nio.ByteBuffer.allocate(16)
      bb.putLong(uuid.getMostSignificantBits)
      bb.putLong(uuid.getLeastSignificantBits)
      bb.array()
    }

    private def fromBytes(bytes: Array[Byte]): UUID = {
      val bb   = java.nio.ByteBuffer.wrap(bytes)
      val high = bb.getLong
      val low  = bb.getLong
      new UUID(high, low)
    }
  }
}

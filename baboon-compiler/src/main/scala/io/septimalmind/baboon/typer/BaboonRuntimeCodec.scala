package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.language.SourceFilePosition

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.{ByteBuffer, ByteOrder}
import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}
import java.util.{Base64, UUID}
import scala.util.Try

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2]() extends BaboonRuntimeCodec[F] {
    private val F = Error2[F]

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
      F.fromEither {
        Try {
          val dom     = getDom(family, pkg, version)
          val typedef = getDef(dom, idString)
          val input   = new DataInputStream(new ByteArrayInputStream(data.toArray))

          typedef match {
            case u: DomainMember.User => decodeUserType(dom, u.defn, input)
            case _                    => throw new IllegalArgumentException(s"Unexpected domain member type: $typedef")
          }
        }.toEither.left.map {
          ex =>
            TranslationIssue.TranslationBug()(
              Issue.IssueContext(
                SourceFilePosition.unknown,
                ex,
              )
            )
        }
      }
    }

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json, indexed: Boolean): F[BaboonIssue, Vector[Byte]] = {
      F.fromEither {
        Try {
          val dom     = getDom(family, pkg, version)
          val typedef = getDef(dom, idString)
          val output  = new ByteArrayOutputStream()
          val writer  = new DataOutputStream(output)

          typedef match {
            case u: DomainMember.User =>
              encodeUserType(dom, u.defn, json, writer, indexed)
              writer.flush()
              Vector.from(output.toByteArray)
            case _ => throw new IllegalArgumentException(s"Unexpected domain member type: $typedef")
          }
        }.toEither.left.map {
          ex =>
            implicit val ctx: Issue.IssueContext =
              Issue.IssueContext(
                SourceFilePosition.unknown,
                ex,
              )
            BaboonIssue.Translation(io.septimalmind.baboon.parser.model.issues.TranslationIssue.TranslationBug())
        }
      }
    }

    private def getDef(dom: Domain, idString: String): DomainMember = {
      dom.defs.meta.nodes.map { case (k, v) => (k.toString, v) }(idString)
    }

    private def getDom(family: BaboonFamily, pkg: Pkg, version: Version): Domain = {
      family.domains(pkg).versions(version)
    }

    // Encode user-defined types
    private def encodeUserType(dom: Domain, typedef: Typedef.User, json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      typedef match {
        case dto: Typedef.Dto    => encodeDto(dom, dto, json, writer, indexed)
        case enum: Typedef.Enum  => encodeEnum(dom, enum, json, writer)
        case adt: Typedef.Adt    => encodeAdt(dom, adt, json, writer, indexed)
        case _: Typedef.Foreign  => throw new IllegalArgumentException(s"Foreign types cannot be encoded: ${typedef.id}")
        case _: Typedef.Service  => throw new IllegalArgumentException(s"Service types cannot be encoded: ${typedef.id}")
        case _: Typedef.Contract => throw new IllegalArgumentException(s"Contract types cannot be encoded: ${typedef.id}")
      }
    }

    // Decode user-defined types
    private def decodeUserType(dom: Domain, typedef: Typedef.User, reader: DataInputStream): Json = {
      typedef match {
        case dto: Typedef.Dto    => decodeDto(dom, dto, reader)
        case enum: Typedef.Enum  => decodeEnum(dom, enum, reader)
        case adt: Typedef.Adt    => decodeAdt(dom, adt, reader)
        case _: Typedef.Foreign  => throw new IllegalArgumentException(s"Foreign types cannot be decoded: ${typedef.id}")
        case _: Typedef.Service  => throw new IllegalArgumentException(s"Service types cannot be decoded: ${typedef.id}")
        case _: Typedef.Contract => throw new IllegalArgumentException(s"Contract types cannot be decoded: ${typedef.id}")
      }
    }

    // Helper to determine if a type requires variable-length encoding
    private def isVariableLength(dom: Domain, tpe: TypeRef): Boolean = tpe match {
      case TypeRef.Scalar(id) =>
        id match {
          case TypeId.Builtins.str => true
          case TypeId.Builtins.lst => true
          case TypeId.Builtins.set => true
          case TypeId.Builtins.map => true
          case TypeId.Builtins.opt => true
          case userId: TypeId.User =>
            // Look up the user type and check if it's variable-length
            val userType = dom.defs.meta.nodes(userId).asInstanceOf[DomainMember.User].defn
            userType match {
              case _: Typedef.Enum => false // Enums are fixed-length (1 byte)
              case _: Typedef.Adt  => true  // ADTs are variable-length (discriminator + branch)
              case dto: Typedef.Dto =>
                // DTO is variable-length if it has any variable-length fields
                dto.fields.exists(f => isVariableLength(dom, f.tpe))
              case _ => false
            }
          case _ => false
        }
      case _: TypeRef.Constructor => true // Collections are variable
    }

    // DTO encoding/decoding
    private def encodeDto(dom: Domain, dto: Typedef.Dto, json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      val obj = json.asObject.getOrElse(
        throw new IllegalArgumentException(s"Expected JSON object for DTO ${dto.id}, got: $json")
      )

      // Write header byte
      val header: Byte = if (indexed) 1 else 0
      writer.writeByte(header)

      if (indexed) {
        // In indexed mode: collect field data in buffer, write index, then data
        val fieldDataBuffer = new ByteArrayOutputStream()
        val fieldDataWriter = new DataOutputStream(fieldDataBuffer)

        // Track variable-length fields for index
        val indexEntries = scala.collection.mutable.ArrayBuffer[(Int, Int)]()
        var offset      = 0

        dto.fields.foreach {
          field =>
            val fieldJson = obj(field.name.name).getOrElse(Json.Null)

            if (isVariableLength(dom, field.tpe)) {
              // Variable-length field: record position, write to buffer, record length
              val before = fieldDataBuffer.size()
              encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, indexed)
              val after  = fieldDataBuffer.size()
              val length = after - before

              indexEntries += ((offset, length))
              offset = after
            } else {
              // Fixed-length field: just write to buffer
              encodeTypeRef(dom, field.tpe, fieldJson, fieldDataWriter, indexed)
              offset = fieldDataBuffer.size()
            }
        }

        fieldDataWriter.flush()

        // Write index entries (offset and length for each variable-length field)
        indexEntries.foreach {
          case (off, len) =>
            writeIntLE(off, writer)
            writeIntLE(len, writer)
        }

        // Write all field data
        writer.write(fieldDataBuffer.toByteArray)
      } else {
        // Compact mode: write fields directly
        dto.fields.foreach {
          field =>
            val fieldJson = obj(field.name.name).getOrElse(Json.Null)
            encodeTypeRef(dom, field.tpe, fieldJson, writer, indexed)
        }
      }
    }

    private def decodeDto(dom: Domain, dto: Typedef.Dto, reader: DataInputStream): Json = {
      // Read header byte
      val header = reader.readByte()

      // Check if indices are used (bit 0 set)
      val useIndices = (header & 1) != 0

      if (useIndices) {
        // Read and skip index entries
        val varLenFieldCount = dto.fields.count(f => isVariableLength(dom, f.tpe))
        (0 until varLenFieldCount).foreach {
          _ =>
            readIntLE(reader) // offset (ignore)
            readIntLE(reader) // length (ignore)
        }
      }

      // Decode each field (data is in the same order regardless of indexed mode)
      val fields = dto.fields.map {
        field =>
          val value = decodeTypeRef(dom, field.tpe, reader)
          (field.name.name, value)
      }

      Json.obj(fields *)
    }

    // Enum encoding/decoding
    private def encodeEnum(dom: Domain, enum: Typedef.Enum, json: Json, writer: DataOutputStream): Unit = {
      val str = json.asString
        .getOrElse(
          throw new IllegalArgumentException(s"Expected JSON string for enum ${enum.id}, got: $json")
        ).toLowerCase.trim

      val idx = enum.members.toList.indexWhere(_.name.toLowerCase == str)
      if (idx < 0) {
        throw new IllegalArgumentException(s"Unknown enum value '$str' for ${enum.id}")
      }

      writer.writeByte(idx)
    }

    private def decodeEnum(dom: Domain, enum: Typedef.Enum, reader: DataInputStream): Json = {
      val idx = reader.readByte() & 0xFF

      if (idx >= enum.members.size) {
        throw new IllegalArgumentException(s"Invalid enum index $idx for ${enum.id}, max is ${enum.members.size - 1}")
      }

      Json.fromString(enum.members.toList(idx).name)
    }

    // ADT encoding/decoding
    private def encodeAdt(dom: Domain, adt: Typedef.Adt, json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      val obj = json.asObject.getOrElse(
        throw new IllegalArgumentException(s"Expected JSON object for ADT ${adt.id}, got: $json")
      )

      // Find the branch - the object should have exactly one key
      val branches = obj.toList
      if (branches.length != 1) {
        throw new IllegalArgumentException(s"Expected exactly one branch for ADT ${adt.id}, got: ${branches.map(_._1)}")
      }

      val (branchName, branchValue) = branches.head
      val dataMembers               = adt.dataMembers(dom)

      val branchIdx = dataMembers.indexWhere(_.name.name == branchName)
      if (branchIdx < 0) {
        throw new IllegalArgumentException(s"Unknown ADT branch '$branchName' for ${adt.id}")
      }

      // Write discriminator
      writer.writeByte(branchIdx)

      // Encode the branch value
      val branchId  = dataMembers(branchIdx)
      val branchDef = dom.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
      encodeUserType(dom, branchDef, branchValue, writer, indexed)
    }

    private def decodeAdt(dom: Domain, adt: Typedef.Adt, reader: DataInputStream): Json = {
      val discriminator = reader.readByte() & 0xFF
      val dataMembers   = adt.dataMembers(dom)

      if (discriminator >= dataMembers.size) {
        throw new IllegalArgumentException(s"Invalid ADT discriminator $discriminator for ${adt.id}, max is ${dataMembers.size - 1}")
      }

      val branchId    = dataMembers(discriminator)
      val branchDef   = dom.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
      val branchValue = decodeUserType(dom, branchDef, reader)

      Json.obj(branchId.name.name -> branchValue)
    }

    // TypeRef encoding/decoding
    private def encodeTypeRef(dom: Domain, tpe: TypeRef, json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      tpe match {
        case TypeRef.Scalar(id)            => encodeScalar(dom, id, json, writer, indexed)
        case TypeRef.Constructor(id, args) => encodeConstructor(dom, id, args.toList, json, writer, indexed)
      }
    }

    private def decodeTypeRef(dom: Domain, tpe: TypeRef, reader: DataInputStream): Json = {
      tpe match {
        case TypeRef.Scalar(id)            => decodeScalar(dom, id, reader)
        case TypeRef.Constructor(id, args) => decodeConstructor(dom, id, args.toList, reader)
      }
    }

    // Scalar encoding/decoding
    private def encodeScalar(dom: Domain, id: TypeId.Scalar, json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      id match {
        case s: TypeId.BuiltinScalar => encodeBuiltinScalar(s, json, writer)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          encodeUserType(dom, typedef, json, writer, indexed)
      }
    }

    private def decodeScalar(dom: Domain, id: TypeId.Scalar, reader: DataInputStream): Json = {
      id match {
        case s: TypeId.BuiltinScalar => decodeBuiltinScalar(s, reader)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          decodeUserType(dom, typedef, reader)
      }
    }

    // Builtin scalar encoding/decoding
    private def encodeBuiltinScalar(id: TypeId.BuiltinScalar, json: Json, writer: DataOutputStream): Unit = {
      id match {
        case TypeId.Builtins.bit =>
          val value = json.asBoolean.getOrElse(throw new IllegalArgumentException(s"Expected boolean, got: $json"))
          writer.writeBoolean(value)

        case TypeId.Builtins.i08 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toByte).getOrElse(throw new IllegalArgumentException(s"Expected i08, got: $json"))
          writer.writeByte(value)

        case TypeId.Builtins.i16 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toShort).getOrElse(throw new IllegalArgumentException(s"Expected i16, got: $json"))
          writeShortLE(value, writer)

        case TypeId.Builtins.i32 =>
          val value = json.asNumber.flatMap(_.toInt).getOrElse(throw new IllegalArgumentException(s"Expected i32, got: $json"))
          writeIntLE(value, writer)

        case TypeId.Builtins.i64 =>
          val value = json.asNumber.flatMap(_.toLong).getOrElse(throw new IllegalArgumentException(s"Expected i64, got: $json"))
          writeLongLE(value, writer)

        case TypeId.Builtins.u08 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toByte).getOrElse(throw new IllegalArgumentException(s"Expected u08, got: $json"))
          writer.writeByte(value)

        case TypeId.Builtins.u16 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toShort).getOrElse(throw new IllegalArgumentException(s"Expected u16, got: $json"))
          writeShortLE(value, writer)

        case TypeId.Builtins.u32 =>
          val value = json.asNumber.flatMap(_.toInt).getOrElse(throw new IllegalArgumentException(s"Expected u32, got: $json"))
          writeIntLE(value, writer)

        case TypeId.Builtins.u64 =>
          val value = json.asNumber.flatMap(_.toLong).getOrElse(throw new IllegalArgumentException(s"Expected u64, got: $json"))
          writeLongLE(value, writer)

        case TypeId.Builtins.f32 =>
          val value = json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble.toFloat)).getOrElse(throw new IllegalArgumentException(s"Expected f32, got: $json"))
          writeFloatLE(value, writer)

        case TypeId.Builtins.f64 =>
          val value = json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble)).getOrElse(throw new IllegalArgumentException(s"Expected f64, got: $json"))
          writeDoubleLE(value, writer)

        case TypeId.Builtins.f128 =>
          val value = json.asNumber.flatMap(_.toBigDecimal).getOrElse(throw new IllegalArgumentException(s"Expected f128, got: $json"))
          encodeDecimal(value, writer)

        case TypeId.Builtins.str =>
          val value = json.asString.getOrElse(throw new IllegalArgumentException(s"Expected string, got: $json"))
          val bytes = value.getBytes(StandardCharsets.UTF_8)
          write7BitEncodedInt(bytes.length, writer)
          writer.write(bytes)

        case TypeId.Builtins.uid =>
          val value = json.asString.map(UUID.fromString).getOrElse(throw new IllegalArgumentException(s"Expected UUID string, got: $json"))
          writer.write(toBytes(value))

        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          val value        = json.asString.getOrElse(throw new IllegalArgumentException(s"Expected timestamp string, got: $json"))
          val offsetDt     = OffsetDateTime.parse(value, isoFormatter)
          val epochMs      = offsetDt.toInstant.toEpochMilli
          val dotNetTicksMs = epochMs + DotNetEpochOffsetMs
          val offsetMs     = offsetDt.getOffset.getTotalSeconds * 1000L
          val kind: Byte   = if (offsetDt.getOffset.getTotalSeconds == 0) 1.toByte else 0.toByte  // 1=UTC, 0=Unspecified
          writeLongLE(dotNetTicksMs, writer)
          writeLongLE(offsetMs, writer)
          writer.writeByte(kind)

        case other => throw new IllegalArgumentException(s"Unsupported builtin scalar: $other")
      }
    }

    private def decodeBuiltinScalar(id: TypeId.BuiltinScalar, reader: DataInputStream): Json = {
      id match {
        case TypeId.Builtins.bit => Json.fromBoolean(reader.readBoolean())
        case TypeId.Builtins.i08 => Json.fromInt(reader.readByte())
        case TypeId.Builtins.i16 => Json.fromInt(readShortLE(reader))
        case TypeId.Builtins.i32 => Json.fromInt(readIntLE(reader))
        case TypeId.Builtins.i64 => Json.fromLong(readLongLE(reader))
        case TypeId.Builtins.u08 => Json.fromInt(reader.readByte() & 0xFF)
        case TypeId.Builtins.u16 => Json.fromInt(readShortLE(reader) & 0xFFFF)
        case TypeId.Builtins.u32 => Json.fromLong(readIntLE(reader) & 0xFFFFFFFFL)
        case TypeId.Builtins.u64 =>
          val value = readLongLE(reader)
          if (value < 0) {
            // Convert to unsigned BigInt
            Json.fromBigInt(BigInt(value & Long.MaxValue) + BigInt(Long.MaxValue) + 1)
          } else {
            Json.fromLong(value)
          }
        case TypeId.Builtins.f32 => Json.fromFloatOrString(readFloatLE(reader))
        case TypeId.Builtins.f64 => Json.fromDoubleOrString(readDoubleLE(reader))
        case TypeId.Builtins.f128 =>
          Json.fromBigDecimal(decodeDecimal(reader))
        case TypeId.Builtins.str =>
          val length = read7BitEncodedInt(reader)
          val bytes  = new Array[Byte](length)
          reader.readFully(bytes)
          Json.fromString(new String(bytes, StandardCharsets.UTF_8))
        case TypeId.Builtins.uid =>
          val bytes = new Array[Byte](16)
          reader.readFully(bytes)
          Json.fromString(fromBytes(bytes).toString)
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          // Read timestamp: 8 bytes (ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
          val dotNetTicksMs = readLongLE(reader)
          val offsetMs     = readLongLE(reader)
          val kind         = reader.readByte()

          // Convert .NET ticks (in ms) to Unix epoch milliseconds
          val epochMs = dotNetTicksMs - DotNetEpochOffsetMs
          val offsetSeconds = (offsetMs / 1000).toInt
          val offset       = ZoneOffset.ofTotalSeconds(offsetSeconds)
          val instant      = Instant.ofEpochMilli(epochMs)
          val offsetDt     = OffsetDateTime.ofInstant(instant, offset)

          Json.fromString(offsetDt.format(isoFormatter))
        case other => throw new IllegalArgumentException(s"Unsupported builtin scalar: $other")
      }
    }

    // Constructor (collection) encoding/decoding
    private def encodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], json: Json, writer: DataOutputStream, indexed: Boolean): Unit = {
      id match {
        case TypeId.Builtins.opt =>
          if (json.isNull) {
            writer.writeByte(0)
          } else {
            writer.writeByte(1)
            encodeTypeRef(dom, args.head, json, writer, indexed)
          }

        case TypeId.Builtins.lst =>
          val arr = json.asArray.getOrElse(throw new IllegalArgumentException(s"Expected array for list, got: $json"))
          writeIntLE(arr.size, writer)
          arr.foreach(elem => encodeTypeRef(dom, args.head, elem, writer, indexed))

        case TypeId.Builtins.set =>
          val arr = json.asArray.getOrElse(throw new IllegalArgumentException(s"Expected array for set, got: $json"))
          writeIntLE(arr.size, writer)
          arr.foreach(elem => encodeTypeRef(dom, args.head, elem, writer, indexed))

        case TypeId.Builtins.map =>
          val obj = json.asObject.getOrElse(throw new IllegalArgumentException(s"Expected object for map, got: $json"))
          writeIntLE(obj.size, writer)
          obj.toList.foreach {
            case (key, value) =>
              // Encode key - for now assume keys can be encoded as strings
              encodeMapKey(dom, args.head, key, writer)
              encodeTypeRef(dom, args.last, value, writer, indexed)
          }

        case other => throw new IllegalArgumentException(s"Unsupported collection type: $other")
      }
    }

    private def decodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], reader: DataInputStream): Json = {
      id match {
        case TypeId.Builtins.opt =>
          val flag = reader.readByte()
          if (flag == 0) {
            Json.Null
          } else {
            decodeTypeRef(dom, args.head, reader)
          }

        case TypeId.Builtins.lst =>
          val count    = readIntLE(reader)
          val elements = (0 until count).map(_ => decodeTypeRef(dom, args.head, reader))
          Json.arr(elements *)

        case TypeId.Builtins.set =>
          val count    = readIntLE(reader)
          val elements = (0 until count).map(_ => decodeTypeRef(dom, args.head, reader))
          Json.arr(elements *)

        case TypeId.Builtins.map =>
          val count = readIntLE(reader)
          val pairs = (0 until count).map {
            _ =>
              val key   = decodeMapKey(dom, args.head, reader)
              val value = decodeTypeRef(dom, args.last, reader)
              (key, value)
          }
          Json.obj(pairs *)

        case other => throw new IllegalArgumentException(s"Unsupported collection type: $other")
      }
    }

    // Map key encoding/decoding - keys need special handling
    private def encodeMapKey(dom: Domain, keyType: TypeRef, key: String, writer: DataOutputStream): Unit = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              val bytes = key.getBytes(StandardCharsets.UTF_8)
              write7BitEncodedInt(bytes.length, writer)
              writer.write(bytes)
            case TypeId.Builtins.bit =>
              writer.writeBoolean(key.toBoolean)
            case TypeId.Builtins.i08 =>
              writer.writeByte(key.toByte)
            case TypeId.Builtins.i16 =>
              writeShortLE(key.toShort, writer)
            case TypeId.Builtins.i32 =>
              writeIntLE(key.toInt, writer)
            case TypeId.Builtins.i64 =>
              writeLongLE(key.toLong, writer)
            case TypeId.Builtins.u08 =>
              writer.writeByte(key.toByte)
            case TypeId.Builtins.u16 =>
              writeShortLE(key.toShort, writer)
            case TypeId.Builtins.u32 =>
              writeIntLE(key.toInt, writer)
            case TypeId.Builtins.u64 =>
              writeLongLE(key.toLong, writer)
            case TypeId.Builtins.f32 =>
              writeFloatLE(key.toFloat, writer)
            case TypeId.Builtins.f64 =>
              writeDoubleLE(key.toDouble, writer)
            case TypeId.Builtins.f128 =>
              // f128 is encoded as string representation
              val str   = key
              val bytes = str.getBytes(StandardCharsets.UTF_8)
              write7BitEncodedInt(bytes.length, writer)
              writer.write(bytes)
            case TypeId.Builtins.uid =>
              writer.write(toBytes(UUID.fromString(key)))
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Encode timestamp as binary: ticks + offset + kind
              writeLongLE(0L, writer)
              writeLongLE(0L, writer)
              writer.writeByte(0)
            case other =>
              throw new IllegalArgumentException(s"Unsupported map key type: $other")
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          // Assume enum or foreign type that can be represented as string
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          typedef match {
            case enum: Typedef.Enum =>
              encodeEnum(dom, enum, Json.fromString(key), writer)
            case _ =>
              throw new IllegalArgumentException(s"Unsupported map key type: $u")
          }
        case _ =>
          throw new IllegalArgumentException(s"Unsupported map key type: $keyType")
      }
    }

    private def decodeMapKey(dom: Domain, keyType: TypeRef, reader: DataInputStream): String = {
      keyType match {
        case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
          id match {
            case TypeId.Builtins.str =>
              val length = read7BitEncodedInt(reader)
              val bytes  = new Array[Byte](length)
              reader.readFully(bytes)
              new String(bytes, StandardCharsets.UTF_8)
            case TypeId.Builtins.bit =>
              reader.readBoolean().toString
            case TypeId.Builtins.i08 =>
              reader.readByte().toString
            case TypeId.Builtins.i16 =>
              readShortLE(reader).toString
            case TypeId.Builtins.i32 =>
              readIntLE(reader).toString
            case TypeId.Builtins.i64 =>
              readLongLE(reader).toString
            case TypeId.Builtins.u08 =>
              (reader.readByte() & 0xFF).toString
            case TypeId.Builtins.u16 =>
              (readShortLE(reader) & 0xFFFF).toString
            case TypeId.Builtins.u32 =>
              (readIntLE(reader) & 0xFFFFFFFFL).toString
            case TypeId.Builtins.u64 =>
              val value = readLongLE(reader)
              if (value < 0) {
                BigInt(value).+(BigInt(1) << 64).toString
              } else {
                value.toString
              }
            case TypeId.Builtins.f32 =>
              readFloatLE(reader).toString
            case TypeId.Builtins.f64 =>
              readDoubleLE(reader).toString
            case TypeId.Builtins.f128 =>
              // f128 is encoded as string with 7-bit length
              val length = read7BitEncodedInt(reader)
              val bytes  = new Array[Byte](length)
              reader.readFully(bytes)
              new String(bytes, StandardCharsets.UTF_8)
            case TypeId.Builtins.uid =>
              val bytes = new Array[Byte](16)
              reader.readFully(bytes)
              fromBytes(bytes).toString
            case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
              // Read timestamp: ticks + offset + kind = 17 bytes
              val ticks       = readLongLE(reader)
              val offsetTicks = readLongLE(reader)
              val kind        = reader.readByte()
              "1970-01-01T00:00:00Z"
            case other =>
              throw new IllegalArgumentException(s"Unsupported map key type: $other")
          }
        case TypeRef.Scalar(u: TypeId.User) =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          typedef match {
            case enum: Typedef.Enum =>
              decodeEnum(dom, enum, reader).asString.getOrElse(
                throw new IllegalArgumentException(s"Failed to decode enum key as string")
              )
            case _ =>
              throw new IllegalArgumentException(s"Unsupported map key type: $u")
          }
        case _ =>
          throw new IllegalArgumentException(s"Unsupported map key type: $keyType")
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

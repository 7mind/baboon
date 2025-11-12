package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.language.SourceFilePosition

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.util.UUID
import scala.util.Try

trait BaboonRuntimeCodec[F[+_, +_]] {
  def decode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, data: Vector[Byte]): F[BaboonIssue, Json]
  def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]]
}

object BaboonRuntimeCodec {

  class BaboonRuntimeCodecImpl[F[+_, +_]: Error2]() extends BaboonRuntimeCodec[F] {
    private val F = Error2[F]

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

    override def encode(family: BaboonFamily, pkg: Pkg, version: Version, idString: String, json: Json): F[BaboonIssue, Vector[Byte]] = {
      F.fromEither {
        Try {
          val dom     = getDom(family, pkg, version)
          val typedef = getDef(dom, idString)
          val output  = new ByteArrayOutputStream()
          val writer  = new DataOutputStream(output)

          typedef match {
            case u: DomainMember.User =>
              encodeUserType(dom, u.defn, json, writer)
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
    private def encodeUserType(dom: Domain, typedef: Typedef.User, json: Json, writer: DataOutputStream): Unit = {
      typedef match {
        case dto: Typedef.Dto    => encodeDto(dom, dto, json, writer)
        case enum: Typedef.Enum  => encodeEnum(dom, enum, json, writer)
        case adt: Typedef.Adt    => encodeAdt(dom, adt, json, writer)
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

    // DTO encoding/decoding
    private def encodeDto(dom: Domain, dto: Typedef.Dto, json: Json, writer: DataOutputStream): Unit = {
      val obj = json.asObject.getOrElse(
        throw new IllegalArgumentException(s"Expected JSON object for DTO ${dto.id}, got: $json")
      )

      // Write header byte (no index support for now)
      val header: Byte = 0
      writer.writeByte(header)

      // Encode each field
      dto.fields.foreach {
        field =>
          val fieldJson = obj(field.name.name).getOrElse(Json.Null)
          encodeTypeRef(dom, field.tpe, fieldJson, writer)
      }
    }

    private def decodeDto(dom: Domain, dto: Typedef.Dto, reader: DataInputStream): Json = {
      // Read header byte
      val header = reader.readByte()

      // Check if indices are used (bit 0 set)
      val useIndices = (header & 1) != 0

      if (useIndices) {
        throw new UnsupportedOperationException("Index-based decoding is not yet supported")
      }

      // Decode each field
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
    private def encodeAdt(dom: Domain, adt: Typedef.Adt, json: Json, writer: DataOutputStream): Unit = {
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
      encodeUserType(dom, branchDef, branchValue, writer)
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
    private def encodeTypeRef(dom: Domain, tpe: TypeRef, json: Json, writer: DataOutputStream): Unit = {
      tpe match {
        case TypeRef.Scalar(id)            => encodeScalar(dom, id, json, writer)
        case TypeRef.Constructor(id, args) => encodeConstructor(dom, id, args.toList, json, writer)
      }
    }

    private def decodeTypeRef(dom: Domain, tpe: TypeRef, reader: DataInputStream): Json = {
      tpe match {
        case TypeRef.Scalar(id)            => decodeScalar(dom, id, reader)
        case TypeRef.Constructor(id, args) => decodeConstructor(dom, id, args.toList, reader)
      }
    }

    // Scalar encoding/decoding
    private def encodeScalar(dom: Domain, id: TypeId.Scalar, json: Json, writer: DataOutputStream): Unit = {
      id match {
        case s: TypeId.BuiltinScalar => encodeBuiltinScalar(s, json, writer)
        case u: TypeId.User =>
          val typedef = dom.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
          encodeUserType(dom, typedef, json, writer)
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
          writer.writeShort(value)

        case TypeId.Builtins.i32 =>
          val value = json.asNumber.flatMap(_.toInt).getOrElse(throw new IllegalArgumentException(s"Expected i32, got: $json"))
          writer.writeInt(value)

        case TypeId.Builtins.i64 =>
          val value = json.asNumber.flatMap(_.toLong).getOrElse(throw new IllegalArgumentException(s"Expected i64, got: $json"))
          writer.writeLong(value)

        case TypeId.Builtins.u08 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toByte).getOrElse(throw new IllegalArgumentException(s"Expected u08, got: $json"))
          writer.writeByte(value)

        case TypeId.Builtins.u16 =>
          val value = json.asNumber.flatMap(_.toLong).map(_.toShort).getOrElse(throw new IllegalArgumentException(s"Expected u16, got: $json"))
          writer.writeShort(value)

        case TypeId.Builtins.u32 =>
          val value = json.asNumber.flatMap(_.toInt).getOrElse(throw new IllegalArgumentException(s"Expected u32, got: $json"))
          writer.writeInt(value)

        case TypeId.Builtins.u64 =>
          val value = json.asNumber.flatMap(_.toLong).getOrElse(throw new IllegalArgumentException(s"Expected u64, got: $json"))
          writer.writeLong(value)

        case TypeId.Builtins.f32 =>
          val value = json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble.toFloat)).getOrElse(throw new IllegalArgumentException(s"Expected f32, got: $json"))
          writer.writeFloat(value)

        case TypeId.Builtins.f64 =>
          val value = json.asNumber.flatMap(_.toBigDecimal.map(_.toDouble)).getOrElse(throw new IllegalArgumentException(s"Expected f64, got: $json"))
          writer.writeDouble(value)

        case TypeId.Builtins.f128 =>
          val value = json.asNumber.flatMap(_.toBigDecimal).getOrElse(throw new IllegalArgumentException(s"Expected f128, got: $json"))
          // Encode BigDecimal as string length + UTF-8 bytes (similar to str)
          val str   = value.toString
          val bytes = str.getBytes(StandardCharsets.UTF_8)
          writer.writeInt(bytes.length)
          writer.write(bytes)

        case TypeId.Builtins.str =>
          val value = json.asString.getOrElse(throw new IllegalArgumentException(s"Expected string, got: $json"))
          val bytes = value.getBytes(StandardCharsets.UTF_8)
          writer.writeInt(bytes.length)
          writer.write(bytes)

        case TypeId.Builtins.uid =>
          val value = json.asString.map(UUID.fromString).getOrElse(throw new IllegalArgumentException(s"Expected UUID string, got: $json"))
          writer.write(toBytes(value))

        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          val value = json.asString.getOrElse(throw new IllegalArgumentException(s"Expected timestamp string, got: $json"))
          val bytes = value.getBytes(StandardCharsets.UTF_8)
          writer.writeInt(bytes.length)
          writer.write(bytes)

        case other => throw new IllegalArgumentException(s"Unsupported builtin scalar: $other")
      }
    }

    private def decodeBuiltinScalar(id: TypeId.BuiltinScalar, reader: DataInputStream): Json = {
      id match {
        case TypeId.Builtins.bit => Json.fromBoolean(reader.readBoolean())
        case TypeId.Builtins.i08 => Json.fromInt(reader.readByte())
        case TypeId.Builtins.i16 => Json.fromInt(reader.readShort())
        case TypeId.Builtins.i32 => Json.fromInt(reader.readInt())
        case TypeId.Builtins.i64 => Json.fromLong(reader.readLong())
        case TypeId.Builtins.u08 => Json.fromInt(reader.readByte() & 0xFF)
        case TypeId.Builtins.u16 => Json.fromInt(reader.readShort() & 0xFFFF)
        case TypeId.Builtins.u32 => Json.fromLong(reader.readInt() & 0xFFFFFFFFL)
        case TypeId.Builtins.u64 =>
          val value = reader.readLong()
          if (value < 0) {
            // Convert to unsigned BigInt
            Json.fromBigInt(BigInt(value & Long.MaxValue) + BigInt(Long.MaxValue) + 1)
          } else {
            Json.fromLong(value)
          }
        case TypeId.Builtins.f32 => Json.fromFloatOrString(reader.readFloat())
        case TypeId.Builtins.f64 => Json.fromDoubleOrString(reader.readDouble())
        case TypeId.Builtins.f128 =>
          val length = reader.readInt()
          val bytes  = new Array[Byte](length)
          reader.readFully(bytes)
          val str = new String(bytes, StandardCharsets.UTF_8)
          Json.fromBigDecimal(BigDecimal(str))
        case TypeId.Builtins.str =>
          val length = reader.readInt()
          val bytes  = new Array[Byte](length)
          reader.readFully(bytes)
          Json.fromString(new String(bytes, StandardCharsets.UTF_8))
        case TypeId.Builtins.uid =>
          val bytes = new Array[Byte](16)
          reader.readFully(bytes)
          Json.fromString(fromBytes(bytes).toString)
        case TypeId.Builtins.tsu | TypeId.Builtins.tso =>
          val length = reader.readInt()
          val bytes  = new Array[Byte](length)
          reader.readFully(bytes)
          Json.fromString(new String(bytes, StandardCharsets.UTF_8))
        case other => throw new IllegalArgumentException(s"Unsupported builtin scalar: $other")
      }
    }

    // Constructor (collection) encoding/decoding
    private def encodeConstructor(dom: Domain, id: TypeId.BuiltinCollection, args: List[TypeRef], json: Json, writer: DataOutputStream): Unit = {
      id match {
        case TypeId.Builtins.opt =>
          if (json.isNull) {
            writer.writeByte(0)
          } else {
            writer.writeByte(1)
            encodeTypeRef(dom, args.head, json, writer)
          }

        case TypeId.Builtins.lst =>
          val arr = json.asArray.getOrElse(throw new IllegalArgumentException(s"Expected array for list, got: $json"))
          writer.writeInt(arr.size)
          arr.foreach(elem => encodeTypeRef(dom, args.head, elem, writer))

        case TypeId.Builtins.set =>
          val arr = json.asArray.getOrElse(throw new IllegalArgumentException(s"Expected array for set, got: $json"))
          writer.writeInt(arr.size)
          arr.foreach(elem => encodeTypeRef(dom, args.head, elem, writer))

        case TypeId.Builtins.map =>
          val obj = json.asObject.getOrElse(throw new IllegalArgumentException(s"Expected object for map, got: $json"))
          writer.writeInt(obj.size)
          obj.toList.foreach {
            case (key, value) =>
              // Encode key - for now assume keys can be encoded as strings
              encodeMapKey(dom, args.head, key, writer)
              encodeTypeRef(dom, args.last, value, writer)
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
          val count    = reader.readInt()
          val elements = (0 until count).map(_ => decodeTypeRef(dom, args.head, reader))
          Json.arr(elements *)

        case TypeId.Builtins.set =>
          val count    = reader.readInt()
          val elements = (0 until count).map(_ => decodeTypeRef(dom, args.head, reader))
          Json.arr(elements *)

        case TypeId.Builtins.map =>
          val count = reader.readInt()
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
              writer.writeInt(bytes.length)
              writer.write(bytes)
            case TypeId.Builtins.i32 =>
              writer.writeInt(key.toInt)
            case TypeId.Builtins.i64 =>
              writer.writeLong(key.toLong)
            case TypeId.Builtins.uid =>
              writer.write(toBytes(UUID.fromString(key)))
            case _ =>
              // For other types, encode as JSON then encode that
              encodeBuiltinScalar(id, Json.fromString(key), writer)
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
              val length = reader.readInt()
              val bytes  = new Array[Byte](length)
              reader.readFully(bytes)
              new String(bytes, StandardCharsets.UTF_8)
            case TypeId.Builtins.i32 =>
              reader.readInt().toString
            case TypeId.Builtins.i64 =>
              reader.readLong().toString
            case TypeId.Builtins.uid =>
              val bytes = new Array[Byte](16)
              reader.readFully(bytes)
              fromBytes(bytes).toString
            case _ =>
              decodeBuiltinScalar(id, reader).asString.getOrElse(
                throw new IllegalArgumentException(s"Failed to decode map key as string")
              )
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

package io.septimalmind.baboon.explore

import io.circe.Json
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

import java.time.OffsetDateTime
import java.time.format.DateTimeFormatterBuilder
import scala.util.Random

class RandomJsonGenerator(domain: Domain, enquiries: BaboonEnquiries) {
  private val random = new Random()

  private val isoFormatter = new DateTimeFormatterBuilder()
    .appendPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
    .appendOffset("+HH:MM", "Z")
    .toFormatter()

  def generate(member: DomainMember.User): Either[String, Json] = {
    if (enquiries.hasForeignType(member, domain)) {
      Left(s"Cannot generate example for type with foreign dependencies: ${member.id.name.name}")
    } else {
      Right(generateUserType(member.defn))
    }
  }

  private def generateUserType(typedef: Typedef.User): Json = {
    typedef match {
      case dto: Typedef.Dto =>
        generateDto(dto)
      case adt: Typedef.Adt =>
        generateAdt(adt)
      case enum: Typedef.Enum =>
        generateEnum(enum)
      case _: Typedef.Foreign =>
        Json.obj("__foreign__" -> Json.fromString("cannot generate"))
      case _: Typedef.Contract =>
        Json.obj("__contract__" -> Json.fromString("cannot generate"))
      case _: Typedef.Service =>
        Json.obj("__service__" -> Json.fromString("cannot generate"))
    }
  }

  private def generateDto(dto: Typedef.Dto): Json = {
    val fields = dto.fields.map {
      field =>
        field.name.name -> generateTypeRef(field.tpe)
    }
    Json.obj(fields*)
  }

  private def generateAdt(adt: Typedef.Adt): Json = {
    val dataMembers = adt.dataMembers(domain)
    val branchId    = dataMembers(random.nextInt(dataMembers.size))
    val branchDef   = domain.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
    val branchJson  = generateUserType(branchDef)
    Json.obj(branchId.name.name -> branchJson)
  }

  private def generateEnum(`enum`: Typedef.Enum): Json = {
    val members = enum.members.toList
    val member  = members(random.nextInt(members.size))
    Json.fromString(member.name)
  }

  private def generateTypeRef(tpe: TypeRef): Json = {
    tpe match {
      case TypeRef.Scalar(id)            => generateScalar(id)
      case TypeRef.Constructor(id, args) => generateConstructor(id, args.toList)
    }
  }

  private def generateScalar(id: TypeId.Scalar): Json = {
    id match {
      case s: TypeId.BuiltinScalar => generateBuiltinScalar(s)
      case u: TypeId.User =>
        val typedef = domain.defs.meta.nodes(u).asInstanceOf[DomainMember.User].defn
        generateUserType(typedef)
    }
  }

  private def generateBuiltinScalar(id: TypeId.BuiltinScalar): Json = {
    import TypeId.Builtins.*

    id match {
      case `bit`   => Json.fromBoolean(random.nextBoolean())
      case `i08`   => Json.fromInt(random.nextInt(256) - 128)
      case `i16`   => Json.fromInt(random.nextInt(65536) - 32768)
      case `i32`   => Json.fromInt(random.nextInt())
      case `i64`   => Json.fromLong(random.nextLong())
      case `u08`   => Json.fromInt(random.nextInt(256))
      case `u16`   => Json.fromInt(random.nextInt(65536))
      case `u32`   => Json.fromLong(random.nextInt().toLong & 0xFFFFFFFFL)
      case `u64`   => Json.fromLong(math.abs(random.nextLong()))
      case `f32`   => Json.fromFloatOrString(random.nextFloat() * 1000)
      case `f64`   => Json.fromDoubleOrString(random.nextDouble() * 1000)
      case `f128`  => Json.fromBigDecimal(BigDecimal(random.nextDouble() * 1000))
      case `str`   => Json.fromString(randomString(10))
      case `bytes` => Json.fromString(randomHexString(16))
      case `uid`   => Json.fromString(randomUUID())
      case `tsu` | `tso` =>
        val year   = 2020 + random.nextInt(10)
        val month  = 1 + random.nextInt(12)
        val day    = 1 + random.nextInt(28)
        val hour   = random.nextInt(24)
        val minute = random.nextInt(60)
        val second = random.nextInt(60)
        val millis = random.nextInt(1000)
        val dt     = OffsetDateTime.of(year, month, day, hour, minute, second, millis * 1000000, java.time.ZoneOffset.UTC)
        Json.fromString(dt.format(isoFormatter))
      case _ =>
        Json.fromString(s"<unknown scalar: ${id.name.name}>")
    }
  }

  private def generateConstructor(id: TypeId.BuiltinCollection, args: List[TypeRef]): Json = {
    import TypeId.Builtins.*

    id match {
      case `opt` =>
        if (random.nextBoolean()) {
          generateTypeRef(args.head)
        } else {
          Json.Null
        }

      case `lst` =>
        val count = random.nextInt(4) + 1
        Json.arr((0 until count).map(_ => generateTypeRef(args.head))*)

      case `set` =>
        val count = random.nextInt(4) + 1
        Json.arr((0 until count).map(_ => generateTypeRef(args.head))*)

      case `map` =>
        val count    = random.nextInt(3) + 1
        val keyRef   = args.head
        val valueRef = args.last
        val entries = (0 until count).map {
          _ =>
            val key   = generateMapKey(keyRef)
            val value = generateTypeRef(valueRef)
            key -> value
        }
        Json.obj(entries*)

      case _ =>
        Json.fromString(s"<unknown collection: ${id.name.name}>")
    }
  }

  private def generateMapKey(keyType: TypeRef): String = {
    keyType match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        import TypeId.Builtins.*
        id match {
          case `str`                                         => randomString(8)
          case `i08` | `i16` | `i32` | `u08` | `u16` | `u32` => random.nextInt(1000).toString
          case `i64` | `u64`                                 => random.nextLong().toString
          case `uid`                                         => randomUUID()
          case _                                             => randomString(8)
        }
      case TypeRef.Scalar(u: TypeId.User) =>
        domain.defs.meta.nodes.get(u) match {
          case Some(m: DomainMember.User) =>
            m.defn match {
              case enum: Typedef.Enum =>
                val members = enum.members.toList
                members(random.nextInt(members.size)).name
              case _ => randomString(8)
            }
          case _ => randomString(8)
        }
      case _ => randomString(8)
    }
  }

  private def randomUUID(): String = {
    def hex(n: Int): String = (0 until n).map(_ => f"${random.nextInt(16)}%x").mkString
    s"${hex(8)}-${hex(4)}-4${hex(3)}-${hex(1)}${hex(3)}-${hex(12)}"
  }

  private def randomString(length: Int): String = {
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    (0 until length).map(_ => chars.charAt(random.nextInt(chars.length))).mkString
  }

  private def randomHexString(byteCount: Int): String = {
    (0 until byteCount).map(_ => f"${random.nextInt(256)}%02X").mkString
  }
}

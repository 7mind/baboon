package io.septimalmind.baboon.explore

import io.circe.Json
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*

import java.io.{ByteArrayInputStream, FilterInputStream, InputStream}

class PositionTrackingInputStream(in: InputStream) extends FilterInputStream(in) {
  private var _position: Int = 0

  def position: Int = _position

  override def read(): Int = {
    val b = super.read()
    if (b >= 0) _position += 1
    b
  }

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    val n = super.read(b, off, len)
    if (n > 0) _position += n
    n
  }
}

class UebaDecodeRenderer(domain: Domain, enquiries: BaboonEnquiries) {
  import Colors.*
  import baboon.runtime.shared.LEDataInputStream

  def renderOffsets(bytes: Vector[Byte], @annotation.unused json: Json, member: DomainMember.User): String = {
    val sb = new StringBuilder

    sb.append(s"${CYAN}Hex dump (${bytes.length} bytes):${RESET}\n")
    sb.append(formatHexDump(bytes))
    sb.append("\n\n")

    sb.append(s"${CYAN}Structure breakdown:${RESET}\n")
    sb.append(s"$DIM%-8s %-24s %s$RESET\n".format("Offset", "Hex", "Description"))
    sb.append("-" * 70 + "\n")

    val trackingStream = new PositionTrackingInputStream(new ByteArrayInputStream(bytes.toArray))
    val reader = new LEDataInputStream(trackingStream)
    try {
      renderUserType(sb, reader, trackingStream, member.defn, 0)
    } catch {
      case e: Exception =>
        sb.append(s"${RED}Parse error: ${e.getMessage}${RESET}\n")
    }

    sb.toString()
  }

  private def renderUserType(sb: StringBuilder, reader: LEDataInputStream, tracker: PositionTrackingInputStream, typedef: Typedef.User, depth: Int): Int = {
    val indent = "  " * depth

    typedef match {
      case dto: Typedef.Dto =>
        val startPos = tracker.position
        val header = reader.readByte()
        val indexed = (header & 1) != 0
        appendRow(sb, startPos, Seq(header), s"${indent}header: ${if (indexed) "indexed" else "compact"}")

        if (indexed) {
          val varLenCount = dto.fields.count(f => enquiries.uebaLen(domain.defs.meta.nodes, f.tpe).isVariable)
          for (i <- 0 until varLenCount) {
            val idxPos = tracker.position
            val offset = reader.readInt()
            val length = reader.readInt()
            appendRow(sb, idxPos, intToBytes(offset) ++ intToBytes(length),
              s"${indent}  index[$i]: offset=$offset, len=$length")
          }
        }

        dto.fields.foreach { field =>
          renderField(sb, reader, tracker, field, depth + 1)
        }
        tracker.position

      case adt: Typedef.Adt =>
        val discPos = tracker.position
        val discriminator = reader.readByte() & 0xFF
        val dataMembers = adt.dataMembers(domain)
        val branchId = if (discriminator < dataMembers.size) dataMembers(discriminator) else null
        val branchName = if (branchId != null) branchId.name.name else s"<invalid: $discriminator>"
        appendRow(sb, discPos, Seq(discriminator.toByte), s"${indent}discriminator: $discriminator ($branchName)")

        if (branchId != null) {
          val branchDef = domain.defs.meta.nodes(branchId).asInstanceOf[DomainMember.User].defn
          renderUserType(sb, reader, tracker, branchDef, depth + 1)
        }
        tracker.position

      case enum: Typedef.Enum =>
        val enumPos = tracker.position
        val idx = reader.readByte() & 0xFF
        val name = if (idx < enum.members.size) enum.members.toList(idx).name else s"<invalid: $idx>"
        appendRow(sb, enumPos, Seq(idx.toByte), s"${indent}enum: $name (index $idx)")
        tracker.position

      case _ =>
        tracker.position
    }
  }

  private def renderField(sb: StringBuilder, reader: LEDataInputStream, tracker: PositionTrackingInputStream, field: Field, depth: Int): Unit = {
    val indent = "  " * depth
    val fieldPos = tracker.position

    field.tpe match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        renderBuiltinScalar(sb, reader, tracker, id, s"$indent${field.name.name}")

      case TypeRef.Scalar(u: TypeId.User) =>
        sb.append(s"%-8d %-24s %s%s:\n".format(fieldPos, "", indent, field.name.name))
        domain.defs.meta.nodes.get(u) match {
          case Some(m: DomainMember.User) =>
            val _ = renderUserType(sb, reader, tracker, m.defn, depth + 1)
          case _ =>
        }

      case TypeRef.Constructor(id, args) =>
        renderConstructor(sb, reader, tracker, id, args.toList, s"$indent${field.name.name}", depth)
    }
  }

  private def renderBuiltinScalar(sb: StringBuilder, reader: LEDataInputStream, tracker: PositionTrackingInputStream, id: TypeId.BuiltinScalar, label: String): Unit = {
    import TypeId.Builtins.*
    val scalarPos = tracker.position

    id match {
      case `bit` =>
        val v = reader.readBoolean()
        appendRow(sb, scalarPos, Seq(if (v) 1.toByte else 0.toByte), s"$label: $v (bit)")

      case `i08` | `u08` =>
        val v = reader.readByte()
        appendRow(sb, scalarPos, Seq(v), s"$label: $v (${id.name.name})")

      case `i16` | `u16` =>
        val v = reader.readShort()
        appendRow(sb, scalarPos, shortToBytes(v), s"$label: $v (${id.name.name})")

      case `i32` | `u32` =>
        val v = reader.readInt()
        appendRow(sb, scalarPos, intToBytes(v), s"$label: $v (${id.name.name})")

      case `i64` | `u64` =>
        val v = reader.readLong()
        appendRow(sb, scalarPos, longToBytes(v), s"$label: $v (${id.name.name})")

      case `f32` =>
        val v = reader.readFloat()
        appendRow(sb, scalarPos, floatToBytes(v), s"$label: $v (f32)")

      case `f64` =>
        val v = reader.readDouble()
        appendRow(sb, scalarPos, doubleToBytes(v), s"$label: $v (f64)")

      case `f128` =>
        val bytes = (0 until 16).map(_ => reader.readByte())
        appendRow(sb, scalarPos, bytes, s"$label: <decimal> (f128)")

      case `str` =>
        val len = readVarInt(reader)
        val strBytes = (0 until len).map(_ => reader.readByte())
        val str = new String(strBytes.map(_.toByte).toArray, "UTF-8")
        val displayStr = if (str.length > 20) str.take(20) + "..." else str
        appendRow(sb, scalarPos, Seq.empty, s"$label: \"$displayStr\" (str, len=$len)")

      case `bytes` =>
        val len = reader.readInt()
        val bytesData = (0 until len).map(_ => reader.readByte())
        appendRow(sb, scalarPos, intToBytes(len) ++ bytesData.take(8), s"$label: <${len} bytes>")

      case `uid` =>
        val bytes = (0 until 16).map(_ => reader.readByte())
        appendRow(sb, scalarPos, bytes, s"$label: <uuid> (uid)")

      case `tsu` | `tso` =>
        val bytes = (0 until 17).map(_ => reader.readByte())
        appendRow(sb, scalarPos, bytes, s"$label: <timestamp> (${id.name.name})")

      case _ =>
        appendRow(sb, scalarPos, Seq.empty, s"$label: <unknown scalar>")
    }
  }

  private def renderConstructor(
    sb: StringBuilder,
    reader: LEDataInputStream,
    tracker: PositionTrackingInputStream,
    id: TypeId.BuiltinCollection,
    args: List[TypeRef],
    label: String,
    depth: Int
  ): Unit = {
    import TypeId.Builtins.*
    val constrPos = tracker.position
    val indent = "  " * depth

    id match {
      case `opt` =>
        val flag = reader.readByte()
        if (flag == 0) {
          appendRow(sb, constrPos, Seq(0.toByte), s"$label: None (opt)")
        } else {
          appendRow(sb, constrPos, Seq(1.toByte), s"$label: Some (opt)")
          renderTypeRef(sb, reader, tracker, args.head, s"$indent  value", depth + 1)
        }

      case `lst` | `set` =>
        val count = reader.readInt()
        appendRow(sb, constrPos, intToBytes(count), s"$label: ${id.name.name}[$count]")
        for (i <- 0 until math.min(count, 5)) {
          renderTypeRef(sb, reader, tracker, args.head, s"$indent  [$i]", depth + 1)
        }
        if (count > 5) {
          sb.append(s"%-8s %-24s %s  ... and ${count - 5} more\n".format("", "", indent))
          for (_ <- 5 until count) {
            skipTypeRef(reader, args.head)
          }
        }

      case `map` =>
        val count = reader.readInt()
        appendRow(sb, constrPos, intToBytes(count), s"$label: map[$count]")
        for (i <- 0 until math.min(count, 3)) {
          renderTypeRef(sb, reader, tracker, args.head, s"$indent  key[$i]", depth + 1)
          renderTypeRef(sb, reader, tracker, args.last, s"$indent  val[$i]", depth + 1)
        }
        if (count > 3) {
          sb.append(s"%-8s %-24s %s  ... and ${count - 3} more entries\n".format("", "", indent))
        }

      case _ =>
        appendRow(sb, constrPos, Seq.empty, s"$label: <unknown collection>")
    }
  }

  private def renderTypeRef(sb: StringBuilder, reader: LEDataInputStream, tracker: PositionTrackingInputStream, tpe: TypeRef, label: String, depth: Int): Unit = {
    tpe match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        renderBuiltinScalar(sb, reader, tracker, id, label)
      case TypeRef.Scalar(u: TypeId.User) =>
        domain.defs.meta.nodes.get(u) match {
          case Some(m: DomainMember.User) =>
            sb.append(s"%-8d %-24s %s:\n".format(tracker.position, "", label))
            val _ = renderUserType(sb, reader, tracker, m.defn, depth)
          case _ =>
        }
      case TypeRef.Constructor(id, args) =>
        renderConstructor(sb, reader, tracker, id, args.toList, label, depth)
    }
  }

  private def skipTypeRef(reader: LEDataInputStream, tpe: TypeRef): Unit = {
    tpe match {
      case TypeRef.Scalar(id: TypeId.BuiltinScalar) =>
        import TypeId.Builtins.*
        id match {
          case `bit` | `i08` | `u08` => val _ = reader.readByte()
          case `i16` | `u16` => val _ = reader.readShort()
          case `i32` | `u32` => val _ = reader.readInt()
          case `i64` | `u64` => val _ = reader.readLong()
          case `f32` => val _ = reader.readFloat()
          case `f64` => val _ = reader.readDouble()
          case `f128` => (0 until 16).foreach(_ => reader.readByte())
          case `str` =>
            val len = readVarInt(reader)
            (0 until len).foreach(_ => reader.readByte())
          case `bytes` =>
            val len = reader.readInt()
            (0 until len).foreach(_ => reader.readByte())
          case `uid` => (0 until 16).foreach(_ => reader.readByte())
          case `tsu` | `tso` => (0 until 17).foreach(_ => reader.readByte())
          case _ =>
        }
      case TypeRef.Scalar(u: TypeId.User) =>
        domain.defs.meta.nodes.get(u).foreach {
          case m: DomainMember.User => skipUserType(reader, m.defn)
          case _ =>
        }
      case TypeRef.Constructor(id, args) =>
        import TypeId.Builtins.*
        id match {
          case `opt` =>
            if (reader.readByte() != 0) skipTypeRef(reader, args.head)
          case `lst` | `set` =>
            val count = reader.readInt()
            (0 until count).foreach(_ => skipTypeRef(reader, args.head))
          case `map` =>
            val count = reader.readInt()
            (0 until count).foreach { _ =>
              skipTypeRef(reader, args.head)
              skipTypeRef(reader, args.last)
            }
          case _ =>
        }
    }
  }

  private def skipUserType(reader: LEDataInputStream, typedef: Typedef.User): Unit = {
    typedef match {
      case dto: Typedef.Dto =>
        val header = reader.readByte()
        if ((header & 1) != 0) {
          val varLenCount = dto.fields.count(f => enquiries.uebaLen(domain.defs.meta.nodes, f.tpe).isVariable)
          (0 until varLenCount * 2).foreach(_ => reader.readInt())
        }
        dto.fields.foreach(f => skipTypeRef(reader, f.tpe))
      case adt: Typedef.Adt =>
        val disc = reader.readByte() & 0xFF
        val dataMembers = adt.dataMembers(domain)
        if (disc < dataMembers.size) {
          val branchDef = domain.defs.meta.nodes(dataMembers(disc)).asInstanceOf[DomainMember.User].defn
          skipUserType(reader, branchDef)
        }
      case _: Typedef.Enum =>
        val _ = reader.readByte()
      case _ =>
    }
  }

  private def readVarInt(reader: LEDataInputStream): Int = {
    var result = 0
    var shift = 0
    var b = 0
    do {
      b = reader.readByte() & 0xFF
      result |= (b & 0x7F) << shift
      shift += 7
    } while ((b & 0x80) != 0 && shift < 35)
    result
  }

  private def appendRow(sb: StringBuilder, offset: Int, bytes: Seq[Byte], description: String): Unit = {
    val hexStr = bytes.take(8).map(b => f"${b & 0xFF}%02X").mkString(" ")
    val truncated = if (bytes.length > 8) hexStr + " ..." else hexStr
    sb.append("%-8d %-24s %s\n".format(offset, truncated, description))
  }

  private def formatHexDump(bytes: Vector[Byte]): String = {
    bytes.grouped(16).zipWithIndex.map { case (chunk, idx) =>
      val offset = idx * 16
      val hex = chunk.map(b => f"${b & 0xFF}%02X").mkString(" ")
      val ascii = chunk.map(b => if (b >= 32 && b < 127) b.toChar else '.').mkString
      f"$offset%08X  $hex%-48s |$ascii|"
    }.mkString("\n")
  }

  private def shortToBytes(v: Short): Seq[Byte] = Seq((v & 0xFF).toByte, ((v >> 8) & 0xFF).toByte)
  private def intToBytes(v: Int): Seq[Byte] = (0 until 4).map(i => ((v >> (i * 8)) & 0xFF).toByte)
  private def longToBytes(v: Long): Seq[Byte] = (0 until 8).map(i => ((v >> (i * 8)) & 0xFF).toByte)
  private def floatToBytes(v: Float): Seq[Byte] = intToBytes(java.lang.Float.floatToIntBits(v))
  private def doubleToBytes(v: Double): Seq[Byte] = longToBytes(java.lang.Double.doubleToLongBits(v))
}

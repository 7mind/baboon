package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object JvScalarCodecEmitter {

  def jsonDecode(b: TypeId.BuiltinScalar, wire: TextTree[JvValue]): TextTree[JvValue] = b match {
    case TypeId.Builtins.bit   => q"$wire.booleanValue()"
    case TypeId.Builtins.i08   => q"(byte) $wire.intValue()"
    case TypeId.Builtins.i16   => q"(short) $wire.intValue()"
    case TypeId.Builtins.i32   => q"$wire.intValue()"
    case TypeId.Builtins.i64   => q"($wire.isTextual() ? Long.parseLong($wire.textValue()) : $wire.longValue())"
    case TypeId.Builtins.u08   => q"(short) $wire.intValue()"
    case TypeId.Builtins.u16   => q"$wire.intValue()"
    case TypeId.Builtins.u32   => q"$wire.longValue()"
    case TypeId.Builtins.u64   => q"($wire.isTextual() ? Long.parseUnsignedLong($wire.textValue()) : $wire.longValue())"
    case TypeId.Builtins.f32   => q"(float) $wire.doubleValue()"
    case TypeId.Builtins.f64   => q"$wire.doubleValue()"
    case TypeId.Builtins.f128  => q"($wire.isTextual() ? new $jvBigDecimal($wire.textValue()) : $wire.decimalValue())"
    case TypeId.Builtins.str   => q"$wire.textValue()"
    case TypeId.Builtins.bytes => q"$jvByteString.fromHex($wire.textValue())"
    case TypeId.Builtins.uid   => q"$jvUid.fromString($wire.textValue())"
    case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseTsu($wire.textValue())"
    case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseTso($wire.textValue())"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def jsonEncode(b: TypeId.BuiltinScalar, value: TextTree[JvValue]): TextTree[JvValue] = b match {
    case TypeId.Builtins.uid   => q"new $textNode($value.toString())"
    case TypeId.Builtins.tsu   => q"new $textNode($baboonTimeFormats.formatTsu($value))"
    case TypeId.Builtins.tso   => q"new $textNode($baboonTimeFormats.formatTso($value))"
    case TypeId.Builtins.bit   => q"$booleanNode.valueOf($value)"
    case TypeId.Builtins.i08   => q"$shortNode.valueOf((short) $value)"
    case TypeId.Builtins.i16   => q"$shortNode.valueOf($value)"
    case TypeId.Builtins.i32   => q"$intNode.valueOf($value)"
    // 64-bit integers go on the wire as decimal strings in every backend — see the C# emitter
    // and docs/json-codecs.md, "64-bit integers". The decoder stays lenient about numbers.
    case TypeId.Builtins.i64   => q"new $textNode(Long.toString($value))"
    case TypeId.Builtins.u08   => q"$shortNode.valueOf($value)"
    case TypeId.Builtins.u16   => q"$intNode.valueOf($value)"
    case TypeId.Builtins.u32   => q"$longNode.valueOf($value)"
    case TypeId.Builtins.u64   => q"new $textNode(Long.toUnsignedString($value))"
    case TypeId.Builtins.f32   => q"$floatNode.valueOf($value)"
    case TypeId.Builtins.f64   => q"$doubleNode.valueOf($value)"
    case TypeId.Builtins.f128  => q"new $textNode($value.toPlainString())"
    case TypeId.Builtins.str   => q"new $textNode($value)"
    case TypeId.Builtins.bytes => q"new $textNode($value.toHex())"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaDecode(b: TypeId.BuiltinScalar, br: TextTree[JvValue]): TextTree[JvValue] = b match {
    case TypeId.Builtins.bit                       => q"$br.readByte() != 0"
    case TypeId.Builtins.i08                       => q"$br.readByte()"
    case TypeId.Builtins.i16                       => q"$br.readShort()"
    case TypeId.Builtins.i32                       => q"$br.readInt()"
    case TypeId.Builtins.i64                       => q"$br.readLong()"
    case TypeId.Builtins.u08                       => q"(short) ($br.readByte() & 0xFF)"
    case TypeId.Builtins.u16                       => q"($br.readShort() & 0xFFFF)"
    case TypeId.Builtins.u32                       => q"($br.readInt() & 0xFFFFFFFFL)"
    case TypeId.Builtins.u64                       => q"$br.readLong()"
    case TypeId.Builtins.f32                       => q"$br.readFloat()"
    case TypeId.Builtins.f64                       => q"$br.readDouble()"
    case TypeId.Builtins.f128                      => q"$baboonBinTools.readBigDecimal($br)"
    case TypeId.Builtins.str                       => q"$baboonBinTools.readString($br)"
    case TypeId.Builtins.bytes                     => q"$baboonBinTools.readByteString($br)"
    case TypeId.Builtins.uid                       => q"$baboonBinTools.readUid($br)"
    case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.readTimestamp($br)"
    case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaEncode(b: TypeId.BuiltinScalar, bw: TextTree[JvValue], value: TextTree[JvValue]): TextTree[JvValue] = b match {
    case TypeId.Builtins.bit                       => q"$bw.writeByte($value ? 1 : 0);"
    case TypeId.Builtins.i08                       => q"$bw.writeByte($value);"
    case TypeId.Builtins.i16                       => q"$bw.writeShort($value);"
    case TypeId.Builtins.i32                       => q"$bw.writeInt($value);"
    case TypeId.Builtins.i64                       => q"$bw.writeLong($value);"
    case TypeId.Builtins.u08                       => q"$bw.writeByte((byte) ($value & 0xFF));"
    case TypeId.Builtins.u16                       => q"$bw.writeShort((short) ($value & 0xFFFF));"
    case TypeId.Builtins.u32                       => q"$bw.writeInt((int) ($value & 0xFFFFFFFFL));"
    case TypeId.Builtins.u64                       => q"$bw.writeLong($value);"
    case TypeId.Builtins.f32                       => q"$bw.writeFloat($value);"
    case TypeId.Builtins.f64                       => q"$bw.writeDouble($value);"
    case TypeId.Builtins.f128                      => q"$baboonBinTools.writeBigDecimal($bw, $value);"
    case TypeId.Builtins.str                       => q"$baboonBinTools.writeString($bw, $value);"
    case TypeId.Builtins.bytes                     => q"$baboonBinTools.writeByteString($bw, $value);"
    case TypeId.Builtins.uid                       => q"$baboonBinTools.writeUid($bw, $value);"
    case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.writeTimestamp($bw, $value);"
    case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }
}

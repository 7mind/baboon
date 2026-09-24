package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.scl.ScTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object ScScalarCodecEmitter {

  def jsonDecoder(b: TypeId.BuiltinScalar): TextTree[ScValue] = b match {
    case TypeId.Builtins.bit   => q"$circeDecodeBoolean"
    case TypeId.Builtins.i08   => q"$baboonDecodeByte"
    case TypeId.Builtins.i16   => q"$baboonDecodeShort"
    case TypeId.Builtins.i32   => q"$baboonDecodeInt"
    case TypeId.Builtins.i64   => q"$baboonDecodeLong"
    case TypeId.Builtins.u08   => q"$baboonDecodeByte"
    case TypeId.Builtins.u16   => q"$baboonDecodeShort"
    case TypeId.Builtins.u32   => q"$baboonDecodeInt"
    case TypeId.Builtins.u64   => q"$baboonDecodeLong"
    case TypeId.Builtins.f32   => q"$circeDecodeFloat"
    case TypeId.Builtins.f64   => q"$circeDecodeDouble"
    case TypeId.Builtins.f128  => q"$baboonDecodeBigDecimalLenient"
    case TypeId.Builtins.str   => q"$circeDecodeString"
    case TypeId.Builtins.bytes => q"$baboonDecodeByteString"
    case TypeId.Builtins.uid   => q"$circeDecodeUuid"
    case TypeId.Builtins.tsu   => q"$baboonDecodeTsu"
    case TypeId.Builtins.tso   => q"$baboonDecodeTso"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def jsonEncode(b: TypeId.BuiltinScalar, value: TextTree[ScValue]): TextTree[ScValue] = b match {
    case TypeId.Builtins.uid   => q"$circeJson.fromString($value.toString())"
    case TypeId.Builtins.tsu   => q"$circeJson.fromString($baboonTimeFormats.formatTsu($value))"
    case TypeId.Builtins.tso   => q"$circeJson.fromString($baboonTimeFormats.formatTso($value))"
    case TypeId.Builtins.bit   => q"$circeJson.fromBoolean($value)"
    case TypeId.Builtins.i08   => q"$circeJson.fromInt($value.toInt)"
    case TypeId.Builtins.i16   => q"$circeJson.fromInt($value.toInt)"
    case TypeId.Builtins.i32   => q"$circeJson.fromInt($value)"
    // 64-bit integers go on the wire as decimal strings in every backend — see the C# emitter
    // and docs/json-codecs.md, "64-bit integers". `decodeLong` stays lenient about numbers.
    case TypeId.Builtins.i64   => q"$circeJson.fromString($value.toString)"
    case TypeId.Builtins.u08   => q"$circeJson.fromInt(java.lang.Byte.toUnsignedInt($value))"
    case TypeId.Builtins.u16   => q"$circeJson.fromInt(java.lang.Short.toUnsignedInt($value))"
    case TypeId.Builtins.u32   => q"$circeJson.fromLong(java.lang.Integer.toUnsignedLong($value))"
    case TypeId.Builtins.u64   => q"$circeJson.fromString($baboonBinTools.toUnsignedBigInt($value).toString)"
    case TypeId.Builtins.f32   => q"$circeJson.fromFloat($value).get"
    case TypeId.Builtins.f64   => q"$circeJson.fromDouble($value).get"
    case TypeId.Builtins.f128  => q"$circeJson.fromBigDecimal($value)"
    case TypeId.Builtins.str   => q"$circeJson.fromString($value)"
    case TypeId.Builtins.bytes => q"$circeJson.fromString($value.toHexString)"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaDecode(b: TypeId.BuiltinScalar, br: TextTree[ScValue]): TextTree[ScValue] = b match {
    case TypeId.Builtins.bit                       => q"$br.readBoolean()"
    case TypeId.Builtins.i08                       => q"$br.readByte()"
    case TypeId.Builtins.i16                       => q"$br.readShort()"
    case TypeId.Builtins.i32                       => q"$br.readInt()"
    case TypeId.Builtins.i64                       => q"$br.readLong()"
    case TypeId.Builtins.u08                       => q"$br.readByte()"
    case TypeId.Builtins.u16                       => q"$br.readShort()"
    case TypeId.Builtins.u32                       => q"$br.readInt()"
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

  def uebaEncode(b: TypeId.BuiltinScalar, bw: TextTree[ScValue], value: TextTree[ScValue]): TextTree[ScValue] = b match {
    case TypeId.Builtins.bit                       => q"$bw.writeBoolean($value)"
    case TypeId.Builtins.i08                       => q"$bw.writeByte($value.toInt)"
    case TypeId.Builtins.i16                       => q"$bw.writeShort($value.toInt)"
    case TypeId.Builtins.i32                       => q"$bw.writeInt($value)"
    case TypeId.Builtins.i64                       => q"$bw.writeLong($value)"
    case TypeId.Builtins.u08                       => q"$bw.writeByte($value.toInt)"
    case TypeId.Builtins.u16                       => q"$bw.writeShort($value.toInt)"
    case TypeId.Builtins.u32                       => q"$bw.writeInt($value)"
    case TypeId.Builtins.u64                       => q"$bw.writeLong($value)"
    case TypeId.Builtins.f32                       => q"$bw.writeFloat($value)"
    case TypeId.Builtins.f64                       => q"$bw.writeDouble($value)"
    case TypeId.Builtins.f128                      => q"$baboonBinTools.writeBigDecimal($bw, $value)"
    case TypeId.Builtins.str                       => q"$baboonBinTools.writeString($bw, $value)"
    case TypeId.Builtins.bytes                     => q"$baboonBinTools.writeByteString($bw, $value)"
    case TypeId.Builtins.uid                       => q"$baboonBinTools.writeUid($bw, $value)"
    case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonBinTools.writeTimestamp($bw, $value)"
    case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }
}

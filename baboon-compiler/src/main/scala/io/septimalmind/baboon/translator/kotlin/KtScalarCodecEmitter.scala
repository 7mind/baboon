package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

final class KtScalarCodecEmitter(ktTypes: KtTypes) {

  def jsonDecode(b: TypeId.BuiltinScalar, wire: TextTree[KtValue]): TextTree[KtValue] = b match {
    case TypeId.Builtins.bit => q"$wire.jsonPrimitive.boolean"
    case TypeId.Builtins.i08 => q"$wire.jsonPrimitive.int.toByte()"
    case TypeId.Builtins.i16 => q"$wire.jsonPrimitive.int.toShort()"
    case TypeId.Builtins.i32 => q"$wire.jsonPrimitive.int"
    case TypeId.Builtins.i64 => q"$wire.jsonPrimitive.long"
    case TypeId.Builtins.u08 => q"$wire.jsonPrimitive.int.toUByte()"
    case TypeId.Builtins.u16 => q"$wire.jsonPrimitive.int.toUShort()"
    case TypeId.Builtins.u32 => q"$wire.jsonPrimitive.long.toUInt()"
    case TypeId.Builtins.u64 => q"$wire.jsonPrimitive.long.toULong()"
    case TypeId.Builtins.f32 => q"$wire.jsonPrimitive.float"
    case TypeId.Builtins.f64 => q"$wire.jsonPrimitive.double"
    case TypeId.Builtins.f128 =>
      if (ktTypes.multiplatform) q"${ktTypes.ktBigDecimal}.fromString($wire.jsonPrimitive.content)"
      else q"java.math.BigDecimal($wire.jsonPrimitive.content)"
    case TypeId.Builtins.str   => q"$wire.jsonPrimitive.content"
    case TypeId.Builtins.bytes => q"$ktByteString.fromHexString($wire.jsonPrimitive.content)"
    case TypeId.Builtins.uid =>
      if (ktTypes.multiplatform) q"kotlin.uuid.Uuid.parse($wire.jsonPrimitive.content)"
      else q"java.util.UUID.fromString($wire.jsonPrimitive.content)"
    case TypeId.Builtins.tsu => q"$baboonTimeFormats.parseTsu($wire.jsonPrimitive.content)"
    case TypeId.Builtins.tso => q"$baboonTimeFormats.parseTso($wire.jsonPrimitive.content)"
    case other               => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def jsonEncode(b: TypeId.BuiltinScalar, value: TextTree[KtValue]): TextTree[KtValue] = b match {
    case TypeId.Builtins.uid => q"$jsonPrimitive($value.toString())"
    case TypeId.Builtins.tsu => q"$jsonPrimitive($baboonTimeFormats.formatTsu($value))"
    case TypeId.Builtins.tso => q"$jsonPrimitive($baboonTimeFormats.formatTso($value))"
    case TypeId.Builtins.bit => q"$jsonPrimitive($value)"
    case TypeId.Builtins.i08 => q"$jsonPrimitive($value.toInt())"
    case TypeId.Builtins.i16 => q"$jsonPrimitive($value.toInt())"
    case TypeId.Builtins.i32 => q"$jsonPrimitive($value)"
    case TypeId.Builtins.i64 => q"$jsonPrimitive($value)"
    case TypeId.Builtins.u08 => q"$jsonPrimitive($value.toInt())"
    case TypeId.Builtins.u16 => q"$jsonPrimitive($value.toInt())"
    case TypeId.Builtins.u32 => q"$jsonPrimitive($value.toLong())"
    case TypeId.Builtins.u64 => q"$jsonPrimitive($value.toLong())"
    case TypeId.Builtins.f32 => q"$jsonPrimitive($value)"
    case TypeId.Builtins.f64 => q"$jsonPrimitive($value)"
    case TypeId.Builtins.f128 =>
      if (ktTypes.multiplatform) q"$jsonPrimitive($value.toString())"
      else q"$jsonPrimitive($value.toPlainString())"
    case TypeId.Builtins.str   => q"$jsonPrimitive($value)"
    case TypeId.Builtins.bytes => q"$jsonPrimitive($value.toHexString())"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaDecode(b: TypeId.BuiltinScalar, br: TextTree[KtValue]): TextTree[KtValue] = b match {
    case TypeId.Builtins.bit => q"$br.readBoolean()"
    case TypeId.Builtins.i08 => q"$br.readByte()"
    case TypeId.Builtins.i16 => q"$br.readShort()"
    case TypeId.Builtins.i32 => q"$br.readInt()"
    case TypeId.Builtins.i64 => q"$br.readLong()"
    case TypeId.Builtins.u08 => q"$br.readByte().toUByte()"
    case TypeId.Builtins.u16 => q"$br.readShort().toUShort()"
    case TypeId.Builtins.u32 => q"$br.readInt().toUInt()"
    case TypeId.Builtins.u64 => q"$br.readLong().toULong()"
    case TypeId.Builtins.f32 => q"$br.readFloat()"
    case TypeId.Builtins.f64 => q"$br.readDouble()"
    case TypeId.Builtins.f128 =>
      if (ktTypes.multiplatform) q"$baboonBinTools.readBaboonDecimal($br)"
      else q"$baboonBinTools.readBigDecimal($br)"
    case TypeId.Builtins.str   => q"$baboonBinTools.readString($br)"
    case TypeId.Builtins.bytes => q"$baboonBinTools.readByteString($br)"
    case TypeId.Builtins.uid   => q"$baboonBinTools.readUid($br)"
    case TypeId.Builtins.tsu   => q"$baboonBinTools.readTimestamp($br)"
    case TypeId.Builtins.tso =>
      if (ktTypes.multiplatform) q"$baboonBinTools.readTimestampOffset($br)"
      else q"$baboonBinTools.readTimestamp($br)"
    case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaEncode(b: TypeId.BuiltinScalar, bw: TextTree[KtValue], value: TextTree[KtValue]): TextTree[KtValue] = b match {
    case TypeId.Builtins.bit => q"$bw.writeBoolean($value)"
    case TypeId.Builtins.i08 => q"$bw.writeByte($value.toInt())"
    case TypeId.Builtins.i16 => q"$bw.writeShort($value.toInt())"
    case TypeId.Builtins.i32 => q"$bw.writeInt($value)"
    case TypeId.Builtins.i64 => q"$bw.writeLong($value)"
    case TypeId.Builtins.u08 => q"$bw.writeByte($value.toInt())"
    case TypeId.Builtins.u16 => q"$bw.writeShort($value.toInt())"
    case TypeId.Builtins.u32 => q"$bw.writeInt($value.toInt())"
    case TypeId.Builtins.u64 => q"$bw.writeLong($value.toLong())"
    case TypeId.Builtins.f32 => q"$bw.writeFloat($value)"
    case TypeId.Builtins.f64 => q"$bw.writeDouble($value)"
    case TypeId.Builtins.f128 =>
      if (ktTypes.multiplatform) q"$baboonBinTools.writeBaboonDecimal($bw, $value)"
      else q"$baboonBinTools.writeBigDecimal($bw, $value)"
    case TypeId.Builtins.str   => q"$baboonBinTools.writeString($bw, $value)"
    case TypeId.Builtins.bytes => q"$baboonBinTools.writeByteString($bw, $value)"
    case TypeId.Builtins.uid   => q"$baboonBinTools.writeUid($bw, $value)"
    case TypeId.Builtins.tsu   => q"$baboonBinTools.writeTimestamp($bw, $value)"
    case TypeId.Builtins.tso =>
      if (ktTypes.multiplatform) q"$baboonBinTools.writeTimestampOffset($bw, $value)"
      else q"$baboonBinTools.writeTimestamp($bw, $value)"
    case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }
}

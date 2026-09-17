package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

private[dart] object DtScalarCodecOps {
  def decodeJson(id: TypeId.BuiltinScalar, wire: TextTree[DtValue]): TextTree[DtValue] = id match {
    case TypeId.Builtins.bit                                             => q"$wire as bool"
    case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"($wire as num).toInt()"
    case TypeId.Builtins.i64                                             => q"($wire is String ? int.parse($wire as String) : ($wire as num).toInt())"
    case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"($wire as num).toInt()"
    case TypeId.Builtins.u64                       => q"($wire is String ? BigInt.parse($wire as String).toSigned(64).toInt() : ($wire as num).toInt())"
    case TypeId.Builtins.f32 | TypeId.Builtins.f64 => q"($wire as num).toDouble()"
    case TypeId.Builtins.f128                      => q"$baboonDecimal($wire is String ? $wire as String : $wire.toString())"
    case TypeId.Builtins.str                       => q"$wire as String"
    case TypeId.Builtins.uid                       => q"$wire as String"
    case TypeId.Builtins.bytes                     => q"$baboonByteStringTools.fromHexString($wire as String)"
    case TypeId.Builtins.tsu                       => q"$baboonTimeFormats.parseUtc($wire as String)"
    case TypeId.Builtins.tso                       => q"$baboonTimeFormats.parseOffset($wire as String)"
    case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in Dart scalar codec: $other")
  }

  def encodeJson(id: TypeId.BuiltinScalar, value: TextTree[DtValue]): TextTree[DtValue] = id match {
    case TypeId.Builtins.bit                                             => q"$value"
    case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"$value"
    case TypeId.Builtins.i64                                             => q"$value.toString()"
    case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"$value"
    case TypeId.Builtins.u64                                             => q"BigInt.from($value).toUnsigned(64).toString()"
    case TypeId.Builtins.f32 | TypeId.Builtins.f64                       => q"$value"
    case TypeId.Builtins.f128                                            => q"$value.value"
    case TypeId.Builtins.str                                             => q"$value"
    case TypeId.Builtins.uid                                             => q"$value"
    case TypeId.Builtins.bytes                                           => q"$value.toHexString()"
    case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($value)"
    case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($value)"
    case other                                                           => throw new RuntimeException(s"BUG: Unsupported builtin scalar in Dart scalar codec: $other")
  }

  def decodeUeba(id: TypeId.BuiltinScalar, reader: TextTree[DtValue]): TextTree[DtValue] = id match {
    case TypeId.Builtins.bit   => q"$reader.readBool()"
    case TypeId.Builtins.i08   => q"$reader.readI8()"
    case TypeId.Builtins.i16   => q"$reader.readI16()"
    case TypeId.Builtins.i32   => q"$reader.readI32()"
    case TypeId.Builtins.i64   => q"$reader.readI64()"
    case TypeId.Builtins.u08   => q"$reader.readU8()"
    case TypeId.Builtins.u16   => q"$reader.readU16()"
    case TypeId.Builtins.u32   => q"$reader.readU32()"
    case TypeId.Builtins.u64   => q"$reader.readU64()"
    case TypeId.Builtins.f32   => q"$reader.readF32()"
    case TypeId.Builtins.f64   => q"$reader.readF64()"
    case TypeId.Builtins.f128  => q"$reader.readDecimal()"
    case TypeId.Builtins.str   => q"$reader.readString()"
    case TypeId.Builtins.bytes => q"$reader.readBytes()"
    case TypeId.Builtins.uid   => q"$reader.readUuid()"
    case TypeId.Builtins.tsu   => q"$reader.readTsu()"
    case TypeId.Builtins.tso   => q"$reader.readTso()"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in Dart scalar codec: $other")
  }

  def encodeUeba(id: TypeId.BuiltinScalar, writer: TextTree[DtValue], value: TextTree[DtValue]): TextTree[DtValue] = id match {
    case TypeId.Builtins.bit   => q"$writer.writeBool($value);"
    case TypeId.Builtins.i08   => q"$writer.writeI8($value);"
    case TypeId.Builtins.i16   => q"$writer.writeI16($value);"
    case TypeId.Builtins.i32   => q"$writer.writeI32($value);"
    case TypeId.Builtins.i64   => q"$writer.writeI64($value);"
    case TypeId.Builtins.u08   => q"$writer.writeU8($value);"
    case TypeId.Builtins.u16   => q"$writer.writeU16($value);"
    case TypeId.Builtins.u32   => q"$writer.writeU32($value);"
    case TypeId.Builtins.u64   => q"$writer.writeU64($value);"
    case TypeId.Builtins.f32   => q"$writer.writeF32($value);"
    case TypeId.Builtins.f64   => q"$writer.writeF64($value);"
    case TypeId.Builtins.f128  => q"$writer.writeDecimal($value);"
    case TypeId.Builtins.str   => q"$writer.writeString($value);"
    case TypeId.Builtins.bytes => q"$writer.writeBytes($value);"
    case TypeId.Builtins.uid   => q"$writer.writeUuid($value);"
    case TypeId.Builtins.tsu   => q"$writer.writeTsu($value);"
    case TypeId.Builtins.tso   => q"$writer.writeTso($value);"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in Dart scalar codec: $other")
  }

}

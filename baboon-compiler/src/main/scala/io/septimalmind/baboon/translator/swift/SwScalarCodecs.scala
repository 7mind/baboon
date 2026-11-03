package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

private[swift] object SwScalarCodecs {
  // Every scalar decode reads untrusted input and throws on truncated, wrong-typed or malformed
  // data, so the expression always needs `try` at its use site.
  final case class Decoded(expression: TextTree[SwValue]) {
    def withTry: TextTree[SwValue] = q"try $expression"
  }

  // Every JSON scalar decode reads untrusted input and throws on a wrong-typed or malformed value
  // (GitHub 7mind/baboon#106). `what` is a Swift string literal naming the decoded location.
  def jsonDecode(id: TypeId.BuiltinScalar, wire: TextTree[SwValue], what: String): Decoded = {
    val w = q"""$wire, "$what""""
    val expression = id match {
      case TypeId.Builtins.bit   => q"$baboonJsonDecode.bool($w)"
      case TypeId.Builtins.i08   => q"Int8(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.i16   => q"Int16(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.i32   => q"Int32(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.i64   => q"$baboonJsonDecode.int64($w)"
      case TypeId.Builtins.u08   => q"UInt8(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.u16   => q"UInt16(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.u32   => q"UInt32(truncatingIfNeeded: $baboonJsonDecode.number($w).intValue)"
      case TypeId.Builtins.u64   => q"$baboonJsonDecode.uint64($w)"
      case TypeId.Builtins.f32   => q"Float($baboonJsonDecode.number($w).doubleValue)"
      case TypeId.Builtins.f64   => q"$baboonJsonDecode.number($w).doubleValue"
      case TypeId.Builtins.f128  => q"$baboonJsonDecode.decimal($w)"
      case TypeId.Builtins.str   => q"$baboonJsonDecode.string($w)"
      case TypeId.Builtins.uid   => q"$baboonJsonDecode.uuid($w)"
      case TypeId.Builtins.bytes => q"$baboonByteStringTools.fromHexString($baboonJsonDecode.string($w))"
      case TypeId.Builtins.tsu   => q"$baboonTimeFormats.parseUtc($baboonJsonDecode.string($w))"
      case TypeId.Builtins.tso   => q"$baboonTimeFormats.parseOffset($baboonJsonDecode.string($w))"
      case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
    }
    Decoded(expression)
  }

  def jsonEncode(id: TypeId.BuiltinScalar, value: TextTree[SwValue]): TextTree[SwValue] = id match {
    case TypeId.Builtins.bit                                             => q"$value"
    case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 => q"Int($value)"
    case TypeId.Builtins.i64                                             => q"String($value)"
    case TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 => q"Int($value)"
    case TypeId.Builtins.u64                                             => q"String($value)"
    case TypeId.Builtins.f32                                             => q"Double($value)"
    case TypeId.Builtins.f64                                             => q"$value"
    case TypeId.Builtins.f128                                            => q"$value.stringValue"
    case TypeId.Builtins.str                                             => q"$value"
    case TypeId.Builtins.uid                                             => q"$value.uuidString"
    case TypeId.Builtins.bytes                                           => q"$baboonByteStringTools.toHexString($value)"
    case TypeId.Builtins.tsu                                             => q"$baboonTimeFormats.formatUtc($value)"
    case TypeId.Builtins.tso                                             => q"$baboonTimeFormats.formatOffset($value)"
    case other                                                           => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaDecode(id: TypeId.BuiltinScalar, reader: TextTree[SwValue]): Decoded = id match {
    case TypeId.Builtins.bit   => Decoded(q"$reader.readBool()")
    case TypeId.Builtins.i08   => Decoded(q"$reader.readI8()")
    case TypeId.Builtins.i16   => Decoded(q"$reader.readI16()")
    case TypeId.Builtins.i32   => Decoded(q"$reader.readI32()")
    case TypeId.Builtins.i64   => Decoded(q"$reader.readI64()")
    case TypeId.Builtins.u08   => Decoded(q"$reader.readU8()")
    case TypeId.Builtins.u16   => Decoded(q"$reader.readU16()")
    case TypeId.Builtins.u32   => Decoded(q"$reader.readU32()")
    case TypeId.Builtins.u64   => Decoded(q"$reader.readU64()")
    case TypeId.Builtins.f32   => Decoded(q"$reader.readF32()")
    case TypeId.Builtins.f64   => Decoded(q"$reader.readF64()")
    case TypeId.Builtins.f128  => Decoded(q"$reader.readDecimal()")
    case TypeId.Builtins.str   => Decoded(q"$reader.readString()")
    case TypeId.Builtins.bytes => Decoded(q"$reader.readBytes()")
    case TypeId.Builtins.uid   => Decoded(q"$reader.readUuid()")
    case TypeId.Builtins.tsu   => Decoded(q"$reader.readTsu()")
    case TypeId.Builtins.tso   => Decoded(q"$reader.readTso()")
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaEncode(id: TypeId.BuiltinScalar, writer: TextTree[SwValue], value: TextTree[SwValue]): TextTree[SwValue] = id match {
    case TypeId.Builtins.bit   => q"$writer.writeBool($value)"
    case TypeId.Builtins.i08   => q"$writer.writeI8($value)"
    case TypeId.Builtins.i16   => q"$writer.writeI16($value)"
    case TypeId.Builtins.i32   => q"$writer.writeI32($value)"
    case TypeId.Builtins.i64   => q"$writer.writeI64($value)"
    case TypeId.Builtins.u08   => q"$writer.writeU8($value)"
    case TypeId.Builtins.u16   => q"$writer.writeU16($value)"
    case TypeId.Builtins.u32   => q"$writer.writeU32($value)"
    case TypeId.Builtins.u64   => q"$writer.writeU64($value)"
    case TypeId.Builtins.f32   => q"$writer.writeF32($value)"
    case TypeId.Builtins.f64   => q"$writer.writeF64($value)"
    case TypeId.Builtins.f128  => q"$writer.writeDecimal($value)"
    case TypeId.Builtins.str   => q"$writer.writeString($value)"
    case TypeId.Builtins.bytes => q"$writer.writeBytes($value)"
    case TypeId.Builtins.uid   => q"$writer.writeUuid($value)"
    case TypeId.Builtins.tsu   => q"$writer.writeTsu($value)"
    case TypeId.Builtins.tso   => q"$writer.writeTso($value)"
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

}

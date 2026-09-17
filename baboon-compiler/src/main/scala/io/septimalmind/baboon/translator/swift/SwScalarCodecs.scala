package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

private[swift] object SwScalarCodecs {
  sealed trait ThrowingEffect
  case object NonThrowing extends ThrowingEffect
  case object Throwing extends ThrowingEffect
  final case class Decoded(expression: TextTree[SwValue], effect: ThrowingEffect) {
    def mayThrow: Boolean          = effect == Throwing
    def withTry: TextTree[SwValue] = if (mayThrow) q"try $expression" else expression
  }

  def jsonDecode(id: TypeId.BuiltinScalar, wire: TextTree[SwValue]): Decoded = id match {
    case TypeId.Builtins.bit   => Decoded(q"$wire as! Bool", NonThrowing)
    case TypeId.Builtins.i08   => Decoded(q"Int8(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.i16   => Decoded(q"Int16(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.i32   => Decoded(q"Int32(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.i64   => Decoded(q"($wire is String ? Int64($wire as! String)! : Int64(truncatingIfNeeded: ($wire as! NSNumber).int64Value))", NonThrowing)
    case TypeId.Builtins.u08   => Decoded(q"UInt8(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.u16   => Decoded(q"UInt16(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.u32   => Decoded(q"UInt32(truncatingIfNeeded: ($wire as! NSNumber).intValue)", NonThrowing)
    case TypeId.Builtins.u64   => Decoded(q"($wire is String ? UInt64($wire as! String)! : UInt64(truncatingIfNeeded: ($wire as! NSNumber).uint64Value))", NonThrowing)
    case TypeId.Builtins.f32   => Decoded(q"Float(($wire as! NSNumber).doubleValue)", NonThrowing)
    case TypeId.Builtins.f64   => Decoded(q"($wire as! NSNumber).doubleValue", NonThrowing)
    case TypeId.Builtins.f128  => Decoded(q"$baboonDecimal($wire is String ? $wire as! String : String(describing: $wire))", NonThrowing)
    case TypeId.Builtins.str   => Decoded(q"($wire as! String)", NonThrowing)
    case TypeId.Builtins.uid   => Decoded(q"UUID(uuidString: $wire as! String)!", NonThrowing)
    case TypeId.Builtins.bytes => Decoded(q"$baboonByteStringTools.fromHexString($wire as! String)", NonThrowing)
    case TypeId.Builtins.tsu   => Decoded(q"$baboonTimeFormats.parseUtc($wire as! String)", NonThrowing)
    case TypeId.Builtins.tso   => Decoded(q"$baboonTimeFormats.parseOffset($wire as! String)", NonThrowing)
    case other                 => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
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
    case TypeId.Builtins.bit   => Decoded(q"$reader.readBool()", NonThrowing)
    case TypeId.Builtins.i08   => Decoded(q"$reader.readI8()", NonThrowing)
    case TypeId.Builtins.i16   => Decoded(q"$reader.readI16()", NonThrowing)
    case TypeId.Builtins.i32   => Decoded(q"$reader.readI32()", NonThrowing)
    case TypeId.Builtins.i64   => Decoded(q"$reader.readI64()", NonThrowing)
    case TypeId.Builtins.u08   => Decoded(q"$reader.readU8()", NonThrowing)
    case TypeId.Builtins.u16   => Decoded(q"$reader.readU16()", NonThrowing)
    case TypeId.Builtins.u32   => Decoded(q"$reader.readU32()", NonThrowing)
    case TypeId.Builtins.u64   => Decoded(q"$reader.readU64()", NonThrowing)
    case TypeId.Builtins.f32   => Decoded(q"$reader.readF32()", NonThrowing)
    case TypeId.Builtins.f64   => Decoded(q"$reader.readF64()", NonThrowing)
    case TypeId.Builtins.f128  => Decoded(q"$reader.readDecimal()", NonThrowing)
    case TypeId.Builtins.str   => Decoded(q"$reader.readString()", Throwing)
    case TypeId.Builtins.bytes => Decoded(q"$reader.readBytes()", Throwing)
    case TypeId.Builtins.uid   => Decoded(q"$reader.readUuid()", Throwing)
    case TypeId.Builtins.tsu   => Decoded(q"$reader.readTsu()", NonThrowing)
    case TypeId.Builtins.tso   => Decoded(q"$reader.readTso()", NonThrowing)
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

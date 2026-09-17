package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.translator.typescript.TsTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

final class TsScalarCodecOps(target: TsTarget) {
  def decodeJson(id: TypeId.BuiltinScalar, wire: TextTree[TsValue]): TextTree[TsValue] = {
    id match {
      case TypeId.Builtins.bit => q"$wire as boolean"
      case TypeId.Builtins.i08 | TypeId.Builtins.i16 | TypeId.Builtins.i32 | TypeId.Builtins.u08 | TypeId.Builtins.u16 | TypeId.Builtins.u32 | TypeId.Builtins.f32 |
          TypeId.Builtins.f64 =>
        q"$wire as number"
      case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"BigInt($wire as string)"
      case TypeId.Builtins.f128                      => q"$tsBaboonDecimal.fromString($wire as string)"
      case TypeId.Builtins.str | TypeId.Builtins.uid => q"$wire as string"
      case TypeId.Builtins.bytes                     => q"$tsBinTools.hexDecode($wire as string)"
      case TypeId.Builtins.tsu =>
        target.language.timestampsUtcMode match {
          case "string" => q"$wire as string"
          case "date"   => q"new Date($wire as string)"
          case _        => q"$tsBaboonDateTimeUtc.fromISO($wire as string)"
        }
      case TypeId.Builtins.tso =>
        target.language.timestampsOffsetMode match {
          case "string" => q"$wire as string"
          case "date"   => q"new Date($wire as string)"
          case _        => q"$tsBaboonDateTimeOffset.fromISO($wire as string)"
        }
      case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
    }
  }

  def encodeJson(id: TypeId.BuiltinScalar, value: TextTree[TsValue]): TextTree[TsValue] = {
    id match {
      case TypeId.Builtins.i64 | TypeId.Builtins.u64 => q"$value.toString()"
      case TypeId.Builtins.f128                      => q"$value.toString()"
      case TypeId.Builtins.bytes                     => q"$tsBinTools.hexEncode($value)"
      case TypeId.Builtins.tsu =>
        target.language.timestampsUtcMode match {
          case "string" => value
          case _        => q"$value.toISOString()"
        }
      case TypeId.Builtins.tso =>
        target.language.timestampsOffsetMode match {
          case "string" => value
          case _        => q"$value.toISOString()"
        }
      case _ => value
    }
  }

  def decodeUeba(id: TypeId.BuiltinScalar, reader: TextTree[TsValue]): TextTree[TsValue] = {
    id match {
      case TypeId.Builtins.bit   => q"$tsBinTools.readBool($reader)"
      case TypeId.Builtins.i08   => q"$tsBinTools.readI8($reader)"
      case TypeId.Builtins.i16   => q"$tsBinTools.readI16($reader)"
      case TypeId.Builtins.i32   => q"$tsBinTools.readI32($reader)"
      case TypeId.Builtins.i64   => q"$tsBinTools.readI64($reader)"
      case TypeId.Builtins.u08   => q"$tsBinTools.readU8($reader)"
      case TypeId.Builtins.u16   => q"$tsBinTools.readU16($reader)"
      case TypeId.Builtins.u32   => q"$tsBinTools.readU32($reader)"
      case TypeId.Builtins.u64   => q"$tsBinTools.readU64($reader)"
      case TypeId.Builtins.f32   => q"$tsBinTools.readF32($reader)"
      case TypeId.Builtins.f64   => q"$tsBinTools.readF64($reader)"
      case TypeId.Builtins.f128  => q"$tsBinTools.readDecimal($reader)"
      case TypeId.Builtins.str   => q"$tsBinTools.readString($reader)"
      case TypeId.Builtins.bytes => q"$tsBinTools.readBytes($reader)"
      case TypeId.Builtins.uid   => q"$tsBinTools.readUuid($reader)"
      case TypeId.Builtins.tsu =>
        target.language.timestampsUtcMode match {
          case "string" => q"$tsBinTools.readTimestampUtc($reader).toISOString()"
          case "date"   => q"$tsBinTools.readTimestampUtc($reader).date"
          case _        => q"$tsBinTools.readTimestampUtc($reader)"
        }
      case TypeId.Builtins.tso =>
        target.language.timestampsOffsetMode match {
          case "string" => q"$tsBinTools.readTimestampOffset($reader).toISOString()"
          case "date"   => q"$tsBinTools.readTimestampOffset($reader).date"
          case _        => q"$tsBinTools.readTimestampOffset($reader)"
        }
      case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
    }
  }

  def encodeUeba(id: TypeId.BuiltinScalar, writer: TextTree[TsValue], value: TextTree[TsValue]): TextTree[TsValue] = {
    id match {
      case TypeId.Builtins.bit   => q"$tsBinTools.writeBool($writer, $value);"
      case TypeId.Builtins.i08   => q"$tsBinTools.writeI8($writer, $value);"
      case TypeId.Builtins.i16   => q"$tsBinTools.writeI16($writer, $value);"
      case TypeId.Builtins.i32   => q"$tsBinTools.writeI32($writer, $value);"
      case TypeId.Builtins.i64   => q"$tsBinTools.writeI64($writer, $value);"
      case TypeId.Builtins.u08   => q"$tsBinTools.writeU8($writer, $value);"
      case TypeId.Builtins.u16   => q"$tsBinTools.writeU16($writer, $value);"
      case TypeId.Builtins.u32   => q"$tsBinTools.writeU32($writer, $value);"
      case TypeId.Builtins.u64   => q"$tsBinTools.writeU64($writer, $value);"
      case TypeId.Builtins.f32   => q"$tsBinTools.writeF32($writer, $value);"
      case TypeId.Builtins.f64   => q"$tsBinTools.writeF64($writer, $value);"
      case TypeId.Builtins.f128  => q"$tsBinTools.writeDecimal($writer, $value);"
      case TypeId.Builtins.str   => q"$tsBinTools.writeString($writer, $value);"
      case TypeId.Builtins.bytes => q"$tsBinTools.writeBytes($writer, $value);"
      case TypeId.Builtins.uid   => q"$tsBinTools.writeUuid($writer, $value);"
      case TypeId.Builtins.tsu =>
        target.language.timestampsUtcMode match {
          case "string" => q"$tsBinTools.writeTimestampUtc($writer, $tsBaboonDateTimeUtc.fromISO($value));"
          case "date"   => q"$tsBinTools.writeTimestampUtc($writer, $tsBaboonDateTimeUtc.fromDate($value));"
          case _        => q"$tsBinTools.writeTimestampUtc($writer, $value);"
        }
      case TypeId.Builtins.tso =>
        target.language.timestampsOffsetMode match {
          case "string" => q"$tsBinTools.writeTimestampOffset($writer, $tsBaboonDateTimeOffset.fromISO($value));"
          case "date"   => q"$tsBinTools.writeTimestampOffset($writer, $tsBaboonDateTimeOffset.fromISO($value.toISOString()));"
          case _        => q"$tsBinTools.writeTimestampOffset($writer, $value);"
        }
      case other => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
    }
  }
}

package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.csharp.CSTypes.*
import io.septimalmind.baboon.typer.model.TypeId
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object CSScalarCodecEmitter {
  sealed trait JsonBooleanFormat
  object JsonBooleanFormat {
    case object BooleanValue extends JsonBooleanFormat
    case object LowercaseString extends JsonBooleanFormat
  }

  def jsonDecode(b: TypeId.BuiltinScalar, wire: TextTree[CSValue]): TextTree[CSValue] = {
    val fref = q"$wire!"
    b match {
      case TypeId.Builtins.bit                       => q"$fref.Value<$csBoolean>()!"
      case TypeId.Builtins.i08                       => q"$fref.Value<$csSByte>()!"
      case TypeId.Builtins.i16                       => q"$fref.Value<$csInt16>()!"
      case TypeId.Builtins.i32                       => q"$fref.Value<$csInt32>()!"
      case TypeId.Builtins.i64                       => q"$fref.Value<$csInt64>()!"
      case TypeId.Builtins.u08                       => q"$fref.Value<$csByte>()!"
      case TypeId.Builtins.u16                       => q"$fref.Value<$csUInt16>()!"
      case TypeId.Builtins.u32                       => q"$fref.Value<$csUInt32>()!"
      case TypeId.Builtins.u64                       => q"$fref.Value<$csUInt64>()!"
      case TypeId.Builtins.f32                       => q"$fref.Value<$csSingle>()!"
      case TypeId.Builtins.f64                       => q"$fref.Value<$csDouble>()!"
      case TypeId.Builtins.f128                      => q"$BaboonTools.ReadDecimalLenient($fref)"
      case TypeId.Builtins.str                       => q"$fref.Value<$csString>()!"
      case TypeId.Builtins.bytes                     => q"$csByteString.Parse($fref.Value<$csString>()!)"
      case TypeId.Builtins.uid                       => q"$csGuid.Parse($fref.Value<$csString>()!)"
      case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.FromString($fref.Value<$csString>()!)"
      case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
    }
  }

  def jsonEncode(b: TypeId.BuiltinScalar, value: TextTree[CSValue], booleanFormat: JsonBooleanFormat): TextTree[CSValue] = b match {
    // 64-bit integers go on the wire as decimal strings in every backend: a JSON number beyond
    // 2^53 is already rounded by the time a JavaScript reader sees it (docs/json-codecs.md,
    // "64-bit integers"). Readers stay lenient about the older numeric form.
    case TypeId.Builtins.i64 | TypeId.Builtins.u64                                 => q"new $nsJValue($value.ToString($csInvariantCulture.InvariantCulture))"
    case TypeId.Builtins.bytes                                                     => q"new $nsJValue($value.Encode())"
    case TypeId.Builtins.uid                                                       => q"new $nsJValue($value.ToString())"
    case TypeId.Builtins.tsu                                                       => q"new $nsJValue($baboonTimeFormats.TsuToString($value))"
    case TypeId.Builtins.tso                                                       => q"new $nsJValue($baboonTimeFormats.TsoToString($value))"
    case TypeId.Builtins.bit if booleanFormat == JsonBooleanFormat.LowercaseString => q"new $nsJValue($value.ToString().ToLowerInvariant())"
    case _: TypeId.BuiltinScalar                                                   => q"new $nsJValue($value)"
  }

  def uebaDecode(b: TypeId.BuiltinScalar, br: TextTree[CSValue]): TextTree[CSValue] = b match {
    case TypeId.Builtins.bit                       => q"$br.ReadBoolean()"
    case TypeId.Builtins.i08                       => q"$br.ReadSByte()"
    case TypeId.Builtins.i16                       => q"$br.ReadInt16()"
    case TypeId.Builtins.i32                       => q"$br.ReadInt32()"
    case TypeId.Builtins.i64                       => q"$br.ReadInt64()"
    case TypeId.Builtins.u08                       => q"$br.ReadByte()"
    case TypeId.Builtins.u16                       => q"$br.ReadUInt16()"
    case TypeId.Builtins.u32                       => q"$br.ReadUInt32()"
    case TypeId.Builtins.u64                       => q"$br.ReadUInt64()"
    case TypeId.Builtins.f32                       => q"$br.ReadSingle()"
    case TypeId.Builtins.f64                       => q"$br.ReadDouble()"
    case TypeId.Builtins.f128                      => q"$br.ReadDecimal()"
    case TypeId.Builtins.str                       => q"$br.ReadString()"
    case TypeId.Builtins.bytes                     => q"$csByteString.ReadBytes($br)"
    case TypeId.Builtins.uid                       => q"new $csGuid($br.ReadBytes(16))"
    case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.DecodeFromBin($br)"
    case other                                     => throw new RuntimeException(s"BUG: Unsupported builtin scalar in service wiring: $other")
  }

  def uebaEncode(b: TypeId.BuiltinScalar, bw: TextTree[CSValue], value: TextTree[CSValue]): TextTree[CSValue] = b match {
    case TypeId.Builtins.bytes                     => q"$csByteString.WriteBytes($value, $bw)"
    case TypeId.Builtins.uid                       => q"$bw.Write($value.ToByteArray())"
    case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$baboonTimeFormats.EncodeToBin($value, $bw)"
    case _: TypeId.BuiltinScalar                   => q"$bw.Write($value)"
  }
}

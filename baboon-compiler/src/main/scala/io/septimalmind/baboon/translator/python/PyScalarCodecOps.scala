package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.typer.model.TypeId
import io.septimalmind.baboon.translator.python.PyTypes.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

private[python] object PyScalarCodecOps {
  def jsonFieldValue(value: TextTree[PyValue]): TextTree[PyValue] = q"$pyToJsonablePython($value)"

  def jsonServiceDecode(id: TypeId.BuiltinScalar, data: TextTree[PyValue]): TextTree[PyValue] = {
    val value = q"$pyJsonLoads($data)"
    id match {
      case TypeId.Builtins.bytes                     => q"$pyBytes.fromhex($value)"
      case TypeId.Builtins.uid                       => q"$pyUuid($value)"
      case TypeId.Builtins.f128                      => q"$pyDecimal(str($value))"
      case TypeId.Builtins.tsu | TypeId.Builtins.tso => q"$pyDateTime.fromisoformat($value)"
      case _                                         => value
    }
  }

  def jsonServiceEncode(id: TypeId.BuiltinScalar, value: TextTree[PyValue]): TextTree[PyValue] = {
    val encoded = id match {
      // Direct DTO bytes use hex config; walked Pydantic subtrees retain their own policy.
      case TypeId.Builtins.bytes                                                                  => q"$value.hex()"
      case TypeId.Builtins.uid | TypeId.Builtins.f128 | TypeId.Builtins.tsu | TypeId.Builtins.tso => jsonFieldValue(value)
      case _                                                                                      => value
    }
    q"$pyJsonDumps($encoded)"
  }

  private final case class BinaryMethods(read: String, write: String)

  private def binaryMethods(id: TypeId.BuiltinScalar): BinaryMethods = id match {
    case TypeId.Builtins.bit   => BinaryMethods("read_bool", "write_bool")
    case TypeId.Builtins.i08   => BinaryMethods("read_byte", "write_byte")
    case TypeId.Builtins.i16   => BinaryMethods("read_i16", "write_i16")
    case TypeId.Builtins.i32   => BinaryMethods("read_i32", "write_i32")
    case TypeId.Builtins.i64   => BinaryMethods("read_i64", "write_i64")
    case TypeId.Builtins.u08   => BinaryMethods("read_ubyte", "write_ubyte")
    case TypeId.Builtins.u16   => BinaryMethods("read_u16", "write_u16")
    case TypeId.Builtins.u32   => BinaryMethods("read_u32", "write_u32")
    case TypeId.Builtins.u64   => BinaryMethods("read_u64", "write_u64")
    case TypeId.Builtins.f32   => BinaryMethods("read_f32", "write_f32")
    case TypeId.Builtins.f64   => BinaryMethods("read_f64", "write_f64")
    case TypeId.Builtins.f128  => BinaryMethods("read_f128", "write_f128")
    case TypeId.Builtins.str   => BinaryMethods("read_string", "write_str")
    case TypeId.Builtins.uid   => BinaryMethods("read_uuid", "write_uuid")
    case TypeId.Builtins.tsu   => BinaryMethods("read_datetime", "write_datetime")
    case TypeId.Builtins.tso   => BinaryMethods("read_datetime", "write_datetime")
    case TypeId.Builtins.bytes => BinaryMethods("read_bytes", "write_bytes")
    case other                 => throw new IllegalStateException(s"Unsupported Python scalar codec: $other")
  }

  def decode(id: TypeId.BuiltinScalar, reader: TextTree[PyValue]): TextTree[PyValue] = {
    q"$reader.${binaryMethods(id).read}()"
  }

  def encode(id: TypeId.BuiltinScalar, writer: TextTree[PyValue], value: TextTree[PyValue]): TextTree[PyValue] = {
    q"$writer.${binaryMethods(id).write}($value)"
  }
}

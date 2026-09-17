package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.kotlin.{KtScalarCodecEmitter, KtTypes, KtValue}
import io.septimalmind.baboon.translator.csharp.{CSScalarCodecEmitter, CSValue}
import io.septimalmind.baboon.translator.java.{JvScalarCodecEmitter, JvValue}
import io.septimalmind.baboon.translator.scl.{ScScalarCodecEmitter, ScValue}
import io.septimalmind.baboon.typer.model.TypeId
import io.septimalmind.baboon.typer.model.TypeId.Builtins.*
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScalarCodecEmitterTest extends AnyFlatSpec with Matchers {
  private def kotlin(tree: TextTree[KtValue]): String = tree.mapRender {
    case t: KtValue.KtType     => t.name
    case t: KtValue.KtTypeName => t.name
  }

  private def scala(tree: TextTree[ScValue]): String = tree.mapRender { case t: ScValue.ScType => t.name }

  "C# scalar emission" should "keep the existing RPC boolean representation explicit" in {
    def render(tree: TextTree[CSValue]): String = tree.mapRender {
      case t: CSValue.CSType     => t.name
      case t: CSValue.CSTypeName => t.name
    }
    render(CSScalarCodecEmitter.jsonEncode(bit, q"value", CSScalarCodecEmitter.JsonBooleanFormat.BooleanValue)) shouldBe "new JValue(value)"
    render(
      CSScalarCodecEmitter.jsonEncode(bit, q"value", CSScalarCodecEmitter.JsonBooleanFormat.LowercaseString)
    ) shouldBe "new JValue(value.ToString().ToLowerInvariant())"
    render(CSScalarCodecEmitter.uebaDecode(uid, q"wire")) shouldBe "new Guid(wire.ReadBytes(16))"
    render(CSScalarCodecEmitter.uebaEncode(uid, q"writer", q"value")) shouldBe "writer.Write(value.ToByteArray())"
  }

  "Java scalar emission" should "retain unsigned widths and lenient textual number decoding" in {
    def render(tree: TextTree[JvValue]): String = tree.mapRender {
      case t: JvValue.JvType     => t.name
      case t: JvValue.JvTypeName => t.name
    }
    render(JvScalarCodecEmitter.jsonDecode(i64, q"wire")) shouldBe "(wire.isTextual() ? Long.parseLong(wire.textValue()) : wire.longValue())"
    render(JvScalarCodecEmitter.jsonDecode(u64, q"wire")) shouldBe "(wire.isTextual() ? Long.parseUnsignedLong(wire.textValue()) : wire.longValue())"
    render(JvScalarCodecEmitter.jsonEncode(u64, q"value")) shouldBe "new TextNode(Long.toUnsignedString(value))"
    render(JvScalarCodecEmitter.uebaDecode(u32, q"wire")) shouldBe "(wire.readInt() & 0xFFFFFFFFL)"
    render(JvScalarCodecEmitter.uebaEncode(u32, q"writer", q"value")) shouldBe "writer.writeInt((int) (value & 0xFFFFFFFFL));"
  }

  private val kotlinJsonCases = List(
    (bit, "value", "boolean"),
    (i08, "value.toInt()", "int.toByte()"),
    (i16, "value.toInt()", "int.toShort()"),
    (i32, "value", "int"),
    (i64, "value", "long"),
    (u08, "value.toInt()", "int.toUByte()"),
    (u16, "value.toInt()", "int.toUShort()"),
    (u32, "value.toLong()", "long.toUInt()"),
    (u64, "value.toLong()", "long.toULong()"),
    (f32, "value", "float"),
    (f64, "value", "double"),
    (str, "value", "content"),
  )

  "Kotlin scalar emission" should "retain JSON primitive narrowing and unsigned expressions on both platforms" in {
    List(false, true).foreach {
      multiplatform =>
        val emitter = new KtScalarCodecEmitter(new KtTypes(multiplatform))
        kotlinJsonCases.foreach {
          case (id, value, accessor) =>
            kotlin(emitter.jsonEncode(id, q"value")) shouldBe s"JsonPrimitive($value)"
            kotlin(emitter.jsonDecode(id, q"wire")) shouldBe s"wire.jsonPrimitive.$accessor"
        }
        kotlin(emitter.jsonEncode(bytes, q"value")) shouldBe "JsonPrimitive(value.toHexString())"
        kotlin(emitter.jsonDecode(bytes, q"wire")) shouldBe "ByteString.fromHexString(wire.jsonPrimitive.content)"
        kotlin(emitter.jsonEncode(uid, q"value")) shouldBe "JsonPrimitive(value.toString())"
        List(tsu -> "Tsu", tso -> "Tso").foreach {
          case (id, suffix) =>
            kotlin(emitter.jsonEncode(id, q"value")) shouldBe s"JsonPrimitive(BaboonTimeFormats.format$suffix(value))"
            kotlin(emitter.jsonDecode(id, q"wire")) shouldBe s"BaboonTimeFormats.parse$suffix(wire.jsonPrimitive.content)"
        }
        val decimalEncode = if (multiplatform) "value.toString()" else "value.toPlainString()"
        val decimalDecode = if (multiplatform) "BaboonDecimal.fromString" else "java.math.BigDecimal"
        val uuidDecode    = if (multiplatform) "kotlin.uuid.Uuid.parse" else "java.util.UUID.fromString"
        kotlin(emitter.jsonEncode(f128, q"value")) shouldBe s"JsonPrimitive($decimalEncode)"
        kotlin(emitter.jsonDecode(f128, q"wire")) shouldBe s"$decimalDecode(wire.jsonPrimitive.content)"
        kotlin(emitter.jsonDecode(uid, q"wire")) shouldBe s"$uuidDecode(wire.jsonPrimitive.content)"
    }
  }

  private val binaryCases = List(
    (bit, "Boolean", "value", ""),
    (i08, "Byte", "value.toInt", ""),
    (i16, "Short", "value.toInt", ""),
    (i32, "Int", "value", ""),
    (i64, "Long", "value", ""),
    (u08, "Byte", "value.toInt", ".toUByte()"),
    (u16, "Short", "value.toInt", ".toUShort()"),
    (u32, "Int", "value", ".toUInt()"),
    (u64, "Long", "value", ".toULong()"),
    (f32, "Float", "value", ""),
    (f64, "Double", "value", ""),
  )

  "Scalar binary emission" should "retain primitive widths, casts and platform-specific runtime calls" in {
    binaryCases.foreach {
      case (id, method, scValue, unsignedRead) =>
        scala(ScScalarCodecEmitter.uebaDecode(id, q"wire")) shouldBe s"wire.read$method()"
        scala(ScScalarCodecEmitter.uebaEncode(id, q"writer", q"value")) shouldBe s"writer.write$method($scValue)"
        List(false, true).foreach {
          multiplatform =>
            val emitter = new KtScalarCodecEmitter(new KtTypes(multiplatform))
            val ktValue = id match {
              case TypeId.Builtins.u32 => "value.toInt()"
              case TypeId.Builtins.u64 => "value.toLong()"
              case _                   => if (scValue.endsWith(".toInt")) scValue + "()" else scValue
            }
            kotlin(emitter.uebaDecode(id, q"wire")) shouldBe s"wire.read$method()$unsignedRead"
            kotlin(emitter.uebaEncode(id, q"writer", q"value")) shouldBe s"writer.write$method($ktValue)"
        }
    }
    val runtimeCases = List(str -> "String", bytes -> "ByteString", uid -> "Uid", tsu -> "Timestamp", tso -> "Timestamp", f128 -> "BigDecimal")
    runtimeCases.foreach {
      case (id, scSuffix) =>
        scala(ScScalarCodecEmitter.uebaDecode(id, q"wire")) shouldBe s"BaboonBinTools.read$scSuffix(wire)"
        scala(ScScalarCodecEmitter.uebaEncode(id, q"writer", q"value")) shouldBe s"BaboonBinTools.write$scSuffix(writer, value)"
        List(false, true).foreach {
          multiplatform =>
            val emitter = new KtScalarCodecEmitter(new KtTypes(multiplatform))
            val suffix  = if (multiplatform && id == tso) "TimestampOffset" else if (multiplatform && id == f128) "BaboonDecimal" else scSuffix
            kotlin(emitter.uebaDecode(id, q"wire")) shouldBe s"BaboonBinTools.read$suffix(wire)"
            kotlin(emitter.uebaEncode(id, q"writer", q"value")) shouldBe s"BaboonBinTools.write$suffix(writer, value)"
        }
    }
  }

  "Scala scalar emission" should "retain unsigned JSON encoding and lenient value decoders" in {
    val cases = List(
      (bit, "fromBoolean(value)", "decodeBoolean"),
      (i08, "fromInt(value.toInt)", "decodeByte"),
      (i16, "fromInt(value.toInt)", "decodeShort"),
      (i32, "fromInt(value)", "decodeInt"),
      (i64, "fromLong(value)", "decodeLong"),
      (u08, "fromInt(java.lang.Byte.toUnsignedInt(value))", "decodeByte"),
      (u16, "fromInt(java.lang.Short.toUnsignedInt(value))", "decodeShort"),
      (u32, "fromLong(java.lang.Integer.toUnsignedLong(value))", "decodeInt"),
      (u64, "fromBigInt(BaboonBinTools.toUnsignedBigInt(value))", "decodeLong"),
      (f32, "fromFloat(value).get", "decodeFloat"),
      (f64, "fromDouble(value).get", "decodeDouble"),
      (f128, "fromBigDecimal(value)", "decodeBigDecimalLenient"),
      (str, "fromString(value)", "decodeString"),
      (bytes, "fromString(value.toHexString)", "decodeByteString"),
      (uid, "fromString(value.toString())", "decodeUUID"),
      (tsu, "fromString(BaboonTimeFormats.formatTsu(value))", "decodeTsu"),
      (tso, "fromString(BaboonTimeFormats.formatTso(value))", "decodeTso"),
    )
    cases.foreach {
      case (id, encoded, decoder) =>
        scala(ScScalarCodecEmitter.jsonEncode(id, q"value")) shouldBe s"Json.$encoded"
        scala(ScScalarCodecEmitter.jsonDecoder(id)) shouldBe decoder
    }
  }
}

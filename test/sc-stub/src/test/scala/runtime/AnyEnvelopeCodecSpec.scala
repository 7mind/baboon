package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class AnyEnvelopeCodecSpec extends AnyFunSuite {
  private val meta    = AnyMeta(0.toByte, None, None, None)
  private val payload = Array[Byte](1, 2)

  private def encode(value: AnyOpaque, kind: Byte): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val writer = new LEDataOutputStream(buffer)
    BaboonAnyBinCodec.encode(BaboonCodecContext.Compact, writer, kind, None, None, None, value)
    writer.close()
    buffer.toByteArray
  }

  private def decode(bytes: Array[Byte], kind: Byte): AnyOpaqueUeba =
    BaboonAnyBinCodec.decode(new LEDataInputStream(new ByteArrayInputStream(bytes)), kind)

  test("binary envelope retains exact bytes") {
    val expected = Array[Byte](7, 0, 0, 0, 1, 0, 0, 0, 0, 1, 2)
    assert(java.util.Arrays.equals(expected, encode(AnyOpaqueUeba(meta, payload), 0.toByte)))
    assert(decode(expected, 0.toByte) == AnyOpaqueUeba(meta, payload))
  }

  test("binary envelope skips future metadata extensions") {
    val extended = Array[Byte](9, 0, 0, 0, 3, 0, 0, 0, 0, 42, 43, 1, 2)
    assert(decode(extended, 0.toByte) == AnyOpaqueUeba(meta, payload))
  }

  test("JSON envelope retains content and checks kind") {
    val content  = Json.obj("x" -> Json.fromInt(1))
    val value    = AnyOpaqueJson(meta, content)
    val expected = Json.obj("$ak" -> Json.fromInt(0), "$c" -> content)
    assert(BaboonAnyJsonCodec.encode(BaboonCodecContext.Compact, 0.toByte, None, None, None, value) == expected)
    assert(BaboonAnyJsonCodec.decode(0.toByte, expected) == Right(value))
    assert(BaboonAnyJsonCodec.decode(1.toByte, expected).isLeft)
    assert(BaboonAnyJsonCodec.decode(0.toByte, Json.obj("$ak" -> Json.fromInt(0))).isLeft)
  }

  test("encoding retains kind and facade requirements") {
    val binary = AnyOpaqueUeba(meta, payload)
    intercept[BaboonCodecException.EncoderFailure](encode(binary, 1.toByte))
    intercept[BaboonCodecException.EncoderFailure] {
      BaboonAnyJsonCodec.encode(BaboonCodecContext.Compact, 0.toByte, None, None, None, binary)
    }
    intercept[BaboonCodecException.EncoderFailure] {
      encode(AnyOpaqueJson(meta, Json.fromInt(1)), 0.toByte)
    }
  }
}

import baboon.runtime.shared._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.funsuite.AnyFunSuite
import ueba.layout.high.Choice

class HighMarkerSpec extends AnyFunSuite {
  test("wrapped branch ordinals cross the signed-byte boundary") {
    def check[T](value: T, codec: BaboonBinCodec[T], ordinal: Int): Unit = {
      val buffer = new ByteArrayOutputStream()
      codec.encode(BaboonCodecContext.Compact, new LEDataOutputStream(buffer), value)
      assert(buffer.toByteArray.toList == List(ordinal.toByte, 0.toByte))
      val _ = assert(codec.decode(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(buffer.toByteArray))) == Right(value))
    }
    check(Choice.B0(), Choice.B0_UEBACodec, 0)
    check(Choice.B127(), Choice.B127_UEBACodec, 127)
    check(Choice.B128(), Choice.B128_UEBACodec, 128)
    assert(Choice.B128_UEBACodec.decode(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(Array[Byte](0, 0)))).isLeft)
  }
}

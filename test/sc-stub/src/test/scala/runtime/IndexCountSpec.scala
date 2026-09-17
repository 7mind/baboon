package runtime

import baboon.runtime.shared._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.funsuite.AnyFunSuite
import testpkg.pkg0.{T1_E2, T1_E2_RET, T1_E2_RET_UEBACodec, T5_A1}

class IndexCountSpec extends AnyFunSuite {
  private val indexed = new BaboonBinCodecIndexed {
    override def indexElementsCount(ctx: BaboonCodecContext): Short = 2
  }

  private def bytes(header: Int, entries: (Int, Int)*): Array[Byte] = {
    val buffer = new ByteArrayOutputStream()
    val writer = new LEDataOutputStream(buffer)
    writer.writeByte(header)
    entries.foreach { case (offset, length) => writer.writeInt(offset); writer.writeInt(length) }
    writer.writeByte(42)
    buffer.toByteArray
  }

  test("count-only index reading preserves entries, validation and cursor consumption") {
    val cases = List(
      bytes(0),
      bytes(1, 0  -> 1, 1 -> 2),
      bytes(1, 0  -> 0, 1 -> 2),
      bytes(1, -1 -> 1, 1 -> 2),
      bytes(1, 0  -> 2, 1 -> 1),
      bytes(1, 0  -> 1),
      Array.emptyByteArray,
    )
    cases.foreach {
      input =>
        val fullStream  = new ByteArrayInputStream(input)
        val countStream = new ByteArrayInputStream(input)
        val full        = indexed.readIndex(BaboonCodecContext.Compact, new LEDataInputStream(fullStream))
        val count       = indexed.readIndexCount(BaboonCodecContext.Compact, new LEDataInputStream(countStream))
        assert(full.map(_.size).left.map(e => (e.getClass, e.getMessage)) == count.left.map(e => (e.getClass, e.getMessage)))
        assert(fullStream.available() == countStream.available())
        assert(fullStream.read() == countStream.read())
    }
  }

  test("both index readers reject zero, negative and overlapping entries through the error channel") {
    val invalid = List(
      bytes(1, 0            -> 0, 1  -> 1),
      bytes(1, -1           -> 1, 1  -> 1),
      bytes(1, 0            -> -1, 1 -> 1),
      bytes(1, 0            -> 2, 1  -> 1),
      bytes(1, Int.MaxValue -> 1, 0  -> 1),
    )
    invalid.foreach {
      input =>
        assert(indexed.readIndex(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(input))).isLeft)
        assert(indexed.readIndexCount(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(input))).isLeft)
    }
  }

  test("generated decoder rejects a missing required index through its error channel") {
    val buffer = new ByteArrayOutputStream()
    T1_E2_RET_UEBACodec.encode(BaboonCodecContext.Compact, new LEDataOutputStream(buffer), T1_E2_RET(T1_E2.A, None))
    val result = T1_E2_RET_UEBACodec.decode(BaboonCodecContext.Indexed, new LEDataInputStream(new ByteArrayInputStream(buffer.toByteArray)))
    assert(result.isLeft)
  }

  test("generated branch decoder rejects a corrupt prefix through its error channel") {
    val buffer = new ByteArrayOutputStream()
    T5_A1.B1_UEBACodec.encode(BaboonCodecContext.Compact, new LEDataOutputStream(buffer), T5_A1.B1("x"))
    val input = buffer.toByteArray
    input(0) = 255.toByte
    assert(T5_A1.B1_UEBACodec.decode(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(input))).isLeft)
  }
}

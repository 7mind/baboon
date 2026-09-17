package runtime

import baboon.runtime.shared._
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import org.scalatest.funsuite.AnyFunSuite

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
}

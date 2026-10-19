package io.septimalmind.baboon.tests

import io.septimalmind.baboon.util.Inflate
import org.scalatest.wordspec.AnyWordSpec

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.Deflater
import scala.util.Random

/** The portable inflater against the JDK's deflater (raw streams, as ZIP method 8 stores them). */
final class InflateTest extends AnyWordSpec {
  private def deflate(data: Array[Byte], level: Int, strategy: Int): Array[Byte] = {
    val deflater = new Deflater(level, true)
    deflater.setStrategy(strategy)
    deflater.setInput(data)
    deflater.finish()
    val out    = new ByteArrayOutputStream()
    val buffer = new Array[Byte](4096)
    while (!deflater.finished()) {
      out.write(buffer, 0, deflater.deflate(buffer))
    }
    deflater.end()
    out.toByteArray
  }

  private val random = new Random(20261019L)
  private val samples: List[(String, Array[Byte])] = List(
    "empty"       -> Array.emptyByteArray,
    "one byte"    -> Array[Byte](42),
    "schema text" -> ("model a.b\nversion \"1.0.0\"\nroot data X {\n  x: i32\n  y: opt[str]\n}\n" * 300).getBytes(StandardCharsets.UTF_8),
    "random"      -> Array.fill(70000)(random.nextInt(256).toByte),
    "skewed"      -> Array.fill(90000)((random.nextGaussian() * 4).toInt.toByte),
    "one symbol"  -> Array.fill(100000)(7.toByte),
  )

  "Inflate" should {
    "reproduce what the JDK deflates, at every level and strategy" in {
      for {
        (name, data) <- samples
        level        <- List(Deflater.NO_COMPRESSION, Deflater.BEST_SPEED, Deflater.DEFAULT_COMPRESSION, Deflater.BEST_COMPRESSION)
        strategy     <- List(Deflater.DEFAULT_STRATEGY, Deflater.FILTERED, Deflater.HUFFMAN_ONLY)
      } {
        val inflated = Inflate.inflate(deflate(data, level, strategy), data.length.toLong)
        assert(inflated.map(_.toSeq) == Right(data.toSeq), s"$name, level $level, strategy $strategy")
      }
    }

    "reject a stream that does not match its declared size, ends early or carries trailing bytes" in {
      val data       = samples(2)._2
      val compressed = deflate(data, Deflater.DEFAULT_COMPRESSION, Deflater.DEFAULT_STRATEGY)
      assert(Inflate.inflate(compressed, data.length.toLong + 1).left.exists(_.contains("not the declared")))
      assert(Inflate.inflate(compressed, data.length.toLong - 1).left.exists(_.contains("more than the declared")))
      assert(Inflate.inflate(compressed.dropRight(1), data.length.toLong).left.exists(_.contains("truncated")))
      assert(Inflate.inflate(compressed :+ 0.toByte, data.length.toLong).left.exists(_.contains("follow the end")))
    }

    "reject malformed blocks" in {
      // BFINAL=1, BTYPE=3
      assert(Inflate.inflate(Array[Byte](0x07), 0L).left.exists(_.contains("block type 3")))
      // a stored block whose length disagrees with its complement
      assert(Inflate.inflate(Array[Byte](0x01, 0x01, 0x00, 0x00, 0x00, 0x41), 1L).left.exists(_.contains("complement")))
      // fixed block: literal 'a', then length 3 at distance 1 ("aaaa"), and at distance 2, which reaches before the start
      assert(Inflate.inflate(Array[Byte](0x4B, 0x04, 0x02, 0x00), 4L).map(new String(_, StandardCharsets.US_ASCII)) == Right("aaaa"))
      assert(Inflate.inflate(Array[Byte](0x4B, 0x04, 0x42, 0x00), 4L).left.exists(_.contains("before the start")))
    }
  }
}

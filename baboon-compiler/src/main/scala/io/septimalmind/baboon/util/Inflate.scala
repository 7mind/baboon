package io.septimalmind.baboon.util

import scala.util.control.NoStackTrace

/** Decoder for raw DEFLATE streams (RFC 1951), the compression of ZIP method 8. Pure Scala, so it runs
  * unchanged on Scala.js, where `java.util.zip` does not exist. A port of zlib's `puff.c` reference
  * decoder: canonical Huffman codes decoded bit by bit, which is plenty for schema-sized entries.
  *
  * Strict: the stream must inflate to exactly `expectedSize` bytes and end with the input; codes that
  * are over-subscribed, or incomplete where RFC 1951 does not allow it, are rejected.
  */
object Inflate {
  private final val MaxBits         = 15
  private final val MaxLitLenCodes  = 286
  private final val MaxDistCodes    = 30
  private final val FixedLitLen     = 288
  private final val CodeLengthCodes = 19
  private final val EndOfBlock      = 256
  private final val MaxArrayLength  = Int.MaxValue - 8

  private val CodeLengthOrder = Array(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15)
  private val LengthBase      = Array(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258)
  private val LengthExtra     = Array(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0)
  private val DistBase =
    Array(1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577)
  private val DistExtra = Array(0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13)

  private final class InflateFailure(message: String) extends RuntimeException(message) with NoStackTrace

  /** Symbols by code length: `count(len)` codes of each length, `symbol` in canonical order. */
  private final class Huffman(val count: Array[Int], val symbol: Array[Int])

  def inflate(input: Array[Byte], expectedSize: Long): Either[String, Array[Byte]] = {
    try {
      Right(new Decoder(input, expectedSize).run())
    } catch {
      case failure: InflateFailure => Left(failure.getMessage)
    }
  }

  private def fail(message: String): Nothing = throw new InflateFailure(message)

  /** Builds a code from per-symbol lengths; the result is < 0 when over-subscribed, > 0 when incomplete. */
  private def construct(lengths: Array[Int], offset: Int, n: Int): (Huffman, Int) = {
    val count = new Array[Int](MaxBits + 1)
    (0 until n).foreach(symbol => count(lengths(offset + symbol)) += 1)
    val symbols = new Array[Int](n)
    if (count(0) == n) {
      (new Huffman(count, symbols), 0)
    } else {
      var left = 1
      var len  = 1
      while (len <= MaxBits && left >= 0) {
        left = (left << 1) - count(len)
        len += 1
      }
      if (left >= 0) {
        val offs = new Array[Int](MaxBits + 1)
        (1 until MaxBits).foreach(l => offs(l + 1) = offs(l) + count(l))
        (0 until n).foreach {
          symbol =>
            val l = lengths(offset + symbol)
            if (l != 0) {
              symbols(offs(l)) = symbol
              offs(l) += 1
            }
        }
      }
      (new Huffman(count, symbols), left)
    }
  }

  private lazy val fixedCodes: (Huffman, Huffman) = {
    val lengths = Array.tabulate(FixedLitLen) {
      symbol =>
        if (symbol < 144) 8 else if (symbol < 256) 9 else if (symbol < 280) 7 else 8
    }
    (construct(lengths, 0, FixedLitLen)._1, construct(Array.fill(MaxDistCodes)(5), 0, MaxDistCodes)._1)
  }

  private final class Decoder(input: Array[Byte], expectedSize: Long) {
    private var inPos  = 0
    private var bitBuf = 0
    private var bitCnt = 0
    private var out    = new Array[Byte](math.min(expectedSize, 1L << 16).toInt.max(16))
    private var outCnt = 0

    def run(): Array[Byte] = {
      var last = false
      while (!last) {
        last = bits(1) == 1
        bits(2) match {
          case 0 => stored()
          case 1 => codes(fixedCodes._1, fixedCodes._2)
          case 2 => dynamic()
          case _ => fail("invalid DEFLATE block type 3")
        }
      }
      if (inPos != input.length) fail(s"${input.length - inPos} byte(s) follow the end of the DEFLATE stream")
      if (outCnt.toLong != expectedSize) fail(s"inflates to $outCnt bytes, not the declared $expectedSize")
      java.util.Arrays.copyOf(out, outCnt)
    }

    private def bits(need: Int): Int = {
      var value = bitBuf
      while (bitCnt < need) {
        if (inPos >= input.length) fail("truncated DEFLATE stream")
        value |= (input(inPos) & 0xFF) << bitCnt
        inPos += 1
        bitCnt += 8
      }
      bitBuf = value >>> need
      bitCnt -= need
      value & ((1 << need) - 1)
    }

    private def emit(byte: Byte): Unit = {
      if (outCnt.toLong >= expectedSize) fail(s"inflates to more than the declared $expectedSize bytes")
      if (outCnt == out.length) {
        val grown = math.min(expectedSize, out.length.toLong * 2)
        if (grown > MaxArrayLength) fail(s"inflates to more than $MaxArrayLength bytes")
        out = java.util.Arrays.copyOf(out, grown.toInt)
      }
      out(outCnt) = byte
      outCnt += 1
    }

    private def stored(): Unit = {
      bitBuf = 0
      bitCnt = 0
      if (inPos + 4 > input.length) fail("truncated DEFLATE stored block header")
      val len  = (input(inPos) & 0xFF) | ((input(inPos + 1) & 0xFF) << 8)
      val nlen = (input(inPos + 2) & 0xFF) | ((input(inPos + 3) & 0xFF) << 8)
      inPos += 4
      if (len != (~nlen & 0xFFFF)) fail("DEFLATE stored block length does not match its complement")
      if (inPos + len > input.length) fail("truncated DEFLATE stored block")
      (0 until len).foreach(i => emit(input(inPos + i)))
      inPos += len
    }

    private def decode(h: Huffman): Int = {
      var code  = 0
      var first = 0
      var index = 0
      var len   = 1
      while (len <= MaxBits) {
        code |= bits(1)
        val count = h.count(len)
        if (code - count < first) return h.symbol(index + (code - first))
        index += count
        first = (first + count) << 1
        code <<= 1
        len += 1
      }
      fail("invalid DEFLATE Huffman code")
    }

    private def codes(litLen: Huffman, dist: Huffman): Unit = {
      var symbol = decode(litLen)
      while (symbol != EndOfBlock) {
        if (symbol < EndOfBlock) {
          emit(symbol.toByte)
        } else {
          val lenSymbol = symbol - 257
          if (lenSymbol >= LengthBase.length) fail(s"invalid DEFLATE length symbol $symbol")
          val length     = LengthBase(lenSymbol) + bits(LengthExtra(lenSymbol))
          val distSymbol = decode(dist)
          if (distSymbol >= DistBase.length) fail(s"invalid DEFLATE distance symbol $distSymbol")
          val distance = DistBase(distSymbol) + bits(DistExtra(distSymbol))
          if (distance > outCnt) fail("DEFLATE distance reaches before the start of the output")
          (0 until length).foreach(_ => emit(out(outCnt - distance)))
        }
        symbol = decode(litLen)
      }
    }

    private def dynamic(): Unit = {
      val nlen  = bits(5) + 257
      val ndist = bits(5) + 1
      val ncode = bits(4) + 4
      if (nlen > MaxLitLenCodes || ndist > MaxDistCodes) fail("DEFLATE dynamic block declares too many codes")
      val lengths = new Array[Int](MaxLitLenCodes + MaxDistCodes)
      (0 until ncode).foreach(i => lengths(CodeLengthOrder(i)) = bits(3))
      val (lenCode, lenErr) = construct(lengths, 0, CodeLengthCodes)
      if (lenErr != 0) fail("invalid DEFLATE code-length code")
      java.util.Arrays.fill(lengths, 0)

      var index = 0
      while (index < nlen + ndist) {
        val symbol = decode(lenCode)
        if (symbol < 16) {
          lengths(index) = symbol
          index += 1
        } else {
          val (len, repeat) = symbol match {
            case 16 =>
              if (index == 0) fail("DEFLATE repeat code with no previous length")
              (lengths(index - 1), 3 + bits(2))
            case 17 => (0, 3 + bits(3))
            case _  => (0, 11 + bits(7))
          }
          if (index + repeat > nlen + ndist) fail("DEFLATE code lengths overrun the declared codes")
          (0 until repeat).foreach(i => lengths(index + i) = len)
          index += repeat
        }
      }
      if (lengths(EndOfBlock) == 0) fail("DEFLATE dynamic block has no end-of-block code")

      // a single-code (incomplete) literal/length or distance code is valid: RFC 1951 §3.2.7
      val (litLen, litErr) = construct(lengths, 0, nlen)
      if (litErr < 0 || (litErr > 0 && nlen != litLen.count(0) + litLen.count(1))) fail("invalid DEFLATE literal/length code")
      val (dist, distErr) = construct(lengths, nlen, ndist)
      if (distErr < 0 || (distErr > 0 && ndist != dist.count(0) + dist.count(1))) fail("invalid DEFLATE distance code")
      codes(litLen, dist)
    }
  }
}

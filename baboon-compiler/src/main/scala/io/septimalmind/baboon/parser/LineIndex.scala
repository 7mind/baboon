package io.septimalmind.baboon.parser

import io.septimalmind.baboon.parser.model.InputOffset

import scala.collection.mutable.ArrayBuffer

/** Line and column (both 1-based) of offsets into one source, from a line index built once.
  * Line starts follow `\n` and a lone `\r`, as in `scala.util.parsing.input.OffsetPosition`, which
  * on Scala.js rebuilds the index for every position it is asked about.
  */
final class LineIndex(source: CharSequence) {
  // every line start, the first line's included, then the end of the source
  private val starts: Array[Int] = {
    val lineStarts = new ArrayBuffer[Int]
    lineStarts += 0
    for (i <- 0 until source.length) {
      if (source.charAt(i) == '\n' || (source.charAt(i) == '\r' && (i == source.length - 1 || source.charAt(i + 1) != '\n'))) {
        lineStarts += i + 1
      }
    }
    lineStarts += source.length
    lineStarts.toArray
  }

  def at(offset: Int): InputOffset = {
    var lo = 0
    var hi = starts.length - 1
    while (lo + 1 < hi) {
      val mid = lo + (hi - lo) / 2
      if (offset < starts(mid)) hi = mid else lo = mid
    }
    InputOffset(offset, lo + 1, offset - starts(lo) + 1)
  }
}

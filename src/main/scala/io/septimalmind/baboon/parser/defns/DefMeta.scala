package io.septimalmind.baboon.parser.defns

import fastparse.{EagerOps, Index, P}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.defns.base.{idt, kw}

import scala.util.parsing.input.OffsetPosition

class DefMeta(context: ParserContext) {
  def positioned[T](
    defparser: => P[T]
  )(implicit v: P[?]
  ): P[(InputPointer, T)] = {
    import fastparse.ScalaWhitespace.whitespace
    (Index ~ defparser ~ Index).map {
      case (start, value, stop) =>
        val begin = DefMeta.makePos(context.content, start)
        val end   = DefMeta.makePos(context.content, stop)
        (InputPointer.Full(context.file, begin, end), value)
    }
  }

  def withMeta[T](defparser: => P[T])(implicit v: P[?]): P[(RawNodeMeta, T)] = {
    P(positioned(defparser)).map {
      case (pos, r) =>
        (RawNodeMeta(pos), r)
    }
  }

  def member[T](
    keyword: => P[Unit],
    defparser: => P[T],
  )(implicit
    v: P[?]
  ): P[(RawNodeMeta, String, T)] = {
    import fastparse.ScalaWhitespace.whitespace
    withMeta(kw(keyword, idt.symbol ~ defparser)).map {
      case (m, (i, t)) => (m, i, t)
    }
  }
}

object DefMeta {
  def makePos(s: CharSequence, index: Int): InputOffset = {
    val pos = OffsetPosition(s, index)
    InputOffset(index, pos.line, pos.column)
  }
}

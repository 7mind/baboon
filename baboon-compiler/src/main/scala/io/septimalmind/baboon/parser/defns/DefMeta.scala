package io.septimalmind.baboon.parser.defns

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.base.{idt, kw}
import io.septimalmind.baboon.parser.model.*

import scala.util.parsing.input.OffsetPosition

class DefMeta(context: ParserContext) {
  // Prefix-doc capture (PR-30.2). Wired into `withMeta` and `member` below
  // so every `RawNodeMeta`-bearing position automatically picks up an
  // optional `/** … */` doc preceding the body. Suffix `//!` capture lives
  // on the field-rule side (DefDto.fieldDef) — see spec §3.3.
  //
  // We read prefix docs BEFORE invoking `positioned(defparser)` so the
  // declaration's `pos` remains anchored on the actual body (existing
  // tests keep their expected ranges). The captured doc is stitched into
  // `RawNodeMeta.docs.prefix`.
  private def prefixDocs[$: P]: P[Option[RawDocComment]] =
    context.defDocs.prefixDocs
  def positioned[T](
    defparser: => P[T]
  )(implicit v: P[?]
  ): P[(InputPointer, T)] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    (Index ~ defparser ~ Index).map {
      case (start, value, stop) =>
        val begin = DefMeta.makePos(context.content, start)
        val end   = DefMeta.makePos(context.content, stop)
        (InputPointer.Full(context.file, begin, end), value)
    }
  }

  def withMeta[T](defparser: => P[T])(implicit v: P[?]): P[(RawNodeMeta, T)] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(prefixDocs ~ positioned(defparser)).map {
      case (doc, (pos, r)) =>
        (RawNodeMeta(pos, RawDocs(doc, None)), r)
    }
  }

  def member[T](
    keyword: => P[Unit],
    defparser: => P[T],
  )(implicit
    v: P[?]
  ): P[(RawNodeMeta, String, T)] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    withMeta(kw(keyword, idt.symbol ~ defparser)).map {
      case (m, (i, t)) => (m, i, t)
    }
  }

  private def simpleTypeRef[$: P]: P[RawTypeRef.Simple] = {
    idt.symbolSeq.map {
      seq =>
        val parts  = seq.map(RawTypeName.apply).toList
        val name   = parts.last
        val prefix = parts.init
        RawTypeRef.Simple(name, prefix)
    }
  }

  private def derivedAnnotation[$: P]: P[RawMemberMeta.Derived] = {
    import fastparse.SingleLineWhitespace.whitespace
    (kw.derived ~ LiteralStr("[") ~ idt.symbol ~ LiteralStr("]")).map(RawMemberMeta.Derived.apply)
  }

  private def wasAnnotation[$: P]: P[RawMemberMeta.Was] = {
    import fastparse.SingleLineWhitespace.whitespace
    (kw.was ~ LiteralStr("[") ~ simpleTypeRef ~ LiteralStr("]")).map(RawMemberMeta.Was.apply)
  }

  private def memberMeta[$: P]: P[RawMemberMeta] = {
    P(derivedAnnotation | wasAnnotation)
  }

  def derived[$: P]: P[Set[RawMemberMeta]] = {
    import io.septimalmind.baboon.parser.defns.base.BaboonWhitespace.whitespace
    P(LiteralStr(":") ~ memberMeta.rep(sep = ",", min = 1)).?.map {
      s => s.map(_.toSet).getOrElse(Set.empty)
    }
  }

}

object DefMeta {
  def makePos(s: CharSequence, index: Int): InputOffset = {
    val pos = OffsetPosition(s, index)
    InputOffset(index, pos.line, pos.column)
  }
}

package io.septimalmind.baboon.parser.defns.base

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.defns.DefMeta
import io.septimalmind.baboon.parser.model.{InputPointer, RawDocComment}
import izumi.fundamentals.platform.language.Quirks.Discarder

// Parser building block for doc-comment capture (M30 / PR-30.2).
//
// Background -- FastParse `ScalaWhitespace` and the doc-comment marker.
// The compiler parses model bodies with `fastparse.ScalaWhitespace.whitespace`
// implicitly in scope wherever a `~` appears. The decompiled state machine
// of `ScalaWhitespace$whitespace$.apply` (verified via `javap`) treats any
// block-comment pair -- INCLUDING the doc-comment form -- as ordinary
// skippable whitespace. Once a `~` triggers the implicit whitespace, a
// leading prefix doc is silently consumed and the doc-rule never gets a
// chance to see it.
//
// Consequence -- the doc rule MUST run BEFORE any `~`-driven implicit
// whitespace boundary. We invoke `prefixDocs` at the entry of `withMeta`
// and `member` (in `DefMeta`), under `NoWhitespace` so the rule itself
// does not trigger whitespace consumption that would eat the marker.
//
// Stacked-doc detection. Per `docs/spec/docstrings.md` §4 (Q3 lock) two
// prefix doc blocks back-to-back with no intervening declaration is a
// parser error. After consuming the first block we manually skip plain
// whitespace (spaces, tabs, newlines -- but NOT comments) under
// `NoWhitespace`, and if the next characters open a second prefix doc we
// set `ParserContext.stackedDocAt` to the second block's position and
// fail the parse. The driver layer (`BaboonParser.BaboonParserImpl.parse`)
// translates the resulting `Parsed.Failure` into
// `ParserIssue.StackedDocComments(pos)`.
object DefDocs {
  // Parser-stage empty-doc check (spec §5.4 / Q4c lock).
  //
  // The full cleanup algorithm (§5.2) is the typer's responsibility -- the
  // parser only carries `raw`. But the parser MUST already silently drop
  // *obviously* empty bodies so that the carrier node carries no `Docs`
  // entry for that slot. We treat a body as empty when every line, after
  // collapsing CRLF -> LF, matches the separator pattern `\s*\*?\s*` -- i.e.
  // is whitespace-only or carries only the conventional Javadoc `*`
  // continuation marker. This covers the degenerate forms the spec §2.6
  // singles out (`/**/`, `/** */`, `/**\n*/`) AND the separator-only multi-
  // line form `/**\n * \n */` whose body is `\n * \n ` -- not whitespace-
  // only by `String.trim` but uniformly a Javadoc separator. [PR-30.2-D06].
  private val separatorLine = """\s*\*?\s*""".r
  def isEmptyDocBody(body: String): Boolean = {
    val normalized = body.replace("\r\n", "\n")
    normalized.split('\n').forall(line => separatorLine.pattern.matcher(line).matches())
  }
}

class DefDocs(context: ParserContext) {

  // Parse a single prefix doc block (open/close per spec §2.6). Returns
  // `None` for the degenerate empty forms (open followed by close with no
  // body, or whitespace-only body) per spec §5.4 -- empty bodies silently
  // drop.
  //
  // Char-wise rule (to avoid `LiteralStr`/`Whitespace` interaction surprises):
  // read the three-char opener, then either
  //   - the standard close path: read body chars up to the close, then
  //     consume the close, OR
  //   - the degenerate four-char `/**/` form, where the second `*` of the
  //     opener is shared with the `*` of the close. Recognised inline as
  //     "after `/**` we immediately see `/`".
  //
  // Operates under `NoWhitespace` so the rule captures contents verbatim
  // and does not consume surrounding whitespace.
  def prefixDoc[$: P]: P[Option[RawDocComment]] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(
      Index ~ "/" ~ "*" ~ "*" ~
      ("/".!.map(_ => "") | ((!("*" ~ "/") ~ AnyChar).rep.! ~ "*" ~ "/")) ~
      Index
    ).map {
      case (start, body, stop) =>
        if (DefDocs.isEmptyDocBody(body)) {
          None
        } else {
          val begin = DefMeta.makePos(context.content, start)
          val end   = DefMeta.makePos(context.content, stop)
          val raw   = "/**" + body + "*/"
          Some(RawDocComment(raw, InputPointer.Full(context.file, begin, end)))
        }
    }
  }

  // Skip plain whitespace AND non-doc comments under `NoWhitespace`. We use
  // this manual skipper to detect back-to-back prefix doc blocks: a second
  // `/**` is a stacked-doc error per spec §4 even when separated from the
  // first by a plain `//` line comment or a plain `/* … */` block comment.
  // Doc markers (`/**` and `//!`) are NOT consumed -- they terminate the
  // skip so the lookahead in `prefixDocs` can detect a stacked second
  // block. [PR-30.2-D03].
  private def plainWs[$: P]: P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P((CharsWhileIn(" \t\r\n", 1) | nonDocComment).rep).map(_ => ())
  }

  // Match a single non-doc comment: a `//` line comment that is NOT the
  // postfix `//!` doc marker, or a `/* … */` block comment that is NOT the
  // prefix `/**` doc opener. Mirrors `BaboonWhitespace`'s comment-skip
  // logic but stops at doc markers instead of continuing past them.
  private def nonDocComment[$: P]: P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    P(
      ("//" ~ !"!" ~ CharsWhile(c => c != '\n' && c != '\r', 0))
      | ("/*" ~ !"*" ~ (!"*/" ~ AnyChar).rep ~ "*/")
    )
  }

  // Optional prefix-doc rule wired into `withMeta` / `member`. Captures at
  // most one prefix doc block. Detects a second back-to-back block and
  // fails the parse with `ParserContext.stackedDocAt` set so the driver
  // surfaces `ParserIssue.StackedDocComments(pos)`.
  def prefixDocs[$: P]: P[Option[RawDocComment]] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    noWhitespaceImplicit.discard()
    // Two-stage rule: first match an optional prefix doc, then look ahead
    // past plain whitespace for a SECOND `/**` -- that's the stacked-docs
    // case. We carry the second-block position out via a side-effect on
    // `ParserContext.stackedDocAt`, because FastParse's `Parsed.Failure`
    // cannot directly carry a typed `ParserIssue`.
    //
    // Structural note: split into a `.map` that records the side-effect,
    // then a `.flatMap` that turns the marker into a parse failure. A
    // single-pass `flatMap` triggered an obscure fastparse-3 macro
    // failure ("Could not find proxy for case val x1") on Scala 2 -- the
    // two-stage form sidesteps the macro's quirk.
    val capture = P((prefixDoc ~ plainWs ~ (Index ~ &("/" ~ "*" ~ "*")).?).?).map {
      case Some((doc, Some(idx2))) =>
        val begin = DefMeta.makePos(context.content, idx2)
        val pos   = InputPointer.Offset(context.file, begin)
        context.stackedDocAt = Some(pos)
        (doc, true)
      case Some((doc, None)) =>
        (doc, false)
      case None =>
        (None: Option[RawDocComment], false)
    }
    capture.flatMap {
      case (_, true)    => Fail.opaque("expected a single prefix doc block; stacked docs are not allowed")
      case (doc, false) => Pass(doc)
    }
  }

  // Parse an optional postfix `//!` line doc. Anchored to end-of-line: the
  // rule reads `//!` followed by content up to (but not including) the
  // next newline or end-of-input. Does NOT consume the terminating
  // newline (callers continue under `BaboonWhitespace` after this rule).
  //
  // Operates under `NoWhitespace`. Empty / whitespace-only bodies
  // silently drop (return `None`) per spec §5.4.
  def suffixDoc[$: P]: P[Option[RawDocComment]] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P((CharsWhileIn(" \t", 0) ~ Index ~ LiteralStr("//!") ~ CharsWhile(c => c != '\r' && c != '\n', 0).! ~ Index).?).map {
      case Some((start, body, stop)) =>
        val trimmed = body.trim
        if (trimmed.isEmpty) {
          None
        } else {
          val begin = DefMeta.makePos(context.content, start)
          val end   = DefMeta.makePos(context.content, stop)
          val raw   = "//!" + body
          Some(RawDocComment(raw, InputPointer.Full(context.file, begin, end)))
        }
      case None =>
        None
    }
  }
}

package io.septimalmind.baboon.parser.defns.base

import fastparse.{ParsingRun, Whitespace}

// Custom whitespace-skipper for Baboon parser sites that must remain
// doc-comment-aware.
//
// Behaviourally identical to `fastparse.ScalaWhitespace.whitespace` for
// spaces, tabs, newlines, line comments, and block comments -- EXCEPT
// that doc-comment markers are NOT consumed:
//
//  - the prefix doc-comment opener (the three-character sequence "/" "*" "*")
//    is left for `DefDocs.prefixDocs` to capture; otherwise
//    `ScalaWhitespace` would eat doc-comments as ordinary block comments
//    between `.rep()` iterations.
//  - the postfix doc marker (the three-character sequence "/" "/" "!")
//    is left for `DefDocs.suffixDoc` to capture; otherwise
//    `ScalaWhitespace` would eat it as an ordinary line comment.
//
// This is the M30 / PR-30.2 design fix: the original plan assumed an
// anchored doc-rule could intercept docs at `withMeta` entry while still
// using `ScalaWhitespace`. In fastparse 3, however, `.rep()` between
// iterations runs the implicit whitespace, which eats the doc before the
// next iteration's `withMeta` even starts. Replacing the implicit
// whitespace with a doc-aware variant gives `withMeta`'s anchored doc
// rule a chance to see the marker.
object BaboonWhitespace {

  implicit val whitespace: Whitespace = (ctx: ParsingRun[?]) => {
    val input    = ctx.input
    var current  = ctx.index
    var continue = true

    while (continue && input.isReachable(current)) {
      val ch = input(current)
      ch match {
        case ' ' | '\t' | '\r' | '\n' =>
          current += 1
        case '/' if input.isReachable(current + 1) =>
          val ch1 = input(current + 1)
          if (ch1 == '/') {
            // either a line comment "//" or the doc-comment marker
            // "//!". Stop iff this is the doc marker.
            if (input.isReachable(current + 2) && input(current + 2) == '!') {
              continue = false
            } else {
              // Skip to end of line (or end of input).
              current += 2
              while (input.isReachable(current) && input(current) != '\n' && input(current) != '\r') {
                current += 1
              }
            }
          } else if (ch1 == '*') {
            // Either a block comment "/*" or the prefix doc opener
            // "/**". Stop unconditionally iff this is "/**" (even for
            // the degenerate "/**/" form -- the doc rule handles it as
            // an empty-body doc and silently drops per spec §5.4).
            if (input.isReachable(current + 2) && input(current + 2) == '*') {
              continue = false
            } else {
              // Plain block comment: skip to "*/" (with nesting per
              // Scala convention).
              current += 2
              var depth = 1
              while (depth > 0 && input.isReachable(current)) {
                val c = input(current)
                if (c == '*' && input.isReachable(current + 1) && input(current + 1) == '/') {
                  depth -= 1
                  current += 2
                } else if (c == '/' && input.isReachable(current + 1) && input(current + 1) == '*') {
                  depth += 1
                  current += 2
                } else {
                  current += 1
                }
              }
            }
          } else {
            continue = false
          }
        case _ =>
          continue = false
      }
    }

    ctx.freshSuccessUnit(current)
  }
}

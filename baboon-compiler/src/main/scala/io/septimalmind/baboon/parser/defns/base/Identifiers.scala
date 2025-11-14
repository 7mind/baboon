package io.septimalmind.baboon.parser.defns.base

import fastparse.{ByNameOps, CharPred, EagerOps, LiteralStr, P}

trait Identifiers {
  import fastparse.CharPredicates.{isDigit, isLetter}

  import fastparse.NoWhitespace.noWhitespaceImplicit

  def symbol[$: P]: P[String] = {
    P(
      (CharPred(c => isLetter(c) | c == '_') ~ CharPred(c => isLetter(c) | isDigit(c) | c == '_').rep).!
    )
  }

  def symbolSeq[$: P]: P[Seq[String]] =
    P(symbol.rep(sep = LiteralStr("."), min = 1))
}

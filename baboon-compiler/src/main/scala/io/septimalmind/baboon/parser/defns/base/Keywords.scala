package io.septimalmind.baboon.parser.defns.base

import fastparse.*
import fastparse.CharPredicates.{isDigit, isLetter}

trait Keywords {
  private def identChar[$: P]: P[Unit] = {
    P(CharPred(c => isLetter(c) | isDigit(c) | c == '_'))
  }

  def kw[$: P](s: String): P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    P(s ~ !identChar)
  }

  def kw[$: P](s: String, alt: String*): P[Unit] = {
    import fastparse.NoWhitespace.noWhitespaceImplicit
    def alts = alt.foldLeft(P(s)) { case (acc, v) => acc | v }
    P(alts ~ !identChar)
  }

  def model[$: P]: P[Unit]     = kw("model")
  def data[$: P]: P[Unit]      = kw("data", "struct")
  def contract[$: P]: P[Unit]  = kw("contract")
  def service[$: P]: P[Unit]   = kw("service")
  def choice[$: P]: P[Unit]    = kw("enum")
  def adt[$: P]: P[Unit]       = kw("adt")
  def foreign[$: P]: P[Unit]   = kw("foreign")
  def root[$: P]: P[Unit]      = kw("root")
  def version[$: P]: P[Unit]   = kw("version")
  def `import`[$: P]: P[Unit]  = kw("import")
  def include[$: P]: P[Unit]   = kw("include")
  def namespace[$: P]: P[Unit] = kw("ns")
  def derived[$: P]: P[Unit]   = kw("derived")
  def was[$: P]: P[Unit]       = kw("was")
  def drop[$: P]: P[Unit]      = kw("drop")
  def pragma[$: P]: P[Unit]    = kw("pragma")

  def apply[T](kw: => P[Unit], defparser: => P[T])(implicit v: P[?]): P[T] = {
    import fastparse.ScalaWhitespace.whitespace
    P(kw ~/ defparser)
  }
}

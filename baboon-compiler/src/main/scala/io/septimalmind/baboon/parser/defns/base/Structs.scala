package io.septimalmind.baboon.parser.defns.base

import fastparse.*

trait Structs {
  import fastparse.ScalaWhitespace.whitespace

  def enclosed[T](defparser: => P[T])(implicit v: P[?]): P[T] = {
    P(inCurlyBraces(defparser) | inBraces(defparser))
  }

  def inCurlyBraces[T](defparser: => P[T])(implicit v: P[?]): P[T] = {
    P("{" ~ defparser ~ "}")
  }

  def inBraces[T](defparser: => P[T])(implicit v: P[?]): P[T] = {
    P("(" ~ defparser ~ ")")
  }
}

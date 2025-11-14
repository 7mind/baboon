package io.septimalmind.baboon.parser.defns.base

import fastparse.*

//trait DefWhitespace {
//  implicit final val whitespace: fastparse.Whitespace =
//    fastparse.ScalaWhitespace.whitespace
//}

trait DefSep {
  def ws[$: P]: P[Unit] = P(" " | "\t")

  def NLC[$: P]: P[Unit] = P("\r\n" | "\n" | "\r")

//  def any[$: P]: P[Unit] = P((ws | NLC).rep)

}
object sep extends DefSep

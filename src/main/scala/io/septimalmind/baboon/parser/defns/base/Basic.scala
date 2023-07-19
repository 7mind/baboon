package io.septimalmind.baboon.parser.defns.base

import fastparse._
import fastparse.CharPredicates._
import NoWhitespace._

object Basic {
  def UnicodeEscape[$: P]: P[Unit] =
    P("u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit)

  //Numbers and digits

  final val digits = "0123456789"
  final val hexDigits = "0123456789abcdefABCDEF"

  def Digit[$: P]: P[Unit] = P(CharIn(digits))
  def HexDigit[$: P]: P[Unit] = P(CharIn(hexDigits))
  def HexNum[$: P]: P[Unit] = P("0x" ~ CharsWhileIn(hexDigits))
  def DecNum[$: P]: P[Unit] = P(CharsWhileIn(digits))
  def Exp[$: P]: P[Unit] = P(CharIn("Ee") ~ CharIn("+\\-").? ~ DecNum)
  def FloatType[$: P]: P[Unit] = P(CharIn("fFdD"))

  def WSChars[$: P]: P[Unit] = P(CharsWhileIn("\u0020\u0009"))
  def Newline[$: P]: P[Unit] = P(StringIn("\r\n", "\n"))
  def Semi[$: P]: P[Unit] = P(";" | Newline.rep(1))
  def OpChar[$: P]: P[Unit] = P(CharPred(isOpChar))

  def isOpChar(c: Char): Boolean =
    c match {
      case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' =>
        true
      case _ => isOtherSymbol(c) || isMathSymbol(c)
    }

  def Letter[$: P]: P[Unit] =
    P(CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_'))
  def LetterDigitDollarUnderscore[$: P]: P[Unit] =
    P(
      CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_')
    )
  def Lower[$: P]: P[Unit] = P(CharPred(c => isLower(c) || c == '$' | c == '_'))
  def Upper[$: P]: P[Unit] = P(CharPred(isUpper))
}

/**
  * Most keywords don't just require the correct characters to match,
  * they have to ensure that subsequent characters *don't* match in
  * order for it to be a keyword. This enforces that rule for key-words
  * (W) and key-operators (O) which have different non-match criteria.
  */
object Key {
  def W[$: P](s: String): P[Unit] =
    P(s ~ !Basic.LetterDigitDollarUnderscore)(
      sourcecode.Name(s"`$s`"),
      implicitly,
    )

  // If the operator is followed by a comment, stop early so we can parse the comment
  def O[$: P](s: String): P[Unit] =
    P(s ~ (!Basic.OpChar | &("/*" | "//")))(
      sourcecode.Name(s"`$s`"),
      implicitly,
    )
}

package io.septimalmind.baboon.parser.defns.base

import fastparse.*
import fastparse.NoWhitespace.*

// based on fastparse/scalaparse

object Literals {

  /**
    * Parses all whitespace, excluding newlines. This is only
    * really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference would work
    */
  def WS[$: P]: P[Unit] =
    P(NoCut(NoTrace((Basic.WSChars | Literals.Comment).rep)))

  /**
    * Parses whitespace, including newlines.
    * This is the default for most things
    */
  def WL0[$: P]: P[Unit] =
    P(NoTrace((Basic.WSChars | Literals.Comment | Basic.Newline).rep)) //(sourcecode.Name("WL"))
  def WL[$: P]: P[Unit] = P(NoCut(WL0))

  def Semi[$: P]: P[Unit] = P(WS ~ Basic.Semi)
  def Semis[$: P]: P[Unit] = P(Semi.rep(1) ~ WS)
  def Newline[$: P]: P[Unit] = P(WL ~ Basic.Newline)

  def NotNewline[$: P]: P0 = P(&(WS ~ !Basic.Newline))
  def ConsumeComments[$: P]: P[Unit] =
    P(
      (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep
    )
  def OneNLMax[$: P]: P0 =
    P(NoCut(WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline))
  def TrailingComma[$: P]: P0 = P(("," ~ WS ~ Basic.Newline).?)

//  def Pattern: P0

  object Literals {

    import Basic._

    def Float[$: P]: P[Unit] = {
      def Thing = P(DecNum ~ Exp.? ~ FloatType.?)

      def Thing2 = P("." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType)

      P("." ~ Thing | DecNum ~ Thing2)
    }

    def Int[$: P]: P[Unit] = P((HexNum | DecNum) ~ CharIn("Ll").?)

    def Bool[$: P]: P[Unit] = P(Key.W("true") | Key.W("false"))

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    def CommentChunk[$: P]: P[Unit] =
      P(
        CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar
      )
    def MultilineComment[$: P]: P0 = P("/*" ~/ CommentChunk.rep ~ "*/")
    def SameLineCharChunks[$: P]: P[Unit] =
      P(CharsWhile(c => c != '\n' && c != '\r') | !Basic.Newline ~ AnyChar)
    def LineComment[$: P]: P[Unit] =
      P("//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End))
    def Comment[$: P]: P0 = P(MultilineComment | LineComment)

    def Null[$: P]: P[Unit] = Key.W("null")

    def OctalEscape[$: P]: P[Unit] = P(Digit ~ Digit.? ~ Digit.?)
    def Escape[$: P]: P[Unit] =
      P("\\" ~/ (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape))

    def Char[$: P]: P[Unit] = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P((Escape | PrintableChar) ~ "'")
    }

    def TQ[$: P]: P[Unit] = P("\"\"\"")

    /**
      * Helper to quickly gobble up large chunks of un-interesting
      * characters. We break out conservatively, even if we don't know
      * it's a "real" escape sequence: worst come to worst it turns out
      * to be a dud and we go back into a CharsChunk next rep
      */
    def StringChars[$: P]: P[Unit] =
      P(CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$'))
    def NonTripleQuoteChar[$: P]: P[Unit] =
      P("\"" ~ "\"".? ~ !"\"" | CharIn("\\$\n"))
    def TripleChars[$: P]: P[Unit] = P((StringChars | NonTripleQuoteChar).rep)
    def TripleTail[$: P]: P[Unit] = P(TQ ~ "\"".rep)
    def SingleChars[$: P](allowSlash: Boolean): P[Unit] = {
      def LiteralSlash = P(if (allowSlash) "\\" else Fail)
      def NonStringEnd = P(!CharIn("\n\"") ~ AnyChar)
      P((StringChars | LiteralSlash | Escape | NonStringEnd).rep)
    }
    def Str[$: P]: P[String] = {
      P {
        (TQ ~/ TripleChars.! ~ TripleTail) |
          ("\"" ~/ SingleChars(false).! ~ "\"")
      }
    }

    def SimpleStr[$: P]: P[String] = {
      P {
        ("\"" ~/ SingleChars(false).! ~ "\"")
      }
    }

//    case class NamedFunction(f: Char => Boolean)
//                            (implicit name: sourcecode.Name) extends (Char => Boolean){
//      def apply(t: Char): Boolean = f(t)
//      override def toString(): String = name.value
//
//    }
//    import CharPredicates._
//    def VarId0[$: P](dollar: Boolean): P[Unit] = P( Lower ~ IdRest(dollar) )
//    def UppercaseId[$: P](dollar: Boolean): P[Unit] = P( Upper ~ IdRest(dollar) )
//    def PlainIdNoDollar[$: P]: P[Unit] = P( UppercaseId(false) | VarId0(false) ).opaque("plain-id")
//    val NotBackTick = NamedFunction(_ != '`')
//    def BacktickId[$: P]: P[Unit] = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
//    def PlainId[$: P]: P[Unit] = P( UppercaseId(true) ~ (!OpChar | &(StringIn("/*", "//"))) ).opaque("plain-id")
//    def Id[$: P]: P[Unit] = P( BacktickId | PlainId ).opaque("id")
//    def IdRest[$: P](allowDollar: Boolean): P[Unit] = {
//
//      val IdCharacter =
//        if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
//        else NamedFunction(c => isLetter(c) || isDigit(c))
//
//      def IdUnderscoreChunk = P( CharsWhileIn("_", 0) ~ CharsWhile(IdCharacter) )
//      P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, 0)).? )
//    }
//    class InterpCtx(interp: Option[() => P[Unit]]) {
//      def Literal[$: P]: P[Unit] = P( ("-".? ~ (Float | Int)) | Bool | String | Null )
//      def Interp[$: P]: P[Unit] = interp match{
//        case None => P ( Fail )
//        case Some(p) => P( "$" ~ PlainIdNoDollar | ("${" ~ p() ~ WL ~ "}") | "$$" )
//      }
//
//
//      def TQ[$: P]: P[Unit] = P( "\"\"\"" )
//      /**
//        * Helper to quickly gobble up large chunks of un-interesting
//        * characters. We break out conservatively, even if we don't know
//        * it's a "real" escape sequence: worst come to worst it turns out
//        * to be a dud and we go back into a CharsChunk next rep
//        */
//      def StringChars[$: P]: P[Unit] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
//      def NonTripleQuoteChar[$: P]: P[Unit] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\\\$\n") )
//      def TripleChars[$: P]: P[Unit] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
//      def TripleTail[$: P]: P[Unit] = P( TQ ~ "\"".rep )
//      def SingleChars[$: P](allowSlash: Boolean): P[Unit] = {
//        def LiteralSlash = P( if(allowSlash) "\\" else Fail )
//        def NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
//        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
//      }
//      def String[$: P]: P[Unit] = {
//        P {
//          (Id ~ TQ ~/ TripleChars ~ TripleTail) |
//            (Id ~ "\"" ~/ SingleChars(true)  ~ "\"") |
//            (TQ ~/ NoInterp.TripleChars ~ TripleTail) |
//            ("\"" ~/ NoInterp.SingleChars(false) ~ "\"")
//        }
//      }
//
//    }
//    def NoInterp[$: P] = new InterpCtx(None)
////    def Pat[$: P] = new InterpCtx(Some(() => l.Pattern))
////    def Expr[$: P] = new InterpCtx(Some(() => Block))

  }

}

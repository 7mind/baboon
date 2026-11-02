// Unified `bytes` JSON hex grammar (https://github.com/7mind/baboon/issues/102, /103):
// ASCII space, TAB, LF, VT, FF, CR, ':' and '-' are ignored; the remainder must be an even
// number of ASCII hex digits. Anything else is a decode failure, never bytes.
//
// Exercises the decoders generated codecs call: `BaboonJsonCodec.decodeByteString` (values)
// and `BaboonJsonCodec.decodeKeyByteString` (map keys).
package runtime

import baboon.runtime.shared.{BaboonJsonCodec, ByteString}
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class BytesHexDecodeSpec extends AnyFunSuite {

  private val valid: List[(String, List[Int])] = List(
    ""             -> Nil,
    "ABCD"         -> List(0xAB, 0xCD),
    "abcd"         -> List(0xAB, 0xCD),
    "aBcD"         -> List(0xAB, 0xCD),
    "AB CD"        -> List(0xAB, 0xCD),
    "AB:CD"        -> List(0xAB, 0xCD),
    "AB-CD"        -> List(0xAB, 0xCD),
    "A B C D"      -> List(0xAB, 0xCD),
    "\tAB\r\nCD "  -> List(0xAB, 0xCD),
    "A:B-C D"      -> List(0xAB, 0xCD),
    "\u000BAB\fCD" -> List(0xAB, 0xCD),
    "00FF7f"       -> List(0x00, 0xFF, 0x7F),
    " - : "        -> Nil,
  )

  private val invalid: List[String] = List(
    "ZZ",
    "AQ",
    "+1",
    "0x01",
    "q80=",
    "ABC",
    "A",
    "-1",
    "٣٣",
    "ＡＡ",
    "aéb",
    "AB CD",
  )

  private def unsigned(bs: ByteString): List[Int] = bs.toArray.toList.map(_ & 0xFF)

  test("valid hex spellings decode as values and map keys") {
    valid.foreach {
      case (input, expected) =>
        assert(ByteString.tryParse(input).map(unsigned).toOption.contains(expected), input)
        assert(BaboonJsonCodec.decodeByteString.decodeJson(Json.fromString(input)).map(unsigned) == Right(expected), input)
        assert(BaboonJsonCodec.decodeKeyByteString(input).map(unsigned).contains(expected), input)
    }
  }

  test("malformed hex is a decode failure, never bytes") {
    invalid.foreach {
      input =>
        assert(ByteString.tryParse(input).failed.toOption.exists(_.isInstanceOf[IllegalArgumentException]), input)
        assert(BaboonJsonCodec.decodeByteString.decodeJson(Json.fromString(input)).isLeft, input)
        assert(BaboonJsonCodec.decodeKeyByteString(input).isEmpty, input)
    }
  }
}

package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import izumi.fundamentals.collections.nonempty.NEString

class ParserTest extends BaboonTest {
  "baboon parser" should {
    "parse sources" in {
      (parser: BaboonParser) =>
        val parsed = parser.parse(
          BaboonParser.Input(
            FSPath.Name(NEString.unsafeFrom("testfile.baboon")),
            """/* test comment */
              |model my.test.model
              |
              |version "1.2.3"
              |
              |enum E1 { A B C }
              |
              |enum E2 {
              | A
              | B
              | C
              |}
              |
              |root data D1 {
              | f1: i08
              | f2: i32
              | f3: opt[str]
              | f4: map[str, str]
              |}
              |
              |adt A1 {
              |  data B1 {
              |     value: D1
              |  }
              |
              |  data B1 {
              |     value: E1
              |  }
              |}
              |""".stripMargin,
          )
        )
//      parsed match {
//        case Left(value) =>
//          value match {
//            case NEList(BaboonIssue.ParserFailed(fail)) =>
//              println(fail.trace())
//          }
//        case Right(value) =>
//          println(value)
//      }
        assert(parsed.isRight)
    }
  }
}

package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEString
import izumi.reflect.TagKK

final class ParserTest extends ParserTestBase[Either]

abstract class ParserTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "baboon parser" should {
    "parse sources" in {
      (parser: BaboonParser[F]) =>
        val input = BaboonParser.Input(
          FSPath.parse(NEString.unsafeFrom("testfile.baboon")),
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

        for {
          parsed <- parser.parse(input)
          _       = assert(parsed.isInstanceOf[RawDomain])
        } yield ()
    }

    "parse field names that start with keyword prefixes" in {
      (parser: BaboonParser[F]) =>
        val input = BaboonParser.Input(
          FSPath.parse(NEString.unsafeFrom("keyword-prefix.baboon")),
          """model keyword.prefix
            |
            |version "1.0.0"
            |
            |data MysticVaultsProfileDetails {
            |  wasOpenedDuringFinishState: bit
            |}
            |""".stripMargin,
        )

        for {
          parsed <- parser.parse(input)
          _       = assert(parsed.isInstanceOf[RawDomain])
        } yield ()
    }
  }
}

package io.septimalmind.baboon

import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.{Parser, ParserContext}
import izumi.fundamentals.collections.nonempty.NonEmptyString

object Main {
  def main(args: Array[String]): Unit = {

    val ctx = ParserContext(
      FSPath.Name(NonEmptyString.unsafeFrom("testfile.baboon")),
      """/* test comment */
        |model my.test.model
        |
        |enum E1 { A B C }
        |
        |enum E2 {
        | A
        | B
        | C
        |}
        |
        |data D1 {
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
        |""".stripMargin
    )
    val parser = new Parser(ctx)

    println(parser.parse())

    println(
      fastparse
        .parse("""data D1 { f1: i08
            | f2: i32 } """.stripMargin, ctx.defDto.dtoEnclosed(_))
    )

    println(
      fastparse
        .parse("""data D1 { } """.stripMargin, ctx.defDto.dtoEnclosed(_))
    )

    println(
      fastparse
        .parse("""data D1 { } """.stripMargin, ctx.defModel.member(_))
    )

  }
}

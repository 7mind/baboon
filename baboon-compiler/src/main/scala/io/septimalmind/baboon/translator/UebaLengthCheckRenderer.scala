package io.septimalmind.baboon.translator

import io.septimalmind.baboon.translator.UebaLayoutPlan.LengthCheck.*
import io.septimalmind.baboon.typer.model.BinReprLen
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

object UebaLengthCheckRenderer {
  def render[V](
    length: BinReprLen,
    equalTo: Int => TextTree[V],
    oneOf: List[Int] => TextTree[V],
    enforce: TextTree[V] => TextTree[V],
  ): TextTree[V] = {
    UebaLayoutPlan
      .lengthChecks(length).map {
        check =>
          val condition = check match {
            case EqualTo(bytes) => equalTo(bytes)
            case AtLeast(bytes) => q"length >= ${bytes.toString}"
            case AtMost(bytes)  => q"length <= ${bytes.toString}"
            case OneOf(bytes)   => oneOf(bytes)
          }
          enforce(condition)
      }.joinN()
  }
}

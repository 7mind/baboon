package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NESet

sealed trait BinReprLen {
  def isVariable: Boolean
  def prefixed(len: Int): BinReprLen
}

object BinReprLen {
  case class Fixed(bytes: Int) extends BinReprLen {

    override def isVariable: Boolean = false

    override def prefixed(len: Int): BinReprLen = Fixed(bytes + len)
  }
  sealed trait Variable extends BinReprLen {
    override def isVariable: Boolean = true
  }

  case class Unknown() extends Variable {
    override def prefixed(len: Int): BinReprLen = this
  }

  case class Alternatives(variants: NESet[Int]) extends Variable {
    override def prefixed(len: Int): BinReprLen =
      Alternatives(NESet(len) ++ variants.map(_ + len))
  }

  case class Range(min: Int, max: Option[Int]) extends Variable {
    override def prefixed(len: Int): BinReprLen =
      Range(min + len, max.map(_ + len))
  }
}

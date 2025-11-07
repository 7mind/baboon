package io.septimalmind.baboon.translator

import izumi.fundamentals.platform.strings.TextTree

trait FQNSymbol[V] {
  def fullyQualified(value: V): V
}

object FQNSymbol {
  implicit class TextTreeExt[V: FQNSymbol](tt: TextTree[V]) {
    def fullyQualified: TextTree[V] = {
      tt.map(v => implicitly[FQNSymbol[V]].fullyQualified(v))
    }
  }
}

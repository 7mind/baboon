package io.septimalmind.baboon.parser.model

import izumi.fundamentals.platform.cache.CachedProductHashcode

case class InputOffset(offset: Int, line: Int, column: Int)
    extends CachedProductHashcode {
  override def toString: String = s"$line:$column"
}

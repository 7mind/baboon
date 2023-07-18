package io.septimalmind.baboon.parser.model

import izumi.fundamentals.platform.cache.CachedProductHashcode

case class RawEnumMember(value: String, meta: RawNodeMeta)
    extends CachedProductHashcode {
  override def toString: String = value
}

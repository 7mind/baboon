package io.septimalmind.baboon.parser.model

import izumi.fundamentals.platform.cache.CachedProductHashcode

case class RawNodeMeta(pos: InputPointer, docs: RawDocs = RawDocs.empty) extends CachedProductHashcode

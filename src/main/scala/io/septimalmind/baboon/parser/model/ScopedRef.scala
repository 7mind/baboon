package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

case class ScopedRef(path: NEList[RawTypeName])

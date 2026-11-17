package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

/** The scope the first name of a reference is looked up from. */
sealed trait RawLookupScope

object RawLookupScope {

  /** The scope of the definition in whose body the reference stands. The parser only produces this. */
  case object Enclosing extends RawLookupScope

  /** The namespace with the given path from the domain root; the root itself when the path is empty.
    *
    * A reference written in the body of a template gets this scope once the template is instantiated:
    * the body is then typed where the instantiation stands, but its names keep denoting what they
    * denote in the namespace that declares the template.
    */
  case class Namespace(path: List[RawTypeName]) extends RawLookupScope
}

case class ScopedRef(path: NEList[RawTypeName], scope: RawLookupScope)

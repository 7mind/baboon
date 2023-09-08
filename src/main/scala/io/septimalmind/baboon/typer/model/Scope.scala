package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEMap

sealed trait Scope[Def]

object Scope {
  case class ScopeName(name: String) extends AnyVal

  case class RootScope[Def](pkg: Pkg, nested: Map[ScopeName, NestedScope[Def]])
      extends Scope[Def]

  sealed trait NestedScope[Def] extends Scope[Def] {
    def name: ScopeName
    def defn: Def
  }

  case class SubScope[Def](name: ScopeName,
                           defn: Def,
                           nested: NEMap[ScopeName, NestedScope[Def]])
      extends NestedScope[Def]

  case class LeafScope[Def](name: ScopeName, defn: Def) extends NestedScope[Def]

  implicit class DebugExt[Def](path: List[Scope[Def]]) {
    private def asPathElement(s: Scope[?]): String = {
      s match {
        case RootScope(pkg, _)     => pkg.toString
        case scope: NestedScope[_] => scope.name.name
      }
    }

    def asStrDebug: String = {
      path.map(v => asPathElement(v)).mkString("/")
    }
  }
}

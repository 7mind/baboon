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

  implicit class DebugExt[Def](scope: Scope[Def]) {
    def debugRepr(defnRepr: Def => String): String = {
      import izumi.fundamentals.platform.strings.IzString.*
      scope match {
        case s: RootScope[Def] =>
          s"${s.pkg.toString}${s.nested.view.values.map(_.debugRepr(defnRepr)).niceList().shift(2)}"
        case s: SubScope[Def] =>
          s"${s.name.name} <- ${defnRepr(s.defn)} ${s.nested.toMap.view.values.map(_.debugRepr(defnRepr)).niceList().shift(2)}"
        case s: LeafScope[Def] =>
          s"${s.name.name} := ${defnRepr(s.defn)}"
      }
    }
  }
}

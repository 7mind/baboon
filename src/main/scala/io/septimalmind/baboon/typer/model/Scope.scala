package io.septimalmind.baboon.typer.model

import izumi.fundamentals.collections.nonempty.NEMap

sealed trait Scope[Def]

object Scope {
  case class ScopeName(name: String) extends AnyVal

  case class RootScope[Def](pkg: Pkg, nested: Map[ScopeName, NestedScope[Def]])
      extends Scope[Def]

  sealed trait ParentWriter[Def] {
    def setParent(parent: Scope[Def]): Unit
  }

  sealed trait WithParent[Def] {
    this: NestedScope[Def] =>

    private var parent: Scope[Def] = null

    def getParent: Scope[Def] = parent

    def unsafeGetWriter: ParentWriter[Def] = {
      new ParentWriter[Def] {
        def setParent(parent: Scope[Def]): Unit = {
          assert(WithParent.this.parent == null)
          WithParent.this.parent = parent
        }
      }
    }
  }

  sealed trait NestedScope[Def] extends Scope[Def] with WithParent[Def] {
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

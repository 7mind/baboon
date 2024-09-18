package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.typer.model.Scope.ScopeUID
import izumi.fundamentals.collections.nonempty.NEMap

sealed trait Scope[Def] {
  def id: ScopeUID

  def map[Def2](defnMap: Def => Def2): Scope[Def2]
}

object Scope {
  case class ScopeUID(id: Int) extends AnyVal {
    override def toString: String = s"[$id]"
  }

  case class ScopeName(name: String) extends AnyVal

  case class RootScope[Def](id: ScopeUID,
                            pkg: Pkg,
                            nested: Map[ScopeName, NestedScope[Def]])
      extends Scope[Def] {

    def map[Def2](defnMap: Def => Def2): RootScope[Def2] = {
      copy(
        nested = nested.view
          .mapValues(_.map(defnMap).asInstanceOf[NestedScope[Def2]])
          .toMap
      )
    }

  }

  sealed trait NestedScope[Def] extends Scope[Def] {
    def name: ScopeName
    def defn: Def
  }

  case class SubScope[Def](id: ScopeUID,
                           name: ScopeName,
                           defn: Def,
                           nested: NEMap[ScopeName, NestedScope[Def]])
      extends NestedScope[Def] {
    def map[Def2](defnMap: Def => Def2): SubScope[Def2] = {
      copy(defn = defnMap(defn), nested = nested.map {
        case (n, v) => (n, v.map(defnMap).asInstanceOf[NestedScope[Def2]])
      })
    }

  }

  case class LeafScope[Def](id: ScopeUID, name: ScopeName, defn: Def)
      extends NestedScope[Def] {
    def map[Def2](defnMap: Def => Def2): LeafScope[Def2] = {
      copy(defn = defnMap(defn))
    }
  }

  implicit class RootScopeExt[Def](scope: RootScope[Def]) {
    def identifyParents: Map[ScopeUID, ScopeUID] = {
      identifyParents(scope)
    }

    def index: Map[ScopeUID, Scope[Def]] = {
      index(scope)
    }

    private def identifyParents(root: RootScope[?]): Map[ScopeUID, ScopeUID] = {
      def identifySubParents(
        scope: NestedScope[?],
        currentParent: ScopeUID
      ): List[(ScopeUID, ScopeUID)] = {
        scope match {
          case s: SubScope[_] =>
            List((s.id, currentParent)) ++ s.nested.toIterable
              .flatMap(n => identifySubParents(n._2, s.id))
          case s: LeafScope[_] =>
            List((s.id, currentParent))
        }
      }
      val list = root.nested.toList
        .flatMap(n => identifySubParents(n._2, root.id))

      assert(list.map(_._1).toSet.size == list.size)
      list.toMap
    }

    private def index[Def](root: RootScope[Def]): Map[ScopeUID, Scope[Def]] = {
      def identifySubParents(
        scope: NestedScope[Def]
      ): List[(ScopeUID, Scope[Def])] = {
        scope match {
          case s: SubScope[_] =>
            List((s.id, s: Scope[Def])) ++ s.nested.toIterable
              .flatMap(n => identifySubParents(n._2))
          case s: LeafScope[_] =>
            List((s.id, s))
        }
      }
      val list = List((root.id, root)) ++ root.nested.toList
        .flatMap(n => identifySubParents(n._2))

      assert(list.map(_._1).toSet.size == list.size)
      list.toMap
    }
  }

  implicit class ScopeExt[Def](scope: Scope[Def]) {
    def debugRepr(defnRepr: Def => String): String = {
      import izumi.fundamentals.platform.strings.IzString.*
      scope match {
        case s: RootScope[Def] =>
          s"${s.id}: ${s.pkg.toString}${s.nested.view.values.map(_.debugRepr(defnRepr)).niceList().shift(2)}"
        case s: SubScope[Def] =>
          s"${s.id}: ${s.name.name} <- ${defnRepr(s.defn)} ${s.nested.toMap.view.values.map(_.debugRepr(defnRepr)).niceList().shift(2)}"
        case s: LeafScope[Def] =>
          s"${s.id}: ${s.name.name} := ${defnRepr(s.defn)}"
      }
    }
  }

}

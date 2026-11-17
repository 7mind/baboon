package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.model.Scope.{NestedScope, RootScope, ScopeUID}

case class FullRawDefn(defn: RawDefn, gcRoot: Boolean)

object FullRawDefn {
  implicit class DebugExt(defn: FullRawDefn) {
    def debugRepr: String = {
      val n = defn.defn.name.name
      val name = defn.defn match {
        case _: RawDto        => s"dto"
        case _: RawIdentifier => s"id"
        case _: RawContract   => s"contract"
        case _: RawEnum       => s"enum"
        case _: RawAdt        => s"adt"
        case _: RawForeign    => s"foreign"
        case _: RawNamespace  => s"namespace"
        case _: RawService    => s"service"
        case _: RawAlias      => s"alias"
      }

      val root = if (defn.gcRoot) { "!" }
      else { "" }
      s"$root$name $n"
    }
  }
}

/** @param declaringAdts for every ADT branch re-emitted by `+ X`, the scope of the ADT whose body declares
  *                      the branch (see [[RawAdtBranchOrigin]]). `parents` holds the ADT that received it.
  */
case class ScopeContext(parents: Map[ScopeUID, ScopeUID], index: Map[ScopeUID, Scope[FullRawDefn]], declaringAdts: Map[ScopeUID, ScopeUID]) {
  /** `index` as seen through [[ExtendedRawDefn]]. `Scope.map` rebuilds a whole subtree, so the tree is
    * mapped once, from its root, instead of on every parent lookup.
    */
  lazy val extendedIndex: Map[ScopeUID, Scope[ExtendedRawDefn]] = {
    val roots = index.values.collect { case root: RootScope[FullRawDefn] => root }.toList
    assert(roots.size == 1, s"BUG: a scope index holds ${roots.size} roots")
    roots.head.map(defn => ExtendedRawDefn(defn, this)).index
  }
}

case class ExtendedRawDefn(origin: FullRawDefn, context: ScopeContext) {
  def defn: RawDefn   = origin.defn
  def gcRoot: Boolean = origin.gcRoot

  def parentOf(s: NestedScope[ExtendedRawDefn]): Scope[ExtendedRawDefn] =
    s.defn.context.extendedIndex(s.defn.context.parents(s.id))

  /** The ADT whose body declares `s`, when `s` is a branch re-emitted into another ADT by `+ X`. */
  def declaringAdtOf(s: NestedScope[ExtendedRawDefn]): Option[Scope[ExtendedRawDefn]] =
    s.defn.context.declaringAdts.get(s.id).map(s.defn.context.extendedIndex)
}

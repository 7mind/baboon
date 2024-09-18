package io.septimalmind.baboon.typer.model

import io.septimalmind.baboon.parser.model.*

case class FullRawDefn(defn: RawDefn, gcRoot: Boolean)

object FullRawDefn {
  implicit class DebugExt(defn: FullRawDefn) {
    def debugRepr: String = {
      val n = defn.defn.name.name
      val name = defn.defn match {
        case _: RawDto       => s"dto"
        case _: RawContract  => s"contract"
        case _: RawEnum      => s"contract"
        case _: RawAdt       => s"adt"
        case _: RawForeign   => s"foreign"
        case _: RawNamespace => s"namespace"
      }

      val root = if (defn.gcRoot) { "!" } else { "" }
      s"$root$name $n"
    }
  }
}

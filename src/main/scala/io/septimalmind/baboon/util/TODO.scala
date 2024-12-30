package io.septimalmind.baboon.util

import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.{Node, StringNode}
import TextTree.*

object TODO {

  implicit class TextTreeGenericOps1[T](target: TextTree[T]) {
    def last: Option[Char] = target match {
      case ValueNode(_)      => None
      case StringNode(value) => value.lastOption
      case Node(chunks)      => chunks.toList.filter(_.nonEmpty).lastOption.flatMap(_.last)
      case Shift(nested, _)  => nested.last
      case Trim(nested)      => nested.last
    }
  }

  implicit class TextTreeCStyleOps[T](target: TextTree[T]) {
    def endC(): TextTree[T] = {
      target.last match {
        case Some('}') => target
        case None      => target
        case _         => q"$target;"
      }
    }
  }

  implicit class TextTreeSeqCStyleOps[T](target: Seq[TextTree[T]]) {
    def joinN(): TextTree[T] = {
      target.join("\n")
    }

    def joinNN(): TextTree[T] = {
      target.join("\n\n")
    }

    def joinCN(): TextTree[T] = {
      if (target.isEmpty) {
        StringNode("")
      } else {
        val withSeparators = target.flatMap {
          t =>
            if (t.last.contains('}')) {
              Seq(t, StringNode("\n"))
            } else {
              Seq(t, StringNode(";\n"))
            }

        }.init

        NEList.from(withSeparators) match {
          case Some(value) =>
            Node(value)
          case None =>
            StringNode("")
        }
      }
    }

  }

}

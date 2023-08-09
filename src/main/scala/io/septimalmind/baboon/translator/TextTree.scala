package io.septimalmind.baboon.translator

import io.septimalmind.baboon.translator.TextTree.{Node, StringNode, ValueNode}
import izumi.fundamentals.collections.nonempty.NonEmptyList

sealed trait TextTree[T] {}

trait TextTreeLowPrio {
  implicit class TextTreeUnknownOps(target: TextTree[?]) {
    def render: String = {
      target match {
        case _: ValueNode[_] =>
          throw new IllegalArgumentException(
            s"Probably TTValue is undefined for $target"
          )
        case s: StringNode[_] =>
          StringNode.StringNodeOps(s).render
        case n: Node[_] =>
          n.chunks.map(c => TextTreeUnknownOps(c).render).mkString
      }
    }
  }
}

object TextTree extends TextTreeLowPrio {
  trait TTValue[T] {
    def render(value: T): String
  }

  object TTValue {
    def apply[T: TTValue]: TTValue[T] = implicitly[TTValue[T]]
  }

  case class ValueNode[T](value: T) extends TextTree[T]
  object ValueNode {
    implicit class ValueNodeOps[T: TextTree.TTValue](target: ValueNode[T]) {
      def render: String = TTValue[T].render(target.value)
    }
  }

  case class StringNode[T](value: String) extends TextTree[T]
  object StringNode {
    implicit class StringNodeOps(target: StringNode[?]) {
      def render: String = target.value
    }
  }

  case class Node[T](chunks: NonEmptyList[TextTree[T]]) extends TextTree[T]

  object Node {
    implicit class NodeOps[T: TextTree.TTValue](target: Node[T]) {
      def render: String =
        target.chunks.map(c => TextTreeOps(c).render).mkString
    }
  }

  implicit class TextTreeOps[T: TextTree.TTValue](target: TextTree[T]) {
    def render: String = {
      target match {
        case v: ValueNode[T]  => ValueNode.ValueNodeOps[T](v).render
        case s: StringNode[T] => StringNode.StringNodeOps(s).render
        case n: Node[T]       => Node.NodeOps[T](n).render
      }
    }

    def flatten: Node[T] = {
      target match {
        case v: ValueNode[T]  => Node(NonEmptyList(v))
        case s: StringNode[T] => Node(NonEmptyList(s))
        case n: Node[T]       => Node(n.chunks.flatMap(_.flatten.chunks))
      }
    }

    def map[U](f: T => U): TextTree[U] = {
      target match {
        case v: ValueNode[T]  => ValueNode(f(v.value))
        case s: StringNode[T] => StringNode(s.value)
        case n: Node[T]       => Node(n.chunks.map(_.map(f)))
      }
    }
  }

  implicit class Quote(val sc: StringContext) extends AnyVal {
    def q[T](args: InterpolationArg[T]*): Node[T] = {
      assert(sc.parts.length == args.length + 1)

      val seq = sc.parts
        .zip(args)
        .flatMap {
          case (t, v) =>
            List(StringNode[T](t), v.asNode)
        }
        .reverse

      Node(NonEmptyList(StringNode[T](sc.parts.last), seq).reverse)
    }
  }

  trait InterpolationArg[T] {
    def asNode: TextTree[T]
  }

  object InterpolationArg {
    implicit def forT[T](t: T): InterpolationArg[T] = new InterpolationArg[T] {
      override def asNode: TextTree[T] = ValueNode(t)
    }

    implicit def forNodeT[T](node: TextTree[T]): InterpolationArg[T] =
      new InterpolationArg[T] {
        override def asNode: TextTree[T] = node
      }

    implicit def forNodeNothing[T](
      node: TextTree[Nothing]
    ): InterpolationArg[T] =
      new InterpolationArg[T] {
        override def asNode: TextTree[T] = node.asInstanceOf[TextTree[T]]
      }

  }
}

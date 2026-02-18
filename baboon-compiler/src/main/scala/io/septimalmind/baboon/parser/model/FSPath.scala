package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.cache.CachedProductHashcode

sealed trait FSPath extends Product with CachedProductHashcode {
  def name: NEString

  def asString: String

  def segments: Seq[NEString]

  def rename(update: NEString => NEString): FSPath

  def move(update: Seq[NEString] => Seq[NEString]): FSPath = {
    this match {
      case FSPath.Full(location, name) =>
        FSPath(update(location) :+ name)
      case FSPath.Relative(location, name) =>
        FSPath(update(location) :+ name)
      case FSPath.Name(name) =>
        FSPath(update(Seq.empty) :+ name)
    }
  }
}

object FSPath {

  final case class Full private[FSPath] (location: Seq[NEString], name: NEString) extends FSPath {
    override def segments: Seq[NEString] = location :+ name

    override def asString: String = {
      "/" + (location :+ name).map(_.theString).mkString("/")
    }

    override def rename(update: NEString => NEString): FSPath =
      Full(location, update(name))
    override def toString: String = asString
  }

  final case class Relative private[FSPath] (location: Seq[NEString], name: NEString) extends FSPath {
    override def segments: Seq[NEString] = location :+ name

    override def asString: String = {
      (location :+ name).map(_.theString).mkString("/")
    }

    override def rename(update: NEString => NEString): FSPath =
      Full(location, update(name))
    override def toString: String = asString
  }

  final case class Name private[FSPath] (name: NEString) extends FSPath {
    override def asString: String = name.theString

    override def segments: Seq[NEString] = Seq(name)

    override def rename(update: NEString => NEString): FSPath =
      Name(update(name))

    override def toString: String = s"${name.theString}"
  }

  def apply(pkg: Seq[NEString]): FSPath = {
    val path = pkg.init
    val name = pkg.last

    assert(name.nonEmpty)

    if (path.nonEmpty) {
      assert(path.tail.forall(p => p.nonEmpty && !p.theString.contains("/")))

      if (path.head.isEmpty) {
        FSPath.Full(path, name)
      } else {
        FSPath.Relative(path, name)
      }
    } else {
      FSPath.Name(name)
    }
  }

  def parse(path: NEString): FSPath = {
    val pathStr    = path.theString
    val isAbsolute = pathStr.startsWith("/")
    val parts      = pathStr.split("/").toIndexedSeq.filter(_.nonEmpty).map(NEString.unsafeFrom)

    if (parts.isEmpty) {
      Name(path)
    } else if (parts.size == 1) {
      Name(parts.head)
    } else if (isAbsolute) {
      Full(parts.init, parts.last)
    } else {
      Relative(parts.init, parts.last)
    }
  }
}

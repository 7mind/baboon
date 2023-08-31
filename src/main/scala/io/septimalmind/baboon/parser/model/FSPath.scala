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
      case FSPath.Name(name) =>
        FSPath(update(Seq.empty) :+ name)
    }
  }
}

object FSPath {

  final case class Full protected (location: Seq[NEString],
                                   name: NEString)
      extends FSPath {
    override def segments: Seq[NEString] = location :+ name

    override def asString: String =
      (location :+ name).map(_.theString).mkString("/", "/", "")

    override def rename(update: NEString => NEString): FSPath =
      Full(location, update(name))

    override def toString: String = asString
  }

  final case class Name protected (name: NEString) extends FSPath {
    override def asString: String = name.theString

    override def segments: Seq[NEString] = Seq(name)

    override def rename(update: NEString => NEString): FSPath =
      Name(update(name))

    override def toString: String = s"${name.theString}"
  }

  def apply(pkg: Seq[NEString]): FSPath = {
    val path = pkg.init
    val name = pkg.last
    assert(path.forall(p => p.nonEmpty && !p.theString.contains("/")))
    assert(name.nonEmpty)

    if (path.nonEmpty) {
      FSPath.Full(path, name)
    } else {
      FSPath.Name(name)
    }
  }

  def parse(path: NEString): FSPath = {
    val parts = path.theString.split("/").toIndexedSeq.dropWhile(_.isEmpty)
    apply(parts.map(NEString.unsafeFrom))
  }
}

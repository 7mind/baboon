package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NonEmptyString
import izumi.fundamentals.platform.cache.CachedProductHashcode

sealed trait FSPath extends Product with CachedProductHashcode {
  def name: NonEmptyString

  def asString: String

  def segments: Seq[NonEmptyString]

  def rename(update: NonEmptyString => NonEmptyString): FSPath

  def move(update: Seq[NonEmptyString] => Seq[NonEmptyString]): FSPath = {
    this match {
      case FSPath.Full(location, name) =>
        FSPath(update(location) :+ name)
      case FSPath.Name(name) =>
        FSPath(update(Seq.empty) :+ name)
    }
  }
}

object FSPath {

  final case class Full protected (location: Seq[NonEmptyString],
                                   name: NonEmptyString)
      extends FSPath {
    override def segments: Seq[NonEmptyString] = location :+ name

    override def asString: String =
      (location :+ name).map(_.theString).mkString("/", "/", "")

    override def rename(update: NonEmptyString => NonEmptyString): FSPath =
      Full(location, update(name))

    override def toString: String = asString
  }

  final case class Name protected (name: NonEmptyString) extends FSPath {
    override def asString: String = name.theString

    override def segments: Seq[NonEmptyString] = Seq(name)

    override def rename(update: NonEmptyString => NonEmptyString): FSPath =
      Name(update(name))

    override def toString: String = s"${name.theString}"
  }

  def apply(pkg: Seq[NonEmptyString]): FSPath = {
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

  def parse(path: NonEmptyString): FSPath = {
    val parts = path.theString.split("/").toIndexedSeq.dropWhile(_.isEmpty)
    apply(parts.map(NonEmptyString.unsafeFrom))
  }
}

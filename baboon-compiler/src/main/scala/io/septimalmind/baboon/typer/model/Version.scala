package io.septimalmind.baboon.typer.model

case class Version(version: String) {
  override def toString: String = s"{$version}"

  def compareTo(o: Version)(implicit ord: Ordering[ParsedVersion]): Int = {
    (for {
      tv <- ParsedVersion.parse(version)
      ov <- ParsedVersion.parse(o.version)
    } yield {
      tv.compare(ov)
    }).getOrElse(version.compareTo(o.version))
  }
  def >(o: Version)(implicit ord: Ordering[ParsedVersion]): Boolean = {
    compareTo(o) > 0
  }

  def <(o: Version)(implicit ord: Ordering[ParsedVersion]): Boolean = o > this

  def <=(o: Version)(implicit ord: Ordering[ParsedVersion]): Boolean = !(this > o)

  def >=(o: Version)(implicit ord: Ordering[ParsedVersion]): Boolean = !(this < o)
}

object Version {
  implicit val ordering: Ordering[Version] = (x: Version, y: Version) => x.compareTo(y)
}

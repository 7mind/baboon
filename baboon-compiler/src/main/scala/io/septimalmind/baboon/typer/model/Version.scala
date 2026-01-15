package io.septimalmind.baboon.typer.model

case class Version(v: izumi.fundamentals.platform.versions.Version) {
  override def toString: String = v.toString

  def compareTo(o: Version)(implicit ord: Ordering[izumi.fundamentals.platform.versions.Version]): Int = {
    ord.compare(v, o.v)
  }

  def >(o: Version)(implicit ord: Ordering[izumi.fundamentals.platform.versions.Version]): Boolean = {
    compareTo(o) > 0
  }

  def <(o: Version)(implicit ord: Ordering[izumi.fundamentals.platform.versions.Version]): Boolean = o > this

  def <=(o: Version)(implicit ord: Ordering[izumi.fundamentals.platform.versions.Version]): Boolean = !(this > o)

  def >=(o: Version)(implicit ord: Ordering[izumi.fundamentals.platform.versions.Version]): Boolean = !(this < o)

  def format(prefix: String = "", delimiter: String): String = {
    s"$prefix${toString.replace(".", delimiter)}"
  }
}

object Version {
  implicit val ordering: Ordering[Version] = (x: Version, y: Version) => x.compareTo(y)

  def parse(versionString: String) = Version(izumi.fundamentals.platform.versions.Version.parse(versionString))
}

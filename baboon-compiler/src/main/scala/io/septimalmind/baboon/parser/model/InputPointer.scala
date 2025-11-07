package io.septimalmind.baboon.parser.model

sealed trait InputPointer extends InputPointer.ComparisonHack

object InputPointer {
  trait InputPointerOrdering extends Ordering[InputPointer] {
    def compare(x: InputPointer, y: InputPointer): Int = {
      (x, y) match {
        case (x1: StartOffsetKnown, y1: StartOffsetKnown) =>
          implicitly[Ordering[(String, Int)]].compare(
            (x1.file.asString, x1.start.offset),
            (y1.file.asString, y1.start.offset),
          )
        case (x1: JustFile, y1: JustFile) =>
          implicitly[Ordering[String]]
            .compare(x1.file.asString, y1.file.asString)
        case (Undefined, Undefined) =>
          0
        case (_: JustFile, Undefined) =>
          1
        case (Undefined, _: JustFile) =>
          -1
        case (_: StartOffsetUnknown, _: StartOffsetKnown) =>
          -1
        case (_: StartOffsetKnown, _: StartOffsetUnknown) =>
          1
      }
    }
  }

  implicit object InputPointerOrderingInstance extends InputPointerOrdering

  trait ComparisonHack {
    // TODO: this is a very dirty solution for comparison problem in LoaderTest
    override def hashCode(): Int = 0

    override def equals(obj: Any): Boolean = this.getClass == obj.getClass
  }

  sealed trait StartOffsetUnknown extends InputPointer

  sealed trait FileKnown extends InputPointer {
    def file: FSPath
  }

  sealed trait StartOffsetKnown extends FileKnown {
    def file: FSPath

    def start: InputOffset
  }

  case object Undefined extends StartOffsetUnknown

  case class JustFile(file: FSPath) extends FileKnown with StartOffsetUnknown

  case class Offset(file: FSPath, start: InputOffset) extends StartOffsetKnown

  case class Full(file: FSPath, start: InputOffset, stop: InputOffset) extends StartOffsetKnown

  def from(path: FSPath, pos: Option[InputOffset]): InputPointer = {
    pos.map(p => Offset(path, p)).getOrElse(JustFile(path))
  }

  def format(pointer: InputPointer): Seq[String] = {
    pointer match {
      case InputPointer.Undefined =>
        Seq.empty
      case InputPointer.JustFile(file) =>
        Seq(s"Defined in $file")
      case s: InputPointer.StartOffsetKnown =>
        Seq(
          s"Defined at ${s.file.asString} @ ${s.start.line}:${s.start.column}"
        )
    }
  }
}

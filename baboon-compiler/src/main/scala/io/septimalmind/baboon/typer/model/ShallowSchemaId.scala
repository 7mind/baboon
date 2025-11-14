package io.septimalmind.baboon.typer.model

object ShortId {
  def toBytes(hex: String): Seq[Byte] = {
    assert(hex.length % 2 == 0) // only manage canonical case
    hex.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toSeq
  }

  def make(hex: String, clz: String): String = {
    val bytes = toBytes(hex)
    assert(bytes.length == 32)
    val grouped = bytes
      .grouped(8)
      .foldLeft(Seq.fill(4)(0.toByte)) {
        case (acc, chunk) =>
          acc.zip(chunk).map {
            case (o, n) =>
              (o ^ n).toByte
          }
      }
      .toArray

    import izumi.fundamentals.platform.bytes.IzBytes.*

    s"${clz}_${grouped.toHex}"
  }

}

case class ShallowSchemaId(id: String) {
  override def toString: String = ShortId.make(id, "S")
}

case class DeepSchemaId(id: String) {
  override def toString: String = ShortId.make(id, "D")

}

package baboon.runtime.shared {
  trait BaboonGenerated {}
  trait BaboonAdtMemberMeta {}
  trait BaboonGeneratedLatest {}
  trait BaboonTypeCodecs {}
  trait BaboonAbstractConversion[F, T] {
    def doConvert[C](
      context: C,
      conversions: BaboonAbstractConversions,
      from: F,
    ): T

    def versionFrom: String
    def versionTo: String
    def typeId: String
  }

  trait BaboonAbstractConversions {
    def register[F, T](conversion: BaboonAbstractConversion[F, T]): Unit = ???

    def convertWithContext[C, F, T](
      context: C,
      from: F,
    ): T = ???

    def versionsFrom: List[String]
    def versionTo: String
  }

  trait BaboonAbstractCodecs {}

  trait BaboonEnum[T] {
    def parse(s: String): Option[T]
  }

  trait BaboonMeta {}
}

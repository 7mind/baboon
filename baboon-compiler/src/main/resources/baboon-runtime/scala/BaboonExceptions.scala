package baboon.runtime.shared {

  trait BaboonCodecException extends Exception {
    def message: String
    override def getMessage: String = message
  }

  object BaboonCodecException {

    final case class EncoderFailure(message: String, cause: Throwable = null) extends Exception(message, cause) with BaboonCodecException

    final case class DecoderFailure(message: String, cause: Throwable = null) extends Exception(message, cause) with BaboonCodecException

    final case class ConverterFailure(message: String, cause: Throwable = null) extends Exception(message, cause) with BaboonCodecException

    final case class CodecNotFound(message: String) extends Exception(message) with BaboonCodecException

    final case class ConversionNotFound(message: String) extends Exception(message) with BaboonCodecException
  }
}

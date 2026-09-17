package baboon.runtime.shared

sealed class BaboonCodecException(override val message: String, cause: Throwable? = null) : Exception(message, cause) {
    class EncoderFailure(message: String, cause: Throwable? = null) : BaboonCodecException(message, cause)
    class DecoderFailure(message: String, cause: Throwable? = null) : BaboonCodecException(message, cause)
    class ConverterFailure(message: String, cause: Throwable? = null) : BaboonCodecException(message, cause)
    class CodecNotFound(message: String) : BaboonCodecException(message)
    class ConversionNotFound(message: String) : BaboonCodecException(message)
}

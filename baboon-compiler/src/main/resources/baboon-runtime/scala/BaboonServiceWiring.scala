package baboon.runtime.shared {

  case class BaboonMethodId(serviceName: String, methodName: String)

  sealed trait BaboonWiringError
  object BaboonWiringError {
    case class NoMatchingMethod(method: BaboonMethodId) extends BaboonWiringError
    case class DecoderFailed(method: BaboonMethodId, exception: Throwable) extends BaboonWiringError
    case class EncoderFailed(method: BaboonMethodId, exception: Throwable) extends BaboonWiringError
    case class CallFailed(method: BaboonMethodId, domainError: Any) extends BaboonWiringError
  }

  class BaboonWiringException(val error: BaboonWiringError) extends RuntimeException(error.toString)

}

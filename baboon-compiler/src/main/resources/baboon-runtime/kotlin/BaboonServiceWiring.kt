package baboon.runtime.shared

data class BaboonMethodId(val serviceName: String, val methodName: String)

sealed class BaboonWiringError {
    data class NoMatchingMethod(val method: BaboonMethodId) : BaboonWiringError()
    data class DecoderFailed(val method: BaboonMethodId, val exception: Throwable) : BaboonWiringError()
    data class EncoderFailed(val method: BaboonMethodId, val exception: Throwable) : BaboonWiringError()
    data class CallFailed(val method: BaboonMethodId, val domainError: Any) : BaboonWiringError()
}

class BaboonWiringException(val error: BaboonWiringError) : RuntimeException(error.toString())

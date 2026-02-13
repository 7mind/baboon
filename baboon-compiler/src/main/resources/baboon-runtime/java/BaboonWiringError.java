package baboon.runtime.shared;

public sealed interface BaboonWiringError {
    record NoMatchingMethod(BaboonMethodId method) implements BaboonWiringError {}
    record DecoderFailed(BaboonMethodId method, Throwable cause) implements BaboonWiringError {}
    record EncoderFailed(BaboonMethodId method, Throwable cause) implements BaboonWiringError {}
    record CallFailed(BaboonMethodId method, Object cause) implements BaboonWiringError {}
}

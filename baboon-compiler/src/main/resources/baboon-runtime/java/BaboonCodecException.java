package baboon.runtime.shared;

public abstract class BaboonCodecException extends RuntimeException {
    protected BaboonCodecException(String message) {
        super(message);
    }

    protected BaboonCodecException(String message, Throwable cause) {
        super(message, cause);
    }

    public static final class EncoderFailure extends BaboonCodecException {
        public EncoderFailure(String message) { super(message); }
        public EncoderFailure(String message, Throwable cause) { super(message, cause); }
    }

    public static final class DecoderFailure extends BaboonCodecException {
        public DecoderFailure(String message) { super(message); }
        public DecoderFailure(String message, Throwable cause) { super(message, cause); }
    }

    public static final class ConverterFailure extends BaboonCodecException {
        public ConverterFailure(String message) { super(message); }
        public ConverterFailure(String message, Throwable cause) { super(message, cause); }
    }

    public static final class CodecNotFound extends BaboonCodecException {
        public CodecNotFound(String message) { super(message); }
    }

    public static final class ConversionNotFound extends BaboonCodecException {
        public ConversionNotFound(String message) { super(message); }
    }
}

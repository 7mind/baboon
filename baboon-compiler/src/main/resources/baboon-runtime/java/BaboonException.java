package baboon.runtime.shared;

public class BaboonException extends RuntimeException {
    public BaboonException(String message) {
        super(message);
    }

    public BaboonException(String message, Throwable cause) {
        super(message, cause);
    }
}

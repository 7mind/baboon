package baboon.runtime.shared;

public class BaboonWiringException extends BaboonException {
    private final BaboonWiringError error;

    public BaboonWiringException(BaboonWiringError error) {
        super("Wiring error: " + error.toString());
        this.error = error;
    }

    public BaboonWiringError getError() {
        return error;
    }
}

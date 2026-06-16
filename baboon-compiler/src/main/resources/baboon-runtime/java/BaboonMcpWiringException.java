package baboon.runtime.shared;

// MCP-tier carrier for BaboonMcpWiringError (tasks:T111).
//
// The structural twin of BaboonWiringException: a thrown programmer error
// carrying a tagged BaboonMcpWiringError value. DuplicateTool propagates to
// the integrator exactly as DuplicateService does, just from the MCP-only
// runtime. Lives here rather than the always-shipped runtime so that
// non-MCP output stays byte-identical to the pre-MCP baseline.
public class BaboonMcpWiringException extends RuntimeException {
    private final BaboonMcpWiringError error;

    public BaboonMcpWiringException(BaboonMcpWiringError error) {
        super("MCP wiring error: " + error.toString());
        this.error = error;
    }

    public BaboonMcpWiringError getError() {
        return error;
    }
}

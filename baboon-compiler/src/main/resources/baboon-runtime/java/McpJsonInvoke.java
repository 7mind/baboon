package baboon.runtime.shared;

// The JSON `tools/call` delegate the generated server supplies: it routes
// one tool invocation into the already-generated service dispatch (the
// errors-mode `invokeJson`, which returns the service-result container). The
// MCP layer turns that `BaboonEither<BaboonWiringError, String>` into
// Channel-A / Channel-B per the wire contract (K4 §3). The codecs are reached
// exclusively through this delegate; the MCP runtime holds no codec logic itself.
@FunctionalInterface
public interface McpJsonInvoke<Ctx> {
    BaboonEither<BaboonWiringError, String> invoke(
            BaboonMethodId method,
            String data,
            Ctx ctx,
            BaboonCodecContext codecCtx);
}

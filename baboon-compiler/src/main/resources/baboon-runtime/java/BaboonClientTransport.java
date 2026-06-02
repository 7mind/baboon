package baboon.runtime.shared;

import java.util.concurrent.CompletableFuture;

/**
 * Transport-callback contracts for generated RPC clients.
 *
 * A generated {@code ${Svc}Client} holds one of these per active wire
 * format (JSON and/or UEBA). The client encodes a method's input to the
 * wire form, calls the transport with {@code (serviceName, methodName,
 * data)}, and decodes the response. The transport is supplied by the host
 * application and is responsible for actually moving bytes (HTTP, queue,
 * in-process dispatch, etc.).
 *
 * Sync variants return the bare wire type and may throw; they back the
 * synchronous client emitted when {@code asyncServices} is off. Async
 * variants return a {@link CompletableFuture} of the wire type; they back
 * the {@code CompletableFuture<T>}-returning client emitted when
 * {@code asyncServices} is on.
 */
public final class BaboonClientTransport {
    private BaboonClientTransport() {}

    @FunctionalInterface
    public interface JsonSync {
        String apply(String service, String method, String data) throws Exception;
    }

    @FunctionalInterface
    public interface UebaSync {
        byte[] apply(String service, String method, byte[] data) throws Exception;
    }

    @FunctionalInterface
    public interface JsonAsync {
        CompletableFuture<String> apply(String service, String method, String data);
    }

    @FunctionalInterface
    public interface UebaAsync {
        CompletableFuture<byte[]> apply(String service, String method, byte[] data);
    }

    // Context-carrying transport variants, used by the generated client when a
    // service.context mode (`abstract` / `type`) is active. The service context
    // {@code Ctx} is forwarded as the leading argument so the host transport can
    // attach it to the outgoing request. The context-free variants above are
    // left untouched so `--service-context-mode none` clients are byte-identical.

    @FunctionalInterface
    public interface JsonSyncCtx<Ctx> {
        String apply(Ctx ctx, String service, String method, String data) throws Exception;
    }

    @FunctionalInterface
    public interface UebaSyncCtx<Ctx> {
        byte[] apply(Ctx ctx, String service, String method, byte[] data) throws Exception;
    }

    @FunctionalInterface
    public interface JsonAsyncCtx<Ctx> {
        CompletableFuture<String> apply(Ctx ctx, String service, String method, String data);
    }

    @FunctionalInterface
    public interface UebaAsyncCtx<Ctx> {
        CompletableFuture<byte[]> apply(Ctx ctx, String service, String method, byte[] data);
    }
}

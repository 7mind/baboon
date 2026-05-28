package baboon.runtime.shared;

/**
 * Service-side wrapper contract for cross-domain JSON dispatch.
 *
 * Carries a {@code serviceName} for routing and an {@code invoke}
 * routing function. The {@code R} type parameter encodes the return
 * shape so the same interface supports the noErrors mode (R = String)
 * and the errors mode (R = container-of-String, e.g.
 * {@code BaboonEither<BaboonWiringError, String>}). Java has no native
 * sync/async polymorphism via type-only mechanisms, so an asynchronous
 * variant would parameterise R as {@code CompletableFuture<String>} —
 * but the current Java backend has no asyncServices flag, so that
 * spelling is not emitted today.
 *
 * The per-domain {@code ${Svc}Wiring.JsonService} nested classes
 * emitted by the generator implement this interface and bake the
 * underlying impl (and, in errors mode, the {@code IBaboonServiceRt}
 * and any service-context value) at construction time so the runtime
 * contract stays uniform across modes.
 */
public interface IBaboonJsonService<R> {
    String serviceName();
    R invoke(BaboonMethodId method, String data, BaboonCodecContext ctx) throws Exception;
}

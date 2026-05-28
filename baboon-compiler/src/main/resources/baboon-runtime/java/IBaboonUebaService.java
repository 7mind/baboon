package baboon.runtime.shared;

/**
 * Service-side wrapper contract for cross-domain UEBA dispatch.
 *
 * Carries a {@code serviceName} for routing and an {@code invoke}
 * routing function. The {@code R} type parameter encodes the return
 * shape so the same interface supports the noErrors mode (R = byte[])
 * and the errors mode (R = container-of-byte[], e.g.
 * {@code BaboonEither<BaboonWiringError, byte[]>}).
 *
 * See {@link IBaboonJsonService} for the broader contract notes.
 */
public interface IBaboonUebaService<R> {
    String serviceName();
    R invoke(BaboonMethodId method, byte[] data, BaboonCodecContext ctx) throws Exception;
}

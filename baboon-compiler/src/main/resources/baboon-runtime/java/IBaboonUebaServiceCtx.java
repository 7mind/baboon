package baboon.runtime.shared;

/**
 * Context-carrying service-side wrapper contract for cross-domain UEBA
 * dispatch.
 *
 * Emitted-code counterpart to {@link IBaboonUebaService} for the
 * service-context modes (`abstract` / `type`): the service context
 * {@code Ctx} is supplied per {@code invoke} (alongside the codec
 * context) instead of being baked into the wrapper at construction time.
 * See {@link IBaboonJsonServiceCtx} for the broader contract notes.
 */
public interface IBaboonUebaServiceCtx<Ctx, R> {
    String serviceName();
    R invoke(BaboonMethodId method, byte[] data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception;
}

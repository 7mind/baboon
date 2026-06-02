package baboon.runtime.shared;

/**
 * Context-carrying service-side wrapper contract for cross-domain JSON
 * dispatch.
 *
 * Emitted-code counterpart to {@link IBaboonJsonService} for the
 * service-context modes (`abstract` / `type`): the service context
 * {@code Ctx} is supplied per {@code invoke} (alongside the codec
 * context) instead of being baked into the wrapper at construction time.
 * The context-free {@link IBaboonJsonService} is left untouched so
 * {@code --service-context-mode none} output (and the service-acceptance
 * matrix) stays byte-identical.
 */
public interface IBaboonJsonServiceCtx<Ctx, R> {
    String serviceName();
    R invoke(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception;
}

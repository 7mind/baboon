package baboon.runtime.shared;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Context-carrying cross-domain composable UEBA dispatch.
 *
 * Holds a set of {@link IBaboonUebaServiceCtx} instances from any
 * model(s) and routes an {@code (method, data, ctx, codecCtx)} call to
 * the right one by {@code method.serviceName()}. The service context
 * {@code Ctx} is threaded per dispatch alongside the codec context. See
 * {@link UebaMuxer} for the broader contract notes; the context-free
 * muxer is left untouched.
 */
public final class UebaMuxerCtx<Ctx, R> {
    private final Map<String, IBaboonUebaServiceCtx<Ctx, R>> table = new LinkedHashMap<>();

    @SafeVarargs
    public UebaMuxerCtx(IBaboonUebaServiceCtx<Ctx, R>... services) {
        for (IBaboonUebaServiceCtx<Ctx, R> s : services) {
            register(s);
        }
    }

    public void register(IBaboonUebaServiceCtx<Ctx, R> service) {
        if (table.containsKey(service.serviceName())) {
            throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.serviceName()));
        }
        table.put(service.serviceName(), service);
    }

    public R invoke(BaboonMethodId method, byte[] data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception {
        IBaboonUebaServiceCtx<Ctx, R> service = table.get(method.serviceName());
        if (service == null) {
            throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
        }
        return service.invoke(method, data, ctx, codecCtx);
    }

    public Collection<String> serviceNames() {
        return Collections.unmodifiableSet(table.keySet());
    }
}

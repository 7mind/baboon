package baboon.runtime.shared;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Context-carrying cross-domain composable JSON dispatch.
 *
 * Holds a set of {@link IBaboonJsonServiceCtx} instances from any
 * model(s) and routes an {@code (method, data, ctx, codecCtx)} call to
 * the right one by {@code method.serviceName()}. The service context
 * {@code Ctx} is threaded per dispatch alongside the codec context. See
 * {@link JsonMuxer} for the broader contract notes; the context-free
 * muxer is left untouched.
 */
public final class JsonMuxerCtx<Ctx, R> {
    private final Map<String, IBaboonJsonServiceCtx<Ctx, R>> table = new LinkedHashMap<>();

    @SafeVarargs
    public JsonMuxerCtx(IBaboonJsonServiceCtx<Ctx, R>... services) {
        for (IBaboonJsonServiceCtx<Ctx, R> s : services) {
            register(s);
        }
    }

    public void register(IBaboonJsonServiceCtx<Ctx, R> service) {
        if (table.containsKey(service.serviceName())) {
            throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.serviceName()));
        }
        table.put(service.serviceName(), service);
    }

    public R invoke(BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx) throws Exception {
        IBaboonJsonServiceCtx<Ctx, R> service = table.get(method.serviceName());
        if (service == null) {
            throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
        }
        return service.invoke(method, data, ctx, codecCtx);
    }

    public Collection<String> serviceNames() {
        return Collections.unmodifiableSet(table.keySet());
    }
}

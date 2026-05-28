package baboon.runtime.shared;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Cross-domain composable UEBA dispatch.
 *
 * Holds a set of {@link IBaboonUebaService} instances from any model(s)
 * and routes an {@code (method, data, ctx)} call to the right one by
 * {@code method.serviceName()}. See {@link JsonMuxer} for the broader
 * contract notes.
 */
public final class UebaMuxer<R> {
    private final Map<String, IBaboonUebaService<R>> table = new LinkedHashMap<>();

    @SafeVarargs
    public UebaMuxer(IBaboonUebaService<R>... services) {
        for (IBaboonUebaService<R> s : services) {
            register(s);
        }
    }

    public void register(IBaboonUebaService<R> service) {
        if (table.containsKey(service.serviceName())) {
            throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.serviceName()));
        }
        table.put(service.serviceName(), service);
    }

    public R invoke(BaboonMethodId method, byte[] data, BaboonCodecContext ctx) throws Exception {
        IBaboonUebaService<R> service = table.get(method.serviceName());
        if (service == null) {
            throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
        }
        return service.invoke(method, data, ctx);
    }

    public Collection<String> serviceNames() {
        return Collections.unmodifiableSet(table.keySet());
    }
}

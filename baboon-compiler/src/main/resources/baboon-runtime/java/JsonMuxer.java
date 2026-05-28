package baboon.runtime.shared;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Cross-domain composable JSON dispatch.
 *
 * Holds a set of {@link IBaboonJsonService} instances from any model(s)
 * and routes an {@code (method, data, ctx)} call to the right one by
 * {@code method.serviceName()}. The {@code R} type parameter mirrors
 * the interface's {@code R} so a single class supports both noErrors
 * mode (R = String) and errors mode (R = container-of-String).
 *
 * Strict fail-fast: duplicate {@code serviceName} on registration
 * raises {@code DuplicateService}; unknown {@code serviceName} on
 * dispatch raises {@code NoMatchingService}.
 */
public final class JsonMuxer<R> {
    private final Map<String, IBaboonJsonService<R>> table = new LinkedHashMap<>();

    @SafeVarargs
    public JsonMuxer(IBaboonJsonService<R>... services) {
        for (IBaboonJsonService<R> s : services) {
            register(s);
        }
    }

    public void register(IBaboonJsonService<R> service) {
        if (table.containsKey(service.serviceName())) {
            throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.serviceName()));
        }
        table.put(service.serviceName(), service);
    }

    public R invoke(BaboonMethodId method, String data, BaboonCodecContext ctx) throws Exception {
        IBaboonJsonService<R> service = table.get(method.serviceName());
        if (service == null) {
            throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
        }
        return service.invoke(method, data, ctx);
    }

    public Collection<String> serviceNames() {
        return Collections.unmodifiableSet(table.keySet());
    }
}

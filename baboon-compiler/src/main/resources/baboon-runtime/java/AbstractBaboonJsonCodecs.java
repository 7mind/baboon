package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import java.util.*;

public abstract class AbstractBaboonJsonCodecs {
    private final Map<String, Lazy<? extends BaboonJsonCodec<?>>> codecs = new LinkedHashMap<>();

    protected void register(String typeId, Lazy<? extends BaboonJsonCodec<?>> codec) {
        codecs.put(typeId, codec);
    }

    @SuppressWarnings("unchecked")
    public <T> BaboonJsonCodec<T> getCodec(String typeId) {
        Lazy<? extends BaboonJsonCodec<?>> codec = codecs.get(typeId);
        if (codec == null) {
            throw new BaboonException("No JSON codec registered for " + typeId);
        }
        return (BaboonJsonCodec<T>) codec.get();
    }
}

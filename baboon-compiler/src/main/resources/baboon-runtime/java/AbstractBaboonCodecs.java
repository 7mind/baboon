package baboon.runtime.shared;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Shared codec-registry base. Holds a single map of `typeId → Lazy<? extends BaboonCodecData>`
 * so the facade's generic `getCodec` can iterate either the JSON or UEBA registry uniformly.
 * Mirrors C# `AbstractBaboonCodecs` (`BaboonCodecs.cs`).
 */
public abstract class AbstractBaboonCodecs {
    private final Map<String, Lazy<? extends BaboonCodecData>> codecs = new LinkedHashMap<>();

    protected void registerData(String typeId, Lazy<? extends BaboonCodecData> codec) {
        codecs.put(typeId, codec);
    }

    public Lazy<? extends BaboonCodecData> find(String typeId) {
        Lazy<? extends BaboonCodecData> v = codecs.get(typeId);
        if (v == null) {
            throw new BaboonException("No codec registered for " + typeId);
        }
        return v;
    }

    public Lazy<? extends BaboonCodecData> tryFind(String typeId) {
        return codecs.get(typeId);
    }
}

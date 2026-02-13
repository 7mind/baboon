package baboon.runtime.shared;

import java.util.*;

public abstract class AbstractBaboonUebaCodecs {
    private final Map<String, Lazy<? extends BaboonBinCodec<?>>> codecs = new LinkedHashMap<>();

    protected void register(String typeId, Lazy<? extends BaboonBinCodec<?>> codec) {
        codecs.put(typeId, codec);
    }

    @SuppressWarnings("unchecked")
    public <T> BaboonBinCodec<T> getCodec(String typeId) {
        Lazy<? extends BaboonBinCodec<?>> codec = codecs.get(typeId);
        if (codec == null) {
            throw new BaboonException("No UEBA codec registered for " + typeId);
        }
        return (BaboonBinCodec<T>) codec.get();
    }
}

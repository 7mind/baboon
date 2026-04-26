package baboon.runtime.shared;

public abstract class AbstractBaboonJsonCodecs extends AbstractBaboonCodecs {
    protected void register(String typeId, Lazy<? extends BaboonJsonCodec<?>> codec) {
        registerData(typeId, codec);
    }

    @SuppressWarnings("unchecked")
    public <T> BaboonJsonCodec<T> getCodec(String typeId) {
        Lazy<? extends BaboonCodecData> codec = tryFind(typeId);
        if (codec == null) {
            throw new BaboonException("No JSON codec registered for " + typeId);
        }
        return (BaboonJsonCodec<T>) codec.get();
    }
}

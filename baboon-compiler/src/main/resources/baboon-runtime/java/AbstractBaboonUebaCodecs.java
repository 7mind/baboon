package baboon.runtime.shared;

public abstract class AbstractBaboonUebaCodecs extends AbstractBaboonCodecs {
    protected void register(String typeId, Lazy<? extends BaboonBinCodec<?>> codec) {
        registerData(typeId, codec);
    }

    @SuppressWarnings("unchecked")
    public <T> BaboonBinCodec<T> getCodec(String typeId) {
        Lazy<? extends BaboonCodecData> codec = tryFind(typeId);
        if (codec == null) {
            throw new BaboonException("No UEBA codec registered for " + typeId);
        }
        return (BaboonBinCodec<T>) codec.get();
    }
}

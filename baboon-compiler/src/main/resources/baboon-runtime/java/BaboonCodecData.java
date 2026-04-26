package baboon.runtime.shared;

/**
 * Marker interface implemented by all codec types — `BaboonBinCodec<T>` and `BaboonJsonCodec<T>`.
 * Lets the codec registry hold a heterogeneous lazy-codec map keyed by type identifier without
 * losing the ability to dispatch generically (e.g. `BaboonCodecsFacade.getCodec`). Mirrors C#
 * `IBaboonCodecData`.
 */
public interface BaboonCodecData {
}

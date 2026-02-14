package baboon.runtime.shared;

public enum BaboonCodecContext {
    Default,
    Indexed,
    Compact;

    public boolean useIndices() {
        return this == Indexed;
    }
}

package baboon.runtime.shared;

/**
 * Codec context: pairs a `useIndices` flag with an optional `BaboonCodecsFacade` reference. The
 * facade is threaded through generated codec calls so the `any`-feature cross-format conversion
 * (UEBA ↔ JSON) can resolve codecs by `(domain, version, typeid)` from an `AnyMeta` envelope.
 *
 * `null` facade for the bare `Compact` / `Indexed` singletons; `withFacade(...)` is the single
 * intended construction path for ctxes that thread a facade. Mirrors PR 3.1 (C#) plumbing —
 * Q6 option (a) in the design plan.
 *
 * Was an enum in earlier Java runtimes. Promoted to a class so a per-instance facade can be
 * carried; the public-static `Default` / `Compact` / `Indexed` singletons preserve call-site
 * compatibility (`BaboonCodecContext.Compact` etc. still resolves the same way).
 */
public final class BaboonCodecContext {
    private final boolean useIndices;
    private final BaboonCodecsFacade facade;

    private BaboonCodecContext(boolean useIndices, BaboonCodecsFacade facade) {
        this.useIndices = useIndices;
        this.facade = facade;
    }

    public boolean useIndices() {
        return useIndices;
    }

    public BaboonCodecsFacade facade() {
        return facade;
    }

    public static final BaboonCodecContext Indexed = new BaboonCodecContext(true, null);
    public static final BaboonCodecContext Compact = new BaboonCodecContext(false, null);
    public static final BaboonCodecContext Default = Compact;

    public static BaboonCodecContext withFacade(boolean useIndices, BaboonCodecsFacade facade) {
        return new BaboonCodecContext(useIndices, facade);
    }
}

package baboon.runtime.shared;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;

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
    /**
     * Which lower bound the WRITER publishes as the UEBA envelope's `domainVersionMinCompat` (the v1
     * binary envelope has a single bound slot; see docs/forward-compat.md, "Envelope integration
     * (UEBA)"). STRICT: the byte-identical bound (`baboonSameInVersions.get(0)`) — the default.
     * TOLERANT: the prefix-read bound for the chosen index mode (`prefix-compact` for compact
     * payloads, `prefix-any-mode` for indexed ones); readers older than the writer then decode the
     * payload with their newest codec, dropping the appended fields they do not know. A reader
     * cannot distinguish such an envelope from a byte-identical one, so re-encoding intermediaries
     * must run at the writer's version or newer.
     */
    public enum ForwardWritePolicy { STRICT, TOLERANT }

    /**
     * Which top-level binary envelope layout the WRITER emits (docs/spec/codec-envelope.md §2.1).
     * V1 (default): single bound slot (`domainVersionMinCompat`), value chosen by {@link ForwardWritePolicy}.
     * V2: JSON-equivalent layout carrying both the byte-identical bound and the prefix-read bound for the
     * payload's index mode; the reader's `ForwardReadPolicy` then applies to binary exactly as it does to
     * JSON. Only readers that know v2 can decode it.
     */
    public enum BaboonEnvelopeVersion { V1, V2 }

    private final boolean useIndices;
    private final ForwardWritePolicy forwardWritePolicy;
    private final BaboonEnvelopeVersion envelopeVersion;
    private final BaboonCodecsFacade facade;
    private final Lazy<ObjectReader> jsonReader = new Lazy<>(() -> new ObjectMapper().reader());

    private BaboonCodecContext(boolean useIndices, ForwardWritePolicy forwardWritePolicy, BaboonEnvelopeVersion envelopeVersion, BaboonCodecsFacade facade) {
        this.useIndices = useIndices;
        this.forwardWritePolicy = forwardWritePolicy;
        this.envelopeVersion = envelopeVersion;
        this.facade = facade;
    }

    public boolean useIndices() {
        return useIndices;
    }

    public ForwardWritePolicy forwardWritePolicy() {
        return forwardWritePolicy;
    }

    public BaboonEnvelopeVersion envelopeVersion() {
        return envelopeVersion;
    }

    public BaboonCodecsFacade facade() {
        return facade;
    }

    public JsonNode parseJson(String value) throws JsonProcessingException {
        return jsonReader.get().readTree(value);
    }

    public static final BaboonCodecContext Indexed = new BaboonCodecContext(true, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V1, null);
    public static final BaboonCodecContext Compact = new BaboonCodecContext(false, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V1, null);
    public static final BaboonCodecContext Default = Compact;

    public static BaboonCodecContext withFacade(boolean useIndices, BaboonCodecsFacade facade) {
        return new BaboonCodecContext(useIndices, ForwardWritePolicy.STRICT, BaboonEnvelopeVersion.V1, facade);
    }

    /** Fully specified context: index mode, writer-side forward policy, envelope layout and optional facade. */
    public static BaboonCodecContext custom(boolean useIndices, ForwardWritePolicy forwardWritePolicy, BaboonEnvelopeVersion envelopeVersion, BaboonCodecsFacade facade) {
        return new BaboonCodecContext(useIndices, forwardWritePolicy, envelopeVersion, facade);
    }
}

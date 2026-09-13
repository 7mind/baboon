package baboon.runtime.shared;

import java.util.List;
import java.util.Map;

public interface BaboonMeta {
    List<String> sameInVersions(String typeId);

    /**
     * Forward-readability per type: newer domain versions whose encoded data the keyed version's
     * codec can decode, mapped to the guarantee tier
     * ("identical" | "prefix-any-mode" | "prefix-compact" | "json-additive").
     * The prefix-* tiers hold only for top-level framed UEBA reads where the caller discards
     * the cursor after decoding.
     */
    Map<String, String> forwardReadableVersions(String typeId);
}

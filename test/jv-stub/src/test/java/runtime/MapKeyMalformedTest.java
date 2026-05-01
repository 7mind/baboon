// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonCodecException.DecoderFailure with message containing
// "malformed key". Replaces the prior unchecked cast that produced ClassCastException.
//
// Uses the my.ok.m19.singleid fixture; generated symbols are produced by
// mdl :test-gen-regular-adt under target/test-regular/jv-stub/.
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import my.ok.m19.singleid.Holder_JsonCodec;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class MapKeyMalformedTest {

    private final BaboonCodecContext ctx = BaboonCodecContext.Compact;
    private final ObjectMapper om = new ObjectMapper();

    @Test
    void holderJsonDecodeThrowsDecoderFailureForMalformedMapKey() throws Exception {
        var badJson = "{\"m\":{\"not_a_valid_id\":\"v\"}}";
        JsonNode node = om.readTree(badJson);
        var ex = assertThrows(BaboonCodecException.DecoderFailure.class, () ->
            Holder_JsonCodec.INSTANCE.decode(ctx, node)
        );
        assertTrue(ex.getMessage().contains("malformed key"),
            "unexpected message: " + ex.getMessage());
    }
}

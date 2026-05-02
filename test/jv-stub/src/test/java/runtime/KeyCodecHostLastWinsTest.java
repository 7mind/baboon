// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
// impl (last-wins). Java already used a `volatile static` mutable singleton
// pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/jv-stub/.
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import my.ok.m19.foreign.FStr_KeyCodec;
import my.ok.m19.foreign.FStr_KeyCodecHost;
import my.ok.m19.foreign.Holder;
import my.ok.m19.foreign.Holder_JsonCodec;
import my.ok.m19.foreign.ItemKey;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class KeyCodecHostLastWinsTest {

    private final BaboonCodecContext ctx = BaboonCodecContext.Compact;
    private final ObjectMapper mapper = new ObjectMapper();

    private static final class PrefixCodec implements FStr_KeyCodec {
        private final String tag;
        PrefixCodec(String tag) { this.tag = tag; }
        @Override public String encodeKey(String value) { return tag + ":" + value; }
        @Override public String decodeKey(String s) {
            String pfx = tag + ":";
            return s.startsWith(pfx) ? s.substring(pfx.length()) : s;
        }
    }

    @Test
    public void registerBAfterRegisterAObservesB() throws Exception {
        var m = new LinkedHashMap<ItemKey, String>();
        m.put(new ItemKey("k"), "v");
        var original = new Holder(m);

        FStr_KeyCodecHost.register(new PrefixCodec("A"));
        var encodedA = mapper.writeValueAsString(Holder_JsonCodec.INSTANCE.encode(ctx, original));
        assertTrue(encodedA.contains("A:k"),
                "expected A: prefix in encoded wire form, got " + encodedA);

        FStr_KeyCodecHost.register(new PrefixCodec("B"));
        var encodedB = mapper.writeValueAsString(Holder_JsonCodec.INSTANCE.encode(ctx, original));
        assertTrue(encodedB.contains("B:k"),
                "PR-26.2 last-wins regression: expected B: prefix after re-register, got " + encodedB);
        assertFalse(encodedB.contains("A:k"),
                "PR-26.2 last-wins regression: A: prefix still present after B re-register, got " + encodedB);

        // Restore identity-encoding default for any subsequent tests in this JVM.
        FStr_KeyCodecHost.register(new FStr_KeyCodec() {
            @Override public String encodeKey(String value) { return value; }
            @Override public String decodeKey(String s) { return s; }
        });
    }
}

// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import identifier.ok.LongId_JsonCodec;
import identifier.ok.UInts_JsonCodec;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class LegacyInt64WireTest {

    private final BaboonCodecContext ctx = BaboonCodecContext.Compact;
    private final ObjectMapper om = new ObjectMapper();

    @Test
    void i64DecodesFromTheLegacyNumericForm() throws Exception {
        var decoded = LongId_JsonCodec.INSTANCE.decode(ctx, om.readTree("{\"x\":-9007199254740991}"));
        assertEquals(-9007199254740991L, decoded.x());
    }

    @Test
    void i64DecodesFromTheStringForm() throws Exception {
        var decoded = LongId_JsonCodec.INSTANCE.decode(ctx, om.readTree("{\"x\":\"-9223372036854775808\"}"));
        assertEquals(Long.MIN_VALUE, decoded.x());
    }

    @Test
    void u64DecodesFromTheLegacyNumericForm() throws Exception {
        var decoded = UInts_JsonCodec.INSTANCE.decode(ctx, om.readTree("{\"a\":1,\"b\":2,\"c\":3,\"d\":42}"));
        assertEquals(42L, decoded.d());
    }

    @Test
    void u64DecodesFromTheStringForm() throws Exception {
        var decoded = UInts_JsonCodec.INSTANCE.decode(ctx, om.readTree("{\"a\":1,\"b\":2,\"c\":3,\"d\":\"42\"}"));
        assertEquals(42L, decoded.d());
    }
}

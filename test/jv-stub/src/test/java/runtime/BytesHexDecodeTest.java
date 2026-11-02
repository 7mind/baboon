// Unified `bytes` JSON hex grammar (https://github.com/7mind/baboon/issues/102, /103):
// ASCII space, TAB, LF, VT, FF, CR, ':' and '-' are ignored; the remainder must be an even
// number of ASCII hex digits. Anything else throws IllegalArgumentException, never bytes.
//
// Generated JSON codecs decode `bytes` values and map keys through ByteString.fromHex.
package runtime;

import baboon.runtime.shared.ByteString;
import org.junit.jupiter.api.Test;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class BytesHexDecodeTest {

    private static final byte[] ABCD = {(byte) 0xAB, (byte) 0xCD};

    private static Map<String, byte[]> valid() {
        Map<String, byte[]> m = new LinkedHashMap<>();
        m.put("", new byte[0]);
        m.put("ABCD", ABCD);
        m.put("abcd", ABCD);
        m.put("aBcD", ABCD);
        m.put("AB CD", ABCD);
        m.put("AB:CD", ABCD);
        m.put("AB-CD", ABCD);
        m.put("A B C D", ABCD);
        m.put("\tAB\r\nCD ", ABCD);
        m.put("A:B-C D", ABCD);
        m.put("\u000BAB\fCD", ABCD);
        m.put("00FF7f", new byte[]{0x00, (byte) 0xFF, 0x7F});
        m.put(" - : ", new byte[0]);
        return m;
    }

    private static final List<String> INVALID = List.of(
        "ZZ", "AQ", "+1", "0x01", "q80=", "ABC", "A", "-1",
        "٣٣", "ＡＡ", "aéb", "AB CD"
    );

    @Test
    void validHexSpellingsDecode() {
        valid().forEach((input, expected) ->
            assertArrayEquals(expected, ByteString.fromHex(input).toByteArray(), input));
    }

    @Test
    void malformedHexIsRejected() {
        for (String input : INVALID) {
            assertThrows(IllegalArgumentException.class, () -> ByteString.fromHex(input), input);
        }
    }
}

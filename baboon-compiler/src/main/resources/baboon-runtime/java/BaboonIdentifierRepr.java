package baboon.runtime.shared;

import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.regex.Pattern;

/**
 * Runtime helpers for the `id` toString / parseRepr machinery defined in
 * docs/spec/identifier-repr.md. The Java backend (PR-57a) uses these helpers
 * from emitted code; conformance to the spec is the contract.
 */
public final class BaboonIdentifierRepr {

    private BaboonIdentifierRepr() {}

    // Defensive: numeric Char constant rather than a quoted-character literal.
    private static final char BS  = (char) 92; // ASCII backslash
    private static final char HSH = '#';
    private static final char COL = ':';
    private static final char OBR = '{';
    private static final char CBR = '}';

    // Identifier repr mandates 24-char tsu (always UTC `Z`) and 29-char tso
    // (always `±HH:MM` offset, never `Z` shorthand). These dedicated formats
    // enforce the widths the schema-directed parser depends on (spec §3 / §5.4).
    private static final DateTimeFormatter TSU_FORMAT =
        DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneOffset.UTC);
    private static final DateTimeFormatter TSO_FORMAT =
        DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSxxx");

    private static final Pattern LOWER_HEX = Pattern.compile("^[0-9a-f]*$");

    /** Backslash-escape the 5 metacharacters per spec §4.2. */
    public static String escapeStr(String s) {
        StringBuilder sb = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == BS || c == HSH || c == COL || c == OBR || c == CBR) {
                sb.append(BS);
                sb.append(c);
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /** Lowercase hex, no separators, per spec §3 / §4.4. */
    public static String bytesToHex(ByteString bs) {
        byte[] arr = bs.underlyingUnsafe();
        StringBuilder sb = new StringBuilder(arr.length * 2);
        for (int i = 0; i < arr.length; i++) {
            int b = arr[i] & 0xFF;
            sb.append(toLowerHexDigit((b >>> 4) & 0xF));
            sb.append(toLowerHexDigit(b & 0xF));
        }
        return sb.toString();
    }

    private static char toLowerHexDigit(int v) {
        return (char) (v < 10 ? ('0' + v) : ('a' + (v - 10)));
    }

    /** Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
     * exactly 24 characters. Normalises the input to UTC. */
    public static String tsuToString(OffsetDateTime dt) {
        OffsetDateTime utc = dt.withOffsetSameInstant(ZoneOffset.UTC);
        return utc.format(TSU_FORMAT);
    }

    /** Render a `tsu` from an Instant. */
    public static String tsuToString(Instant instant) {
        return TSU_FORMAT.format(instant);
    }

    /** Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
     * milliseconds, exactly 29 characters. Never emits `Z` shorthand. */
    public static String tsoToString(OffsetDateTime dt) {
        return dt.format(TSO_FORMAT);
    }

    /** Render an unsigned-i64 (carried as signed long) as decimal. */
    public static String u64ToString(long v) {
        return Long.toUnsignedString(v);
    }

    /** Render a `bit` per spec §3 — exact lowercase ASCII. */
    public static String bitToString(boolean b) {
        return b ? "true" : "false";
    }

    public static BaboonEither<String, OffsetDateTime> parseTsuRepr(String s) {
        if (s.length() != 24) {
            return BaboonEither.left("tsu repr must be 24 chars, got " + s.length());
        }
        if (s.charAt(23) != 'Z') {
            return BaboonEither.left("tsu repr must end with 'Z', got: " + s);
        }
        try {
            // Reuse the pre-built TSU_FORMAT (see field decl) — its literal 'Z'
            // pattern anchors the parsed offset-time at UTC.
            OffsetDateTime parsed = OffsetDateTime.parse(s, TSU_FORMAT);
            return BaboonEither.right(parsed.withOffsetSameInstant(ZoneOffset.UTC));
        } catch (DateTimeParseException e) {
            return BaboonEither.left("could not parse tsu: " + s);
        }
    }

    public static BaboonEither<String, OffsetDateTime> parseTsoRepr(String s) {
        if (s.length() != 29) {
            return BaboonEither.left("tso repr must be 29 chars, got " + s.length());
        }
        try {
            OffsetDateTime parsed = OffsetDateTime.parse(s, TSO_FORMAT);
            return BaboonEither.right(parsed);
        } catch (DateTimeParseException e) {
            return BaboonEither.left("could not parse tso: " + s);
        }
    }

    /** Decode `bytes` from lowercase hex. Empty string is legal (empty bytes). */
    public static BaboonEither<String, ByteString> parseBytesHex(String s) {
        if (s.isEmpty()) {
            return BaboonEither.right(ByteString.of(new byte[0]));
        }
        if ((s.length() & 1) != 0) {
            return BaboonEither.left("odd-length hex sequence: " + s);
        }
        // Spec mandates lowercase; reject uppercase to keep exactly one canonical form.
        if (!LOWER_HEX.matcher(s).matches()) {
            return BaboonEither.left("non-lowercase or non-hex character in: " + s);
        }
        byte[] out = new byte[s.length() / 2];
        for (int i = 0; i < s.length(); i += 2) {
            int hi = hexDigit(s.charAt(i));
            int lo = hexDigit(s.charAt(i + 1));
            out[i / 2] = (byte) ((hi << 4) | lo);
        }
        return BaboonEither.right(ByteString.of(out));
    }

    private static int hexDigit(char c) {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
        return -1;
    }

    public static BaboonEither<String, Boolean> parseBit(String s) {
        if ("true".equals(s))  return BaboonEither.right(Boolean.TRUE);
        if ("false".equals(s)) return BaboonEither.right(Boolean.FALSE);
        return BaboonEither.left("expected 'true' or 'false' but found '" + s + "'");
    }

    /** Cursor-based parser for parseRepr decoders. Schema-directed; the
     * caller (the emitted &lt;TypeName&gt;Codec.parseRepr) drives the field
     * sequence per declared type and order. */
    public static final class Cursor {
        private final String source;
        private int pos;

        public Cursor(String source) {
            this.source = source;
            this.pos    = 0;
        }

        public int position()   { return pos; }
        public boolean atEnd()  { return pos >= source.length(); }
        public char peek()      { return source.charAt(pos); }

        public BaboonEither<String, Void> expect(char c) {
            if (atEnd()) return BaboonEither.left("expected '" + c + "' at " + pos + " but reached end of input");
            if (peek() != c) return BaboonEither.left("expected '" + c + "' at " + pos + " but found '" + peek() + "'");
            pos += 1;
            return BaboonEither.right(null);
        }

        public BaboonEither<String, Void> expectLiteral(String lit) {
            if (pos + lit.length() > source.length()) {
                return BaboonEither.left("expected literal '" + lit + "' at " + pos + " but reached end of input");
            }
            for (int i = 0; i < lit.length(); i++) {
                if (source.charAt(pos + i) != lit.charAt(i)) {
                    return BaboonEither.left("expected literal '" + lit + "' at " + pos);
                }
            }
            pos += lit.length();
            return BaboonEither.right(null);
        }

        /** Read until the next bare metachar in `:#{}`. Backslash escapes are
         * NOT processed here — see readStrField for that. Used for primitive
         * consumption (numbers, uuids, hex bytes). */
        public String readUntilStructural() {
            int start = pos;
            while (pos < source.length()) {
                char c = source.charAt(pos);
                if (c == COL || c == HSH || c == OBR || c == CBR) {
                    return source.substring(start, pos);
                }
                pos += 1;
            }
            return source.substring(start, pos);
        }

        /** Consume exactly n characters as a fixed-width lexeme. Used for
         * tsu/tso (which contain `:` characters inside the lexeme). */
        public BaboonEither<String, String> readFixed(int n) {
            if (pos + n > source.length()) {
                return BaboonEither.left("expected " + n + " chars at " + pos + " but only " + (source.length() - pos) + " remain");
            }
            String s = source.substring(pos, pos + n);
            pos += n;
            return BaboonEither.right(s);
        }

        /** Read a `str` field value with backslash-unescaping per spec §5.5. */
        public BaboonEither<String, String> readStrField() {
            StringBuilder sb = new StringBuilder();
            while (pos < source.length()) {
                char c = source.charAt(pos);
                if (c == COL || c == HSH || c == OBR || c == CBR) {
                    return BaboonEither.right(sb.toString());
                }
                if (c == BS) {
                    if (pos + 1 >= source.length()) {
                        return BaboonEither.left("trailing backslash at " + pos);
                    }
                    char nxt = source.charAt(pos + 1);
                    if (nxt == BS || nxt == HSH || nxt == COL || nxt == OBR || nxt == CBR) {
                        sb.append(nxt);
                        pos += 2;
                    } else {
                        return BaboonEither.left("invalid escape at " + pos);
                    }
                } else {
                    sb.append(c);
                    pos += 1;
                }
            }
            return BaboonEither.right(sb.toString());
        }
    }

    /** Validate header of an identifier-repr: &lt;simpleName&gt;:&lt;version&gt;#. */
    public static BaboonEither<String, Void> parseHeader(Cursor cursor, String expectedSimpleName, String expectedVersion) {
        String nameLit = cursor.readUntilStructural();
        if (!nameLit.equals(expectedSimpleName)) {
            return BaboonEither.left("expected name '" + expectedSimpleName + "' but found '" + nameLit + "'");
        }
        BaboonEither<String, Void> afterName = cursor.expect(':');
        if (afterName instanceof BaboonEither.Left<String, Void> ln) {
            return BaboonEither.left(ln.value());
        }
        String verLit = cursor.readUntilStructural();
        if (!verLit.equals(expectedVersion)) {
            return BaboonEither.left("expected version '" + expectedVersion + "' but found '" + verLit + "'");
        }
        return cursor.expect('#');
    }

    /** Validate field-name segment: &lt;expectedFieldName&gt;:. */
    public static BaboonEither<String, Void> parseFieldName(Cursor cursor, String expectedFieldName) {
        String name = cursor.readUntilStructural();
        if (!name.equals(expectedFieldName)) {
            return BaboonEither.left("expected field name '" + expectedFieldName + "' but found '" + name + "'");
        }
        return cursor.expect(':');
    }
}

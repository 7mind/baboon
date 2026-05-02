package baboon.runtime.shared;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;

public final class BaboonTimeFormats {
    private BaboonTimeFormats() {}

    // tsu: UTC, render trailing 'Z' for offset 0 (XXX); tsu owns Z semantics.
    public static final DateTimeFormatter tsuFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
    // tso: explicit ±HH:MM always (UTC = "+00:00", NOT "Z"). PR-28.3 cross-backend
    // canonicalisation — appendOffset's no-offset literal forces zero to "+00:00".
    public static final DateTimeFormatter tsoFormat = new DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")
        .appendOffset("+HH:MM", "+00:00")
        .toFormatter();

    private static final DateTimeFormatter flexibleParseFormat = new DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd'T'HH:mm:ss")
        .optionalStart()
        .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
        .optionalEnd()
        .appendPattern("XXX")
        .toFormatter();

    public static OffsetDateTime parseTso(String s) { return OffsetDateTime.parse(s, flexibleParseFormat); }
    public static OffsetDateTime parseTsu(String s) { return OffsetDateTime.parse(s, flexibleParseFormat); }

    public static String formatTsu(OffsetDateTime s) { return s.format(tsuFormat); }
    public static String formatTso(OffsetDateTime s) { return s.format(tsoFormat); }
}

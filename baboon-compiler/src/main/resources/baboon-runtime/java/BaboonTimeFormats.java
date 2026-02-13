package baboon.runtime.shared;

import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;

public final class BaboonTimeFormats {
    private BaboonTimeFormats() {}

    public static final DateTimeFormatter tsuFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
    public static final DateTimeFormatter tsoFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");

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

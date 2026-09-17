package runtime

import baboon.runtime.shared.BaboonIdentifierRepr
import baboon.runtime.shared.BaboonOffsetDateTime
import kotlinx.datetime.Instant
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

class CivilDateFormattingTest {
    @Test
    fun calendarBoundariesMatchJvmCalendar() {
        val utc = DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSS'Z'").withZone(ZoneOffset.UTC)
        val offset = DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSSxxx")
        val instants = listOf(
            "0001-01-02T00:00:00Z",
            "1900-03-01T00:00:00Z",
            "1969-12-31T23:59:59Z",
            "1970-01-01T00:00:00Z",
            "2000-02-29T23:59:59.999Z",
            "2024-02-29T00:00:00.001Z",
            "9999-12-30T23:59:59.999Z",
        )
        for (text in instants) {
            val reference = java.time.Instant.parse(text)
            val millis = reference.toEpochMilli()
            assertEquals(utc.format(reference), BaboonIdentifierRepr.tsuToString(Instant.fromEpochMilliseconds(millis)))
            for (seconds in listOf(-12600, 0, 7200)) {
                val expected = offset.format(reference.atOffset(ZoneOffset.ofTotalSeconds(seconds)))
                assertEquals(expected, BaboonOffsetDateTime.fromEpochMilliseconds(millis, seconds).toString())
            }
        }
    }

    @Test
    fun preserveExistingNegativeFractionalMillisecondRoundingDifference() {
        // This characterization is not a correctness endorsement: changing the
        // offset formatter's truncation is a separate wire-visible correction.
        assertEquals("1969-12-31T23:59:59.999Z", BaboonIdentifierRepr.tsuToString(Instant.fromEpochMilliseconds(-1)))
        assertEquals("1970-01-01T00:00:00.999+00:00", BaboonOffsetDateTime.fromEpochMilliseconds(-1, 0).toString())
    }
}

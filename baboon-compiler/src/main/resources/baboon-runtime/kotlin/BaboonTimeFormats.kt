package baboon.runtime.shared

import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

object BaboonTimeFormats {
    val tsuFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    val tsoFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

    private val flexibleParseFormat: DateTimeFormatter = DateTimeFormatterBuilder()
        .appendPattern("yyyy-MM-dd'T'HH:mm:ss")
        .optionalStart()
        .appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true)
        .optionalEnd()
        .appendPattern("XXX")
        .toFormatter()

    fun parseTso(s: String): OffsetDateTime = OffsetDateTime.parse(s, flexibleParseFormat)
    fun parseTsu(s: String): OffsetDateTime = OffsetDateTime.parse(s, flexibleParseFormat)

    fun formatTsu(s: OffsetDateTime): String = s.format(tsuFormat)
    fun formatTso(s: OffsetDateTime): String = s.format(tsoFormat)

    fun decodeTsuFromBin(s: LEDataInputStream): OffsetDateTime = decodeFromBin(s)
    fun decodeTsoFromBin(s: LEDataInputStream): OffsetDateTime = decodeFromBin(s)

    fun decodeFromBin(s: LEDataInputStream): OffsetDateTime {
        val millis = s.readLong()
        val offsetMillis = s.readLong()
        val kind = s.readByte()
        require(kind in 0..2)

        val instant = Instant.ofEpochMilli(millis)
        val offset = ZoneOffset.ofTotalSeconds((offsetMillis / 1000).toInt())

        return OffsetDateTime.ofInstant(instant, offset)
    }

    fun encodeTsuToBin(dt: OffsetDateTime, writer: LEDataOutputStream) {
        encodeToBin(dt, writer, 1)
    }

    fun encodeTsoToBin(dt: OffsetDateTime, writer: LEDataOutputStream) {
        encodeToBin(dt, writer, 0)
    }

    fun encodeToBin(dt: OffsetDateTime, writer: LEDataOutputStream, kind: Byte) {
        val millis = dt.toInstant().toEpochMilli()
        val offsetMillis = dt.offset.totalSeconds * 1000L
        writer.writeLong(millis)
        writer.writeLong(offsetMillis)
        writer.writeByte(kind.toInt())
    }
}

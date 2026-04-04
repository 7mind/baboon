package baboon.runtime.shared

import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset

object BaboonTimeFormats {
    fun parseTsu(s: String): Instant = Instant.parse(s)
    fun parseTso(s: String): BaboonOffsetDateTime = BaboonOffsetDateTime.parse(s)

    fun formatTsu(s: Instant): String = s.toString()
    fun formatTso(s: BaboonOffsetDateTime): String = s.toString()

    fun decodeTsuFromBin(s: BaboonBinaryReader): Instant = BaboonBinTools.readTimestamp(s)
    fun decodeTsoFromBin(s: BaboonBinaryReader): BaboonOffsetDateTime = BaboonBinTools.readTimestampOffset(s)

    fun encodeTsuToBin(dt: Instant, writer: BaboonBinaryWriter) {
        BaboonBinTools.writeTimestamp(writer, dt)
    }

    fun encodeTsoToBin(dt: BaboonOffsetDateTime, writer: BaboonBinaryWriter) {
        BaboonBinTools.writeTimestampOffset(writer, dt)
    }
}

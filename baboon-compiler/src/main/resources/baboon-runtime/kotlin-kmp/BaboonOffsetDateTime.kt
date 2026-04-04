package baboon.runtime.shared

import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset

/**
 * Multiplatform offset date-time wrapper. Represents a point in time with a UTC offset.
 * Compatible with Baboon's tso (timestamp with offset) type.
 */
data class BaboonOffsetDateTime(
    val instant: Instant,
    val offset: UtcOffset,
) : Comparable<BaboonOffsetDateTime> {

    fun toEpochMilliseconds(): Long = instant.toEpochMilliseconds()

    override fun compareTo(other: BaboonOffsetDateTime): Int = instant.compareTo(other.instant)

    override fun toString(): String {
        val totalSeconds = offset.totalSeconds
        val epochMs = instant.toEpochMilliseconds()
        val offsetMs = totalSeconds * 1000L
        val localMs = epochMs + offsetMs

        val localSeconds = localMs / 1000
        val millis = (localMs % 1000).let { if (it < 0) it + 1000 else it }

        // Calculate date components from epoch seconds (adjusted for offset)
        val days = floorDiv(localSeconds, 86400L).toInt()
        val timeOfDay = floorMod(localSeconds, 86400L).toInt()
        val hours = timeOfDay / 3600
        val minutes = (timeOfDay % 3600) / 60
        val seconds = timeOfDay % 60

        // Gregorian calendar from days since epoch (1970-01-01)
        val z = days + 719468
        val era = (if (z >= 0) z else z - 146096) / 146097
        val doe = z - era * 146097
        val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
        val y = yoe + era * 400
        val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
        val mp = (5 * doy + 2) / 153
        val d = doy - (153 * mp + 2) / 5 + 1
        val m = mp + (if (mp < 10) 3 else -9)
        val year = y + (if (m <= 2) 1 else 0)

        val sign = if (totalSeconds >= 0) "+" else "-"
        val absOffset = kotlin.math.abs(totalSeconds)
        val offsetH = absOffset / 3600
        val offsetM = (absOffset % 3600) / 60

        return "${pad(year, 4)}-${pad(m, 2)}-${pad(d, 2)}T${pad(hours, 2)}:${pad(minutes, 2)}:${pad(seconds, 2)}.${pad(millis.toInt(), 3)}$sign${pad(offsetH, 2)}:${pad(offsetM, 2)}"
    }

    companion object {
        private fun pad(v: Int, n: Int): String = v.toString().padStart(n, '0')

        private fun floorDiv(x: Long, y: Long): Long {
            val r = x / y
            return if ((x xor y) < 0 && r * y != x) r - 1 else r
        }

        private fun floorMod(x: Long, y: Long): Long = x - floorDiv(x, y) * y

        fun fromEpochMilliseconds(epochMs: Long, offsetSeconds: Int): BaboonOffsetDateTime {
            return BaboonOffsetDateTime(
                Instant.fromEpochMilliseconds(epochMs),
                UtcOffset(seconds = offsetSeconds),
            )
        }

        fun parse(s: String): BaboonOffsetDateTime {
            // Parse ISO-8601 with offset: 2024-06-15T14:30:45.987+02:00
            val instant = Instant.parse(s)
            val offsetSeconds = parseOffsetSeconds(s)
            return BaboonOffsetDateTime(instant, UtcOffset(seconds = offsetSeconds))
        }

        private fun parseOffsetSeconds(s: String): Int {
            // Find the offset part: +HH:MM or -HH:MM or Z at the end
            val timePartStart = s.indexOf('T')
            if (timePartStart < 0) return 0
            val timePart = s.substring(timePartStart)
            return when {
                timePart.endsWith("Z") -> 0
                else -> {
                    val offsetIdx = maxOf(timePart.lastIndexOf('+'), timePart.lastIndexOf('-'))
                    if (offsetIdx <= 0) 0
                    else {
                        val sign = if (timePart[offsetIdx] == '+') 1 else -1
                        val offsetStr = timePart.substring(offsetIdx + 1)
                        val parts = offsetStr.split(":")
                        val h = parts[0].toIntOrNull() ?: 0
                        val m = parts.getOrNull(1)?.toIntOrNull() ?: 0
                        sign * (h * 3600 + m * 60)
                    }
                }
            }
        }
    }
}

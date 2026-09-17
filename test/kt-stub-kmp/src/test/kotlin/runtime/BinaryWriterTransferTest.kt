package runtime

import baboon.runtime.shared.BaboonBinaryWriter
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class BinaryWriterTransferTest {
    @Test
    fun transferAppendsOnlyWrittenBytesAndRetainsIndependentOwnership() {
        val source = BaboonBinaryWriter()
        val destination = BaboonBinaryWriter()
        source.write(byteArrayOf(1, 2, 3))
        destination.writeByte(9)
        source.writeTo(destination)
        assertArrayEquals(byteArrayOf(9, 1, 2, 3), destination.toByteArray())
        assertEquals(3, source.size())
        source.writeByte(4)
        destination.writeByte(5)
        assertArrayEquals(byteArrayOf(1, 2, 3, 4), source.toByteArray())
        assertArrayEquals(byteArrayOf(9, 1, 2, 3, 5), destination.toByteArray())
        val snapshot = destination.toByteArray()
        snapshot[0] = 0
        assertEquals(9.toByte(), destination.toByteArray()[0])
    }

    @Test
    fun transferHandlesEmptyBuffersGrowthAndSelfAppend() {
        val source = BaboonBinaryWriter()
        val destination = BaboonBinaryWriter()
        source.writeTo(destination)
        assertEquals(0, destination.size())
        val payload = ByteArray(1025) { it.toByte() }
        source.write(payload)
        source.writeTo(destination)
        assertArrayEquals(payload, destination.toByteArray())
        destination.writeTo(destination)
        assertArrayEquals(payload + payload, destination.toByteArray())
    }
}

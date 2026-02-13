package baboon.runtime.shared

import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import java.io.InputStream
import java.io.OutputStream

interface BaboonCodecData {
    val baboonDomainVersion: String
    val baboonDomainIdentifier: String
    val baboonTypeIdentifier: String
}

interface BaboonCodecContext {
    val useIndices: Boolean

    companion object {
        val Default: BaboonCodecContext = Compact
    }

    object Indexed : BaboonCodecContext {
        override val useIndices: Boolean = true
    }

    object Compact : BaboonCodecContext {
        override val useIndices: Boolean = false
    }
}

interface BaboonCodec<T> : BaboonCodecData

interface BaboonJsonCodec<T> : BaboonCodec<T> {
    fun encode(ctx: BaboonCodecContext, instance: T): JsonElement
    fun decode(ctx: BaboonCodecContext, wire: JsonElement): T

    fun getField(jsonObject: JsonObject, name: String): JsonElement {
        return jsonObject[name] ?: throw RuntimeException("Cannot decode $jsonObject to $baboonTypeIdentifier: missing field $name")
    }

    interface Base<T> : BaboonJsonCodec<T>
    interface BaseGenerated<T> : BaboonJsonCodec<T>
    interface BaseGeneratedAdt<T> : BaboonJsonCodec<T>
    interface NoEncoder<T> : BaboonJsonCodec<T> {
        override fun encode(ctx: BaboonCodecContext, instance: T): JsonElement =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGenerated<T> : BaboonJsonCodec<T> {
        override fun encode(ctx: BaboonCodecContext, instance: T): JsonElement =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGeneratedAdt<T> : BaboonJsonCodec<T> {
        override fun encode(ctx: BaboonCodecContext, instance: T): JsonElement =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
}

interface BaboonBinCodec<T> : BaboonCodec<T> {
    fun encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T)
    fun decode(ctx: BaboonCodecContext, wire: LEDataInputStream): T

    interface Base<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
    }
    interface BaseGenerated<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
    }
    interface BaseGeneratedAdt<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
    }
    interface NoEncoder<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
        override fun encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGenerated<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
        override fun encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGeneratedAdt<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
        override fun encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
}

data class BaboonIndexEntry(val offset: Long, val length: Long)

interface BaboonBinCodecIndexed {
    fun indexElementsCount(ctx: BaboonCodecContext): Short

    fun readIndex(ctx: BaboonCodecContext, wire: LEDataInputStream): List<BaboonIndexEntry> {
        val header = wire.readByte()
        val isIndexed = (header.toInt() and 0x01) != 0
        val result = mutableListOf<BaboonIndexEntry>()
        var prevOffset = 0L
        var prevLen = 0L
        if (isIndexed) {
            var left = indexElementsCount(ctx).toInt()
            while (left > 0) {
                val offset = wire.readInt()
                val len = wire.readInt()

                require(len > 0) { "Length must be positive" }
                require(offset >= prevOffset + prevLen) { "Offset violation: $offset not >= ${prevOffset + prevLen}" }

                result.add(BaboonIndexEntry(offset.toLong(), len.toLong()))
                left -= 1
                prevOffset = offset.toLong()
                prevLen = len.toLong()
            }
        }
        return result
    }
}

open class AbstractBaboonCodecs {
    private val registry = mutableMapOf<String, Lazy<out BaboonCodecData>>()

    fun register(id: String, codec: Lazy<out BaboonCodecData>) {
        registry[id] = codec
    }

    fun find(id: String): Lazy<out BaboonCodecData> {
        return registry[id] ?: throw NoSuchElementException("Codec not found: $id")
    }

    fun tryFind(id: String): Lazy<out BaboonCodecData>? {
        return registry[id]
    }
}

open class AbstractBaboonJsonCodecs : AbstractBaboonCodecs()
open class AbstractBaboonUebaCodecs : AbstractBaboonCodecs()

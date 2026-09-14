package baboon.runtime.shared

// @baboon:json-start
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
// @baboon:json-end

interface BaboonCodecData {
    val baboonDomainVersion: String
    val baboonDomainIdentifier: String
    val baboonTypeIdentifier: String
}

/**
 * Which lower bound the WRITER publishes as the UEBA envelope's `domainVersionMinCompat` (the v1
 * binary envelope has a single bound slot; see docs/forward-compat.md, "Envelope integration (UEBA)").
 *   - Strict: the byte-identical bound (`baboonSameInVersions.first()`) — the default.
 *   - Tolerant: the prefix-read bound for the chosen index mode (`prefix-compact` for compact
 *     payloads, `prefix-any-mode` for indexed ones). Readers older than the writer then decode the
 *     payload with their newest codec, dropping the appended fields they do not know. A reader
 *     cannot distinguish such an envelope from a byte-identical one, so re-encoding intermediaries
 *     must run at the writer's version or newer.
 */
enum class ForwardWritePolicy { Strict, Tolerant }

interface BaboonCodecContext {
    val useIndices: Boolean
    val forwardWritePolicy: ForwardWritePolicy get() = ForwardWritePolicy.Strict

    /** Optional facade reference, threaded through generated codec calls so the `any`-feature
     *  cross-format conversion (UEBA ↔ JSON) can resolve codecs by `(domain, version, typeid)`
     *  from an `AnyMeta` envelope. `null` for the bare `Compact` / `Indexed` singletons;
     *  `withFacade(...)` is the single intended construction path for ctxes that thread a facade.
     *  See PR 5.1 plumbing. */
    val facade: BaboonCodecsFacade? get() = null

    companion object {
        val Default: BaboonCodecContext = Compact

        fun withFacade(useIndices: Boolean, baboonFacade: BaboonCodecsFacade): BaboonCodecContext =
            object : BaboonCodecContext {
                override val useIndices: Boolean = useIndices
                override val facade: BaboonCodecsFacade = baboonFacade
            }

        /** Fully specified context: index mode, writer-side forward policy and optional facade. */
        fun custom(indices: Boolean, policy: ForwardWritePolicy, baboonFacade: BaboonCodecsFacade?): BaboonCodecContext =
            object : BaboonCodecContext {
                override val useIndices: Boolean = indices
                override val forwardWritePolicy: ForwardWritePolicy = policy
                override val facade: BaboonCodecsFacade? = baboonFacade
            }
    }

    object Indexed : BaboonCodecContext {
        override val useIndices: Boolean = true
    }

    object Compact : BaboonCodecContext {
        override val useIndices: Boolean = false
    }
}

interface BaboonCodec<T> : BaboonCodecData

// @baboon:json-start
interface BaboonJsonCodec<T> : BaboonCodec<T> {
    fun encode(ctx: BaboonCodecContext, instance: T): JsonElement
    fun decode(ctx: BaboonCodecContext, wire: JsonElement): T

    fun getField(jsonObject: JsonObject, name: String): JsonElement {
        return jsonObject[name] ?: throw RuntimeException("Cannot decode $$jsonObject to $$baboonTypeIdentifier: missing field $$name")
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
// @baboon:json-end

interface BaboonBinCodec<T> : BaboonCodec<T> {
    fun encode(ctx: BaboonCodecContext, writer: BaboonBinaryWriter, instance: T)
    fun decode(ctx: BaboonCodecContext, wire: BaboonBinaryReader): T

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
        override fun encode(ctx: BaboonCodecContext, writer: BaboonBinaryWriter, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGenerated<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
        override fun encode(ctx: BaboonCodecContext, writer: BaboonBinaryWriter, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
    interface NoEncoderGeneratedAdt<T, C : BaboonBinCodec<T>> : BaboonBinCodec<T> {
        val LazyInstance: Lazy<C>
        val instance: C get() = LazyInstance.value
        override fun encode(ctx: BaboonCodecContext, writer: BaboonBinaryWriter, instance: T) =
            throw UnsupportedOperationException("Encoder not available for deprecated version")
    }
}

data class BaboonIndexEntry(val offset: Long, val length: Long)

interface BaboonBinCodecIndexed {
    fun indexElementsCount(ctx: BaboonCodecContext): Short

    fun readIndex(ctx: BaboonCodecContext, wire: BaboonBinaryReader): List<BaboonIndexEntry> {
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
                require(offset >= prevOffset + prevLen) { "Offset violation: $$offset not >= $${prevOffset + prevLen}" }

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
        return registry[id] ?: throw NoSuchElementException("Codec not found: $$id")
    }

    fun tryFind(id: String): Lazy<out BaboonCodecData>? {
        return registry[id]
    }
}

// @baboon:json-start
open class AbstractBaboonJsonCodecs : AbstractBaboonCodecs()
// @baboon:json-end
open class AbstractBaboonUebaCodecs : AbstractBaboonCodecs()

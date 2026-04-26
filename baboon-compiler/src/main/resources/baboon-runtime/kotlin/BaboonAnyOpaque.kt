package baboon.runtime.shared

// @baboon:json-start
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.contentOrNull
import kotlinx.serialization.json.intOrNull
// @baboon:json-end
import java.io.FilterInputStream
import java.io.InputStream

/**
 * `AnyMeta` carries the locked four-byte/six-kind meta envelope for `any`-typed payloads.
 *
 * Invariants enforced at construction:
 *   - bit 2 (DOMAIN_BIT, 0x04) set ⇔ `domain != null`
 *   - bit 1 (VERSION_BIT, 0x02) set ⇔ `version != null`
 *   - bit 0 (TYPEID_BIT, 0x01) set ⇔ `typeid != null`
 *   - `kind ∈ {0x00, 0x01, 0x02, 0x03, 0x06, 0x07}` — `0x04`/`0x05` are reserved (PR-04-D01)
 *
 * Construction with a reserved or mismatched kind throws `IllegalArgumentException`.
 */
data class AnyMeta(
    val kind: Byte,
    val domain: String?,
    val version: String?,
    val typeid: String?,
) {
    init {
        val domainBitSet = (kind.toInt() and AnyMetaCodec.DOMAIN_BIT.toInt()) != 0
        require(domainBitSet == (domain != null)) {
            "AnyMeta: domain presence (${domain != null}) does not match kind 0x${(kind.toInt() and 0xFF).toString(16)} bit 2"
        }
        val versionBitSet = (kind.toInt() and AnyMetaCodec.VERSION_BIT.toInt()) != 0
        require(versionBitSet == (version != null)) {
            "AnyMeta: version presence (${version != null}) does not match kind 0x${(kind.toInt() and 0xFF).toString(16)} bit 1"
        }
        val typeidBitSet = (kind.toInt() and AnyMetaCodec.TYPEID_BIT.toInt()) != 0
        require(typeidBitSet == (typeid != null)) {
            "AnyMeta: typeid presence (${typeid != null}) does not match kind 0x${(kind.toInt() and 0xFF).toString(16)} bit 0"
        }
        require(AnyMetaCodec.VALID_KINDS.contains(kind)) {
            "AnyMeta: reserved or invalid meta-kind byte: 0x${(kind.toInt() and 0xFF).toString(16).padStart(2, '0')}"
        }
    }
}

/**
 * `AnyOpaque` is the language-surface ADT for `any`-typed fields. Instances carry a meta envelope
 * plus a payload in either binary (UEBA) or JSON form. See spec §"Wire format" for the locked
 * meta-kind table.
 */
sealed class AnyOpaque {
    abstract val meta: AnyMeta
}

/**
 * UEBA-formed payload. Not a `data class`: Kotlin auto-generated `equals`/`hashCode` on a
 * `data class` use reference identity for `ByteArray` fields (PR-05-D08 lesson — same JVM gotcha
 * as Scala). We override to content-wise comparison so generated round-trip tests work.
 */
class AnyOpaqueUeba(
    override val meta: AnyMeta,
    val bytes: ByteArray,
) : AnyOpaque() {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is AnyOpaqueUeba) return false
        return meta == other.meta && bytes.contentEquals(other.bytes)
    }

    override fun hashCode(): Int = 31 * meta.hashCode() + bytes.contentHashCode()

    override fun toString(): String = "AnyOpaqueUeba(meta=$meta, bytes=ByteArray(${bytes.size}))"
}

// @baboon:json-start
/**
 * JSON-formed payload. `kotlinx.serialization.json.JsonElement` subtypes (`JsonObject`,
 * `JsonArray`, `JsonPrimitive`, `JsonNull`) are content-wise data classes/objects, so the
 * `data class` auto-generated `equals` is structural — no manual override needed.
 */
data class AnyOpaqueJson(
    override val meta: AnyMeta,
    val json: JsonElement,
) : AnyOpaque()
// @baboon:json-end

object AnyMetaCodec {
    const val DOMAIN_BIT: Byte = 0x04
    const val VERSION_BIT: Byte = 0x02
    const val TYPEID_BIT: Byte = 0x01

    const val ANY_KIND_KEY = "${'$'}ak"
    const val ANY_DOMAIN_KEY = "${'$'}ad"
    const val ANY_VERSION_KEY = "${'$'}av"
    const val ANY_TYPEID_KEY = "${'$'}at"

    val VALID_KINDS: Set<Byte> = setOf<Byte>(0x00, 0x01, 0x02, 0x03, 0x06, 0x07)

    fun writeBin(meta: AnyMeta, writer: LEDataOutputStream) {
        writer.writeByte(meta.kind.toInt() and 0xFF)
        meta.domain?.let { BaboonBinTools.writeString(writer, it) }
        meta.version?.let { BaboonBinTools.writeString(writer, it) }
        meta.typeid?.let { BaboonBinTools.writeString(writer, it) }
    }

    fun readBin(reader: LEDataInputStream): AnyMeta {
        val kind = reader.readByte()
        val domain = if ((kind.toInt() and DOMAIN_BIT.toInt()) != 0) BaboonBinTools.readString(reader) else null
        val version = if ((kind.toInt() and VERSION_BIT.toInt()) != 0) BaboonBinTools.readString(reader) else null
        val typeid = if ((kind.toInt() and TYPEID_BIT.toInt()) != 0) BaboonBinTools.readString(reader) else null
        return AnyMeta(kind, domain, version, typeid)
    }

    /**
     * Counting wrapper used to track bytes consumed during a meta read. Mirrors Scala's
     * `CountingInputStream` (PR-05-D01). Named (not anonymous) so we don't need reflective access.
     */
    private class CountingInputStream(stream: InputStream) : FilterInputStream(stream) {
        var count: Int = 0
            private set

        override fun read(): Int {
            val b = `in`.read()
            if (b >= 0) count += 1
            return b
        }

        override fun read(b: ByteArray, off: Int, len: Int): Int {
            val n = `in`.read(b, off, len)
            if (n > 0) count += n
            return n
        }
    }

    /**
     * Reads meta and reports the number of bytes consumed. Callers that know the on-wire
     * `meta-length` window can skip any trailing bytes left in the window — that's how the wire
     * format keeps forward-compat with future meta extensions (PR-05-D01).
     */
    fun readBinWithLength(reader: LEDataInputStream): Pair<AnyMeta, Int> {
        val counting = CountingInputStream(reader)
        val wrapped = LEDataInputStream(counting)
        val meta = readBin(wrapped)
        return Pair(meta, counting.count)
    }

    // @baboon:json-start
    /**
     * Always returns a `JsonObject`. The JSON encoder envelope build relies on this invariant
     * (PR-06-D08) — adding `$c` content into a non-object would silently lose the key.
     */
    fun writeJson(meta: AnyMeta): JsonElement {
        val pairs = mutableMapOf<String, JsonElement>()
        pairs[ANY_KIND_KEY] = JsonPrimitive(meta.kind.toInt() and 0xFF)
        meta.domain?.let { pairs[ANY_DOMAIN_KEY] = JsonPrimitive(it) }
        meta.version?.let { pairs[ANY_VERSION_KEY] = JsonPrimitive(it) }
        meta.typeid?.let { pairs[ANY_TYPEID_KEY] = JsonPrimitive(it) }
        return JsonObject(pairs)
    }

    /**
     * Returns `Either` to mirror PR-04-D02: binary decode trusts the wire and throws on bad input;
     * JSON decode is user-facing and threads errors as `Either`.
     */
    fun readJson(json: JsonElement): Either<BaboonCodecException, AnyMeta> {
        if (json !is JsonObject) {
            return Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: expected JSON object, got ${json::class.simpleName}"
                )
            )
        }

        val kindToken = json[ANY_KIND_KEY]
        if (kindToken !is JsonPrimitive || kindToken.isString) {
            return Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: missing or non-numeric '$ANY_KIND_KEY' field"
                )
            )
        }
        val kindInt = kindToken.intOrNull
            ?: return Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: invalid '$ANY_KIND_KEY' integer value"
                )
            )
        val kind = kindInt.toByte()

        val domainResult = readOptString(json, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain")
        val domain = when (domainResult) {
            is Either.Left -> return Either.Left(domainResult.value)
            is Either.Right -> domainResult.value
        }

        val versionResult = readOptString(json, ANY_VERSION_KEY, kind, VERSION_BIT, "version")
        val version = when (versionResult) {
            is Either.Left -> return Either.Left(versionResult.value)
            is Either.Right -> versionResult.value
        }

        val typeidResult = readOptString(json, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid")
        val typeid = when (typeidResult) {
            is Either.Left -> return Either.Left(typeidResult.value)
            is Either.Right -> typeidResult.value
        }

        return try {
            Either.Right(AnyMeta(kind, domain, version, typeid))
        } catch (e: IllegalArgumentException) {
            Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: invalid meta: ${e.message}"
                )
            )
        }
    }

    private fun readOptString(
        json: JsonObject,
        key: String,
        kind: Byte,
        bit: Byte,
        name: String,
    ): Either<BaboonCodecException, String?> {
        val present = (kind.toInt() and bit.toInt()) != 0
        val token = json[key]
        val value = if (token is JsonPrimitive && token.isString) token.contentOrNull else null

        return when {
            present && value != null -> Either.Right(value)
            !present && value == null -> Either.Right(null)
            present -> Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: kind 0x${(kind.toInt() and 0xFF).toString(16)} requires '$key' ($name) but it is missing"
                )
            )
            else -> Either.Left(
                BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: kind 0x${(kind.toInt() and 0xFF).toString(16)} forbids '$key' ($name) but it is present"
                )
            )
        }
    }
    // @baboon:json-end

}

package baboon.runtime.shared

import java.util.concurrent.atomic.AtomicReference

interface BaboonGenerated {
    val baboonDomainVersion: String
    val baboonDomainIdentifier: String
    val baboonSameInVersions: List<String>

    /**
     * Forward-readability: newer domain versions whose encoded data THIS version's codec can
     * decode, mapped to the guarantee tier
     * ("identical" | "prefix-any-mode" | "prefix-compact" | "json-additive").
     * The prefix-* tiers hold only for top-level framed UEBA reads where the caller discards
     * the cursor after decoding.
     */
    val baboonForwardReadable: Map<String, String>

    /**
     * Writer-side inverse of [baboonForwardReadable]: guarantee tier -> oldest domain version whose
     * codec can decode THIS version's encoding of this type. The "identical" bound equals
     * `baboonSameInVersions[0]`; the "json-additive" bound is published as `$rv`.
     */
    val baboonMinReaderVersions: Map<String, String>
    val baboonTypeIdentifier: String

    fun domainVersion(): BaboonDomainVersion = BaboonDomainVersion(baboonDomainIdentifier, baboonDomainVersion)
}

fun BaboonGenerated.baboonUnmodifiedSinceVersion(): String = baboonSameInVersions[0]

fun BaboonMeta.unmodifiedSinceVersion(typeId: String): String = sameInVersions(typeId)[0]

interface BaboonGeneratedLatest : BaboonGenerated

interface BaboonAdtMemberMeta {
    val baboonAdtTypeIdentifier: String
    val baboonAdtType: Class<*>
}

interface BaboonMeta {
    fun sameInVersions(typeId: String): List<String>

    /** Forward-readability per type: newer version -> guarantee tier (see BaboonGenerated.baboonForwardReadable). */
    fun forwardReadableVersions(typeId: String): Map<String, String>
}

interface BaboonEnum<T> {
    fun parse(s: String): T?
    fun all(): List<T>
}

class Lazy<T>(private val initializer: () -> T) {
    private val valueRef = AtomicReference<T?>(null)

    val value: T
        get() {
            valueRef.get()?.let { return it }
            val computed = initializer()
            if (valueRef.compareAndSet(null, computed)) return computed
            return valueRef.get()!!
        }

    val isValueCreated: Boolean get() = valueRef.get() != null
}

interface BaboonSingleton<T> {
    val LazyInstance: Lazy<T>
    fun instance(): T = LazyInstance.value
}

data class BaboonDomainVersion(val domainIdentifier: String, val domainVersion: String) {
    private val lazyVersion = Lazy { Version.from(domainVersion) }
    val version: Version get() = lazyVersion.value
}

data class Version(val major: Int, val minor: Int, val patch: Int) : Comparable<Version> {
    override fun compareTo(other: Version): Int {
        val cmp1 = major.compareTo(other.major)
        if (cmp1 != 0) return cmp1
        val cmp2 = minor.compareTo(other.minor)
        if (cmp2 != 0) return cmp2
        return patch.compareTo(other.patch)
    }

    companion object {
        fun from(version: String): Version {
            val chunks = version.split(".")
            require(chunks.isNotEmpty()) { "Expected to have version in format x.[y].[z], got $version" }
            val major = chunks[0].trim().toIntOrNull()
                ?: throw Exception("Expected to have version in format x.[y].[z], got $version. Invalid major value.")
            val minor = chunks.getOrNull(1)?.trim()?.toIntOrNull()
                ?: throw Exception("Expected to have version in format x.[y].[z], got $version. Invalid minor value.")
            val patch = chunks.getOrNull(2)?.trim()?.toIntOrNull()
                ?: throw Exception("Expected to have version in format x.[y].[z], got $version. Invalid patch value.")
            return Version(major, minor, patch)
        }
    }
}

data class BaboonTypeMeta(
    val metaVersion: Byte,
    val domainIdentifier: String,
    val domainVersion: String,
    val domainVersionMinCompat: String,
    val typeIdentifier: String,
    /**
     * Oldest domain version whose JSON codec can decode the payload under the json-additive
     * contract (tolerant key lookup; fields unknown to that version are dropped). Always
     * <= domainVersionMinCompat. Published as `$rv` when it differs from the (effective)
     * minCompat; the binary v1 envelope does not carry it. Defaults to minCompat.
     */
    val domainVersionReadableMin: String = domainVersionMinCompat,
) {
    fun version(): BaboonDomainVersion = BaboonDomainVersion(domainIdentifier, domainVersion)
    fun versionMinCompat(): BaboonDomainVersion? {
        return when {
            domainVersionMinCompat.isEmpty() -> null
            domainVersionMinCompat == domainVersion -> null
            else -> BaboonDomainVersion(domainIdentifier, domainVersionMinCompat)
        }
    }
    fun versionReadableMin(): BaboonDomainVersion? {
        return when {
            domainVersionReadableMin.isEmpty() -> versionMinCompat()
            domainVersionReadableMin == domainVersion -> null
            else -> BaboonDomainVersion(domainIdentifier, domainVersionReadableMin)
        }
    }

    fun writeBin(writer: LEDataOutputStream) {
        BaboonTypeMetaCodec.writeBin(this, writer)
    }

    companion object {
        fun from(value: BaboonGenerated): BaboonTypeMeta {
            val typeIdentifier = when {
                value is BaboonAdtMemberMeta && value::class.java.interfaces.any { it == BaboonGenerated::class.java } ->
                    (value as BaboonAdtMemberMeta).baboonAdtTypeIdentifier
                else -> value.baboonTypeIdentifier
            }
            val readableMin = value.baboonMinReaderVersions[JSON_READABLE_TIER]
                ?: error("baboonMinReaderVersions lacks '$JSON_READABLE_TIER' for type ${value.baboonTypeIdentifier}")
            return BaboonTypeMeta(
                BaboonTypeMetaCodec.META_VERSION,
                value.baboonDomainIdentifier,
                value.baboonDomainVersion,
                value.baboonSameInVersions.first(),
                typeIdentifier,
                readableMin,
            )
        }

        /** Tier key of the JSON envelope's readable-min bound in `baboonMinReaderVersions`. */
        const val JSON_READABLE_TIER: String = "json-additive"

        fun readMeta(reader: LEDataInputStream): BaboonTypeMeta? {
            return BaboonTypeMetaCodec.readMeta(reader)
        }
    }
}

object BaboonTypeMetaCodec {
    private const val META_VERSION_1: Byte = 1
    const val META_VERSION: Byte = META_VERSION_1

    fun writeBin(meta: BaboonTypeMeta, writer: LEDataOutputStream) {
        writer.write(META_VERSION_1.toInt())
        BaboonBinTools.writeString(writer, meta.domainIdentifier)
        BaboonBinTools.writeString(writer, meta.domainVersion)
        if (meta.domainVersion == meta.domainVersionMinCompat) {
            writer.write(0)
        } else {
            writer.write(1)
            BaboonBinTools.writeString(writer, meta.domainVersionMinCompat)
        }
        BaboonBinTools.writeString(writer, meta.typeIdentifier)
    }

    fun readMeta(reader: LEDataInputStream): BaboonTypeMeta? {
        val metaVersion = reader.readByte()
        if (metaVersion == META_VERSION_1) {
            return readMetaV1(reader)
        }
        return null
    }

    private fun readMetaV1(reader: LEDataInputStream): BaboonTypeMeta? {
        val domainIdentifier = BaboonBinTools.readString(reader)
        val domainVersion = BaboonBinTools.readString(reader)
        val hasMinCompat = reader.readByte().toInt()
        // codec-envelope.md §2.1: only 0x00 (elided) and 0x01 (present) are legal; anything else is rejected
        if (hasMinCompat != 0 && hasMinCompat != 1) return null
        val domainVersionMinCompat = if (hasMinCompat == 1) BaboonBinTools.readString(reader) else domainVersion
        val typeIdentifier = BaboonBinTools.readString(reader)

        return BaboonTypeMeta(
            META_VERSION_1,
            domainIdentifier,
            domainVersion,
            domainVersionMinCompat,
            typeIdentifier,
        )
    }
}

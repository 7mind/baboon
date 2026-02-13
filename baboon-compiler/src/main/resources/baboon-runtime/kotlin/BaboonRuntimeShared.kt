package baboon.runtime.shared

import java.util.concurrent.atomic.AtomicReference

interface BaboonGenerated {
    val baboonDomainVersion: String
    val baboonDomainIdentifier: String
    val baboonSameInVersions: List<String>
    val baboonTypeIdentifier: String

    fun domainVersion(): BaboonDomainVersion = BaboonDomainVersion(baboonDomainIdentifier, baboonDomainVersion)
}

interface BaboonGeneratedLatest : BaboonGenerated

interface BaboonAdtMemberMeta {
    val baboonAdtTypeIdentifier: String
    val baboonAdtType: Class<*>
}

interface BaboonMeta {
    fun sameInVersions(typeId: String): List<String>
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
) {
    fun version(): BaboonDomainVersion = BaboonDomainVersion(domainIdentifier, domainVersion)
    fun versionMinCompat(): BaboonDomainVersion? {
        return when {
            domainVersionMinCompat.isEmpty() -> null
            domainVersionMinCompat == domainVersion -> null
            else -> BaboonDomainVersion(domainIdentifier, domainVersionMinCompat)
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
            return BaboonTypeMeta(
                BaboonTypeMetaCodec.META_VERSION,
                value.baboonDomainIdentifier,
                value.baboonDomainVersion,
                value.baboonSameInVersions.first(),
                typeIdentifier,
            )
        }

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
        if (metaVersion.toInt() == 1) {
            return readMetaV1(reader)
        }
        return null
    }

    private fun readMetaV1(reader: LEDataInputStream): BaboonTypeMeta {
        val domainIdentifier = BaboonBinTools.readString(reader)
        val domainVersion = BaboonBinTools.readString(reader)
        val domainVersionMinCompat = if (reader.readByte().toInt() == 1) BaboonBinTools.readString(reader) else domainVersion
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

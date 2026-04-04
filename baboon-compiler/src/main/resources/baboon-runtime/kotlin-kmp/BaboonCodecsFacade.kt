package baboon.runtime.shared

// @baboon:json-start
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.jsonObject
// @baboon:json-end

open class BaboonCodecsFacade {
    private val CONTENT_JSON_KEY = "${'$'}c"

    // @baboon:json-start
    private val versionsCodecsJson = HashMap<BaboonDomainVersion, Lazy<AbstractBaboonJsonCodecs>>()
    // @baboon:json-end
    private val versionsCodecsBin = HashMap<BaboonDomainVersion, Lazy<AbstractBaboonUebaCodecs>>()
    private val versionsConversions = HashMap<BaboonDomainVersion, Lazy<AbstractBaboonConversions>>()
    private val versionsMeta = HashMap<BaboonDomainVersion, Lazy<BaboonMeta>>()
    private val domainVersions = HashMap<String, MutableList<BaboonDomainVersion>>()

    fun latest(domain: String): Version {
        val versions = domainVersions[domain]
        if (versions == null || versions.isEmpty()) {
            throw Exception("No registered version for $$domain domain found.")
        }
        return versions.last().version
    }

    fun register(facade: BaboonCodecsFacade) {
        facade.domainVersions.forEach { (id, versions) -> domainVersions[id] = versions }
        // @baboon:json-start
        facade.versionsCodecsJson.forEach { (id, codec) -> versionsCodecsJson[id] = codec }
        // @baboon:json-end
        facade.versionsCodecsBin.forEach { (id, codec) -> versionsCodecsBin[id] = codec }
        facade.versionsConversions.forEach { (id, conversion) -> versionsConversions[id] = conversion }
        facade.versionsMeta.forEach { (id, meta) -> versionsMeta[id] = meta }
    }

    // @baboon:json-start
    fun register(
        domainVersion: BaboonDomainVersion,
        codecsJson: () -> AbstractBaboonJsonCodecs,
        codecsBin: () -> AbstractBaboonUebaCodecs,
        conversions: () -> AbstractBaboonConversions,
        meta: () -> BaboonMeta,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = Lazy(codecsJson)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        versionsConversions[domainVersion] = Lazy(conversions)
        versionsMeta[domainVersion] = Lazy(meta)
        return domainVersion
    }

    fun register(
        domainVersion: BaboonDomainVersion,
        codecsJson: () -> AbstractBaboonJsonCodecs,
        codecsBin: () -> AbstractBaboonUebaCodecs,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = Lazy(codecsJson)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        return domainVersion
    }

    fun register(
        domainVersion: BaboonDomainVersion,
        codecsJson: () -> AbstractBaboonJsonCodecs,
        codecsBin: () -> AbstractBaboonUebaCodecs,
        meta: () -> BaboonMeta,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsJson[domainVersion] = Lazy(codecsJson)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        versionsMeta[domainVersion] = Lazy(meta)
        return domainVersion
    }
    // @baboon:json-end

    fun registerBin(
        domainVersion: BaboonDomainVersion,
        codecsBin: () -> AbstractBaboonUebaCodecs,
        conversions: () -> AbstractBaboonConversions,
        meta: () -> BaboonMeta,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        versionsConversions[domainVersion] = Lazy(conversions)
        versionsMeta[domainVersion] = Lazy(meta)
        return domainVersion
    }

    fun registerBin(
        domainVersion: BaboonDomainVersion,
        codecsBin: () -> AbstractBaboonUebaCodecs,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        return domainVersion
    }

    fun registerBin(
        domainVersion: BaboonDomainVersion,
        codecsBin: () -> AbstractBaboonUebaCodecs,
        meta: () -> BaboonMeta,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsCodecsBin[domainVersion] = Lazy(codecsBin)
        versionsMeta[domainVersion] = Lazy(meta)
        return domainVersion
    }

    fun registerConversions(
        domainVersion: BaboonDomainVersion,
        conversions: () -> AbstractBaboonConversions,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsConversions[domainVersion] = Lazy(conversions)
        return domainVersion
    }

    fun registerMeta(
        domainVersion: BaboonDomainVersion,
        meta: () -> BaboonMeta,
    ): BaboonDomainVersion {
        registerVersion(domainVersion)
        versionsMeta[domainVersion] = Lazy(meta)
        return domainVersion
    }

    @Suppress("UNCHECKED_CAST")
    fun <T : BaboonGenerated> encodeToBin(
        ctx: BaboonCodecContext,
        value: T,
    ): ByteArray {
        val writer = BaboonBinaryWriter()
        val typeMeta = BaboonTypeMeta.from(value)
        val codec = getBinCodec(typeMeta, exact = true) as BaboonBinCodec<T>
        typeMeta.writeBin(writer)
        codec.encode(ctx, writer, value)
        return writer.toByteArray()
    }

    fun decodeFromBin(reader: BaboonBinaryReader): BaboonGenerated {
        val typeMeta = BaboonTypeMeta.readMeta(reader)
            ?: throw BaboonCodecException.DecoderFailure("Cannot decode binary type meta")

        @Suppress("UNCHECKED_CAST")
        val codec = getBinCodec(typeMeta, exact = false) as BaboonBinCodec<BaboonGenerated>
        return codec.decode(BaboonCodecContext.Compact, reader)
    }

    fun decodeFromBin(bytes: ByteArray): BaboonGenerated {
        val reader = BaboonBinaryReader(bytes)
        return decodeFromBin(reader)
    }

    // @baboon:json-start
    @Suppress("UNCHECKED_CAST")
    fun <T : BaboonGenerated> encodeToJson(ctx: BaboonCodecContext, value: T): JsonElement {
        val typeMeta = BaboonTypeMeta.from(value)
        val codec = getJsonCodec(typeMeta, exact = true) as BaboonJsonCodec<T>
        return codec.encode(ctx, value)
    }

    fun decodeFromJson(value: JsonElement): BaboonGenerated? {
        if (value !is JsonObject) return null
        val typeMeta = readJsonMeta(value) ?: return null
        val contentToken = value[CONTENT_JSON_KEY] ?: return null

        @Suppress("UNCHECKED_CAST")
        val codec = getJsonCodec(typeMeta, exact = false) as BaboonJsonCodec<BaboonGenerated>
        return codec.decode(BaboonCodecContext.Compact, contentToken)
    }

    fun decodeFromJson(value: String): BaboonGenerated? {
        val json = Json.parseToJsonElement(value)
        return decodeFromJson(json)
    }

    private fun readJsonMeta(json: JsonObject): BaboonTypeMeta? {
        val d = json["${'$'}d"]?.toString()?.trim('"') ?: return null
        val v = json["${'$'}v"]?.toString()?.trim('"') ?: return null
        val t = json["${'$'}t"]?.toString()?.trim('"') ?: return null
        val uv = json["${'$'}uv"]?.toString()?.trim('"') ?: v
        return BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION, d, v, uv, t)
    }

    private fun getJsonCodec(typeMeta: BaboonTypeMeta, exact: Boolean): BaboonCodecData {
        return getCodec(versionsCodecsJson, typeMeta, exact)
    }
    // @baboon:json-end

    private fun getBinCodec(typeMeta: BaboonTypeMeta, exact: Boolean): BaboonCodecData {
        return getCodec(versionsCodecsBin, typeMeta, exact)
    }

    private fun <TCodecs : AbstractBaboonCodecs> getCodec(
        versionsCodecs: HashMap<BaboonDomainVersion, Lazy<TCodecs>>,
        typeMeta: BaboonTypeMeta,
        exact: Boolean,
    ): BaboonCodecData {
        val versions = domainVersions[typeMeta.domainIdentifier]
            ?.takeIf { it.isNotEmpty() }
            ?: throw BaboonCodecException.CodecNotFound("Unknown domain $${typeMeta.domainIdentifier}.")

        val minVersion = versions.first()
        val maxVersion = versions.last()

        val modelVersion = when {
            typeMeta.versionMinCompat() != null && typeMeta.version().version > maxVersion.version ->
                typeMeta.versionMinCompat()!!
            else -> typeMeta.version()
        }

        return when {
            exact && modelVersion.version == maxVersion.version ->
                getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier)
            modelVersion.version >= minVersion.version && modelVersion.version < maxVersion.version ->
                getCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.typeIdentifier)
            modelVersion.version < minVersion.version ->
                getCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.typeIdentifier)
            else ->
                throw BaboonCodecException.CodecNotFound("Unsupported domain version '$$modelVersion'.")
        }
    }

    private fun <TCodecs : AbstractBaboonCodecs> getCodecExact(
        versionsCodecs: HashMap<BaboonDomainVersion, Lazy<TCodecs>>,
        domainVersion: BaboonDomainVersion,
        typeIdentifier: String,
    ): BaboonCodecData {
        val lazyVersionCodecs = versionsCodecs[domainVersion]
            ?: throw BaboonCodecException.CodecNotFound("No codecs registered for domain version '$$domainVersion'.")
        return lazyVersionCodecs.value.tryFind(typeIdentifier)?.value
            ?: throw BaboonCodecException.CodecNotFound("No codec found for type [$${domainVersion.domainVersion}.$$typeIdentifier] of version '$${domainVersion.version}'.")
    }

    private fun <TCodecs : AbstractBaboonCodecs> getCodecMaxCompat(
        versionsCodecs: HashMap<BaboonDomainVersion, Lazy<TCodecs>>,
        modelVersion: BaboonDomainVersion,
        maxVersion: BaboonDomainVersion,
        typeIdentifier: String,
    ): BaboonCodecData {
        val lazyDomainMeta = versionsMeta[modelVersion]
            ?: throw BaboonCodecException.CodecNotFound("Unknown domain version '$$modelVersion'.")

        val sameVersions = lazyDomainMeta.value.sameInVersions(typeIdentifier)
        val sameVersion = sameVersions.lastOrNull { sv ->
            sv == maxVersion.domainVersion || Version.from(sv) <= maxVersion.version
        } ?: throw BaboonCodecException.CodecNotFound(
            "No max compat codec found for type [$${modelVersion.domainIdentifier}.$$typeIdentifier] of version '$${modelVersion.version}'."
        )

        val maxCompatVersion = BaboonDomainVersion(modelVersion.domainIdentifier, sameVersion)
        return getCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier)
    }

    private fun registerVersion(domainVersion: BaboonDomainVersion): BaboonDomainVersion {
        val versions = domainVersions.getOrPut(domainVersion.domainIdentifier) { mutableListOf() }
        if (!versions.contains(domainVersion)) {
            versions.add(domainVersion)
            versions.sortBy { it.version }
        }
        return domainVersion
    }
}

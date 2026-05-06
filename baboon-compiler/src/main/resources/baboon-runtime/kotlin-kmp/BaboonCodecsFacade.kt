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
            ?: throw BaboonCodecException.CodecNotFound("Unknown domain ${typeMeta.domainIdentifier}.")

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
            // PR-07-D02 fix: non-exact lookup at the latest registered version routes to exact
            // lookup. Without this arm a single-version domain (min == max == model) falls through
            // every other arm because the next one's strict `<` excludes equality, producing a
            // misleading "Unsupported domain version" error. Mirrors Scala/C# fix.
            !exact && modelVersion.version == maxVersion.version ->
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

    fun preload() {
        try {
            versionsCodecsJson.values.forEach { it.value }
            versionsCodecsBin.values.forEach { it.value }
        } catch (_: Throwable) {}
    }

    fun <T : BaboonGeneratedLatest> decodeFromBinLatest(reader: BaboonBinaryReader, targetClass: kotlin.reflect.KClass<T>): Either<BaboonCodecException, T> {
        return try {
            val decoded = decodeFromBin(reader)
            convert(decoded, targetClass)
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        } catch (e: Throwable) {
            Either.Left(BaboonCodecException.DecoderFailure("decodeFromBinLatest: decode failed.", e))
        }
    }

    fun <T : BaboonGeneratedLatest> decodeFromBinLatest(bytes: ByteArray, targetClass: kotlin.reflect.KClass<T>): Either<BaboonCodecException, T> {
        return try {
            val decoded = decodeFromBin(bytes)
            convert(decoded, targetClass)
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        } catch (e: Throwable) {
            Either.Left(BaboonCodecException.DecoderFailure("decodeFromBinLatest: decode failed.", e))
        }
    }

    // @baboon:json-start
    fun <T : BaboonGeneratedLatest> decodeFromJsonLatest(value: kotlinx.serialization.json.JsonElement, targetClass: kotlin.reflect.KClass<T>): Either<BaboonCodecException, T?> {
        return try {
            val decoded = decodeFromJson(value) ?: return Either.Right(null)
            convert(decoded, targetClass).let { r ->
                when (r) {
                    is Either.Left -> r
                    is Either.Right -> Either.Right(r.value)
                }
            }
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        } catch (e: Throwable) {
            Either.Left(BaboonCodecException.DecoderFailure("decodeFromJsonLatest: decode failed.", e))
        }
    }

    fun <T : BaboonGeneratedLatest> decodeFromJsonLatest(value: String, targetClass: kotlin.reflect.KClass<T>): Either<BaboonCodecException, T?> {
        return try {
            val decoded = decodeFromJson(value) ?: return Either.Right(null)
            convert(decoded, targetClass).let { r ->
                when (r) {
                    is Either.Left -> r
                    is Either.Right -> Either.Right(r.value)
                }
            }
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        } catch (e: Throwable) {
            Either.Left(BaboonCodecException.DecoderFailure("decodeFromJsonLatest: decode failed.", e))
        }
    }
    // @baboon:json-end

    private fun <T : BaboonGeneratedLatest> convert(value: BaboonGenerated, targetClass: kotlin.reflect.KClass<T>): Either<BaboonCodecException, T> {
        if (targetClass.isInstance(value)) {
            @Suppress("UNCHECKED_CAST")
            return Either.Right(value as T)
        }
        val domainVersion = value.domainVersion()
        val versions = domainVersions[domainVersion.domainIdentifier]
            ?.takeIf { it.isNotEmpty() }
            ?: return Either.Left(BaboonCodecException.ConverterFailure("Unknown domain '${domainVersion.domainIdentifier}'."))

        var current: BaboonGenerated = value
        for (toVersion in versions) {
            if (current.domainVersion().version >= toVersion.version) continue
            val lazyConversions = versionsConversions[toVersion]
                ?: return Either.Left(BaboonCodecException.ConverterFailure("Cannot find version '$toVersion' conversions."))
            val candidates = lazyConversions.value.findConversions(current)
            val conversion = candidates
                .filter { c -> current::class == c.typeFrom || (current is BaboonAdtMemberMeta && (current as BaboonAdtMemberMeta).baboonAdtType == c.typeFrom) }
                .maxByOrNull { c -> Version.from(c.versionTo) }
                ?: return Either.Left(BaboonCodecException.ConverterFailure("Cannot find version '$toVersion' type [${current::class.simpleName}] conversions."))
            current = try {
                lazyConversions.value.convert(current, conversion)
            } catch (e: Throwable) {
                return Either.Left(BaboonCodecException.ConverterFailure(
                    "Exception while converting type [${current::class.simpleName}] of version '${current.domainVersion()}' to version '$toVersion'.", e
                ))
            }
        }
        return if (targetClass.isInstance(current)) {
            @Suppress("UNCHECKED_CAST")
            Either.Right(current as T)
        } else {
            Either.Left(BaboonCodecException.ConverterFailure(
                "Expected type [${targetClass.simpleName}] after conversion, but got [${current::class.simpleName}]."
            ))
        }
    }

    /** Decode an `AnyOpaque` payload via the registered codec for `(meta.domain, meta.version,
     *  meta.typeid)`. User-facing — `meta` must carry all three components (variant A only). For
     *  variants B/C/D1/D2/D3 use `jsonToUebaBytes`/`uebaToJson` (cross-format helpers with static
     *  fallbacks). PR-04-D02. */
    fun decodeAny(opaque: AnyOpaque): Either<BaboonCodecException, BaboonGenerated> {
        val meta = opaque.meta
        return try {
            val typeMetaResult = buildSyntheticTypeMeta(meta, null, null, null)
            when (typeMetaResult) {
                is Either.Left -> typeMetaResult
                is Either.Right -> {
                    val typeMeta = typeMetaResult.value
                    when (opaque) {
                        is AnyOpaqueUeba -> {
                            @Suppress("UNCHECKED_CAST")
                            val codec = getBinCodec(typeMeta, exact = false) as BaboonBinCodec<BaboonGenerated>
                            try {
                                val reader = BaboonBinaryReader(opaque.bytes)
                                Either.Right(codec.decode(BaboonCodecContext.Compact, reader))
                            } catch (e: Throwable) {
                                Either.Left(
                                    BaboonCodecException.DecoderFailure(
                                        "decodeAny: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                        e,
                                    )
                                )
                            }
                        }
                        // @baboon:json-start
                        is AnyOpaqueJson -> {
                            @Suppress("UNCHECKED_CAST")
                            val codec = getJsonCodec(typeMeta, exact = false) as BaboonJsonCodec<BaboonGenerated>
                            try {
                                Either.Right(codec.decode(BaboonCodecContext.Compact, opaque.json))
                            } catch (e: Throwable) {
                                Either.Left(
                                    BaboonCodecException.DecoderFailure(
                                        "decodeAny: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                        e,
                                    )
                                )
                            }
                        }
                        // @baboon:json-end
                    }
                }
            }
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        }
    }

    // @baboon:json-start
    /** Cross-format helper: decode an `AnyOpaqueJson` payload via the registered JSON codec, then
     *  re-encode via UEBA. Static fallbacks fill components missing from the wire `meta` (variants
     *  B/C/D1/D2/D3). Wire data wins (override semantics). See PR-06-D01. */
    fun jsonToUebaBytes(
        meta: AnyMeta,
        json: JsonElement,
        staticDomain: String? = null,
        staticVersion: String? = null,
        staticTypeid: String? = null,
    ): Either<BaboonCodecException, ByteArray> {
        return try {
            val typeMetaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid)
            when (typeMetaResult) {
                is Either.Left -> typeMetaResult
                is Either.Right -> {
                    val typeMeta = typeMetaResult.value
                    @Suppress("UNCHECKED_CAST")
                    val jsonCodec = getJsonCodec(typeMeta, exact = false) as BaboonJsonCodec<BaboonGenerated>
                    @Suppress("UNCHECKED_CAST")
                    val binCodec = getBinCodec(typeMeta, exact = false) as BaboonBinCodec<BaboonGenerated>
                    val typed = try {
                        jsonCodec.decode(BaboonCodecContext.Compact, json)
                    } catch (e: Throwable) {
                        return Either.Left(
                            BaboonCodecException.DecoderFailure(
                                "jsonToUebaBytes: cannot decode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                e,
                            )
                        )
                    }
                    try {
                        val writer = BaboonBinaryWriter()
                        binCodec.encode(BaboonCodecContext.Compact, writer, typed)
                        Either.Right(writer.toByteArray())
                    } catch (e: Throwable) {
                        Either.Left(
                            BaboonCodecException.EncoderFailure(
                                "jsonToUebaBytes: cannot encode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                e,
                            )
                        )
                    }
                }
            }
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        }
    }

    /** Cross-format helper symmetric to `jsonToUebaBytes`. */
    fun uebaToJson(
        meta: AnyMeta,
        bytes: ByteArray,
        staticDomain: String? = null,
        staticVersion: String? = null,
        staticTypeid: String? = null,
    ): Either<BaboonCodecException, JsonElement> {
        return try {
            val typeMetaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid)
            when (typeMetaResult) {
                is Either.Left -> typeMetaResult
                is Either.Right -> {
                    val typeMeta = typeMetaResult.value
                    @Suppress("UNCHECKED_CAST")
                    val binCodec = getBinCodec(typeMeta, exact = false) as BaboonBinCodec<BaboonGenerated>
                    @Suppress("UNCHECKED_CAST")
                    val jsonCodec = getJsonCodec(typeMeta, exact = false) as BaboonJsonCodec<BaboonGenerated>
                    val typed = try {
                        val reader = BaboonBinaryReader(bytes)
                        binCodec.decode(BaboonCodecContext.Compact, reader)
                    } catch (e: Throwable) {
                        return Either.Left(
                            BaboonCodecException.DecoderFailure(
                                "uebaToJson: cannot decode UEBA payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                e,
                            )
                        )
                    }
                    try {
                        Either.Right(jsonCodec.encode(BaboonCodecContext.Compact, typed))
                    } catch (e: Throwable) {
                        Either.Left(
                            BaboonCodecException.EncoderFailure(
                                "uebaToJson: cannot encode JSON payload of type [${typeMeta.domainIdentifier}.${typeMeta.typeIdentifier}] of version '${typeMeta.domainVersion}'.",
                                e,
                            )
                        )
                    }
                }
            }
        } catch (e: BaboonCodecException) {
            Either.Left(e)
        }
    }
    // @baboon:json-end

    /** Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks. Wire data
     *  wins (override semantics). PR-06-D01. */
    private fun buildSyntheticTypeMeta(
        meta: AnyMeta,
        staticDomain: String?,
        staticVersion: String?,
        staticTypeid: String?,
    ): Either<BaboonCodecException, BaboonTypeMeta> {
        val domain = meta.domain ?: staticDomain
        val version = meta.version ?: staticVersion
        val typeid = meta.typeid ?: staticTypeid
        if (domain != null && version != null && typeid != null) {
            return Either.Right(
                BaboonTypeMeta(
                    BaboonTypeMetaCodec.META_VERSION,
                    domain,
                    version,
                    version,
                    typeid,
                )
            )
        }
        val missing = buildList {
            if (domain == null) add("domain")
            if (version == null) add("version")
            if (typeid == null) add("typeid")
        }.joinToString(", ")
        return Either.Left(
            BaboonCodecException.DecoderFailure(
                "AnyMeta requires domain/version/typeid for facade resolution; got kind 0x${(meta.kind.toInt() and 0xFF).toString(16)} which lacks: $missing"
            )
        )
    }
}

package baboon.runtime.shared;

import baboon.runtime.shared.BaboonAnyOpaque.AnyMeta;
import baboon.runtime.shared.BaboonAnyOpaque.AnyMetaCodec;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaque;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueJson;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueUeba;
import baboon.runtime.shared.BaboonTypeMeta.BaboonTypeMetaCodec;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

/**
 * Facade over generated per-domain codec registries. Mirrors C# `BaboonCodecsFacade.cs` and
 * Kotlin `BaboonCodecsFacade.kt`. Key surfaces: codec/conversion/meta registration, version-
 * compat dispatch (`getCodec` per PR-07-D02), encode/decode entry points, cross-version
 * `convert<>`, `decodeAny`, and cross-format `jsonToUebaBytes`/`uebaToJson` with optional
 * static fallbacks per PR-06-D01.
 */
public class BaboonCodecsFacade {
    private static final String CONTENT_JSON_KEY = "$c";
    private static final ObjectMapper JSON_PARSER = new ObjectMapper();

    private final ConcurrentHashMap<BaboonDomainVersion, Lazy<? extends AbstractBaboonJsonCodecs>> versionsCodecsJson = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<BaboonDomainVersion, Lazy<? extends AbstractBaboonUebaCodecs>> versionsCodecsBin = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<BaboonDomainVersion, Lazy<? extends AbstractBaboonConversions>> versionsConversions = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<BaboonDomainVersion, Lazy<? extends BaboonMeta>> versionsMeta = new ConcurrentHashMap<>();
    private final ConcurrentHashMap<String, List<BaboonDomainVersion>> domainVersions = new ConcurrentHashMap<>();
    private final Object domainVersionsLock = new Object();

    public BaboonVersion latest(String domain) {
        List<BaboonDomainVersion> versions = domainVersions.get(domain);
        if (versions != null && !versions.isEmpty()) {
            return versions.get(versions.size() - 1).version();
        }
        throw new BaboonException("No registered version for " + domain + " domain found.");
    }

    public void register(BaboonCodecsFacade other) {
        for (var e : other.domainVersions.entrySet()) domainVersions.put(e.getKey(), e.getValue());
        for (var e : other.versionsCodecsJson.entrySet()) versionsCodecsJson.put(e.getKey(), e.getValue());
        for (var e : other.versionsCodecsBin.entrySet()) versionsCodecsBin.put(e.getKey(), e.getValue());
        for (var e : other.versionsConversions.entrySet()) versionsConversions.put(e.getKey(), e.getValue());
        for (var e : other.versionsMeta.entrySet()) versionsMeta.put(e.getKey(), e.getValue());
    }

    public BaboonDomainVersion register(
        BaboonDomainVersion domainVersion,
        Supplier<AbstractBaboonJsonCodecs> codecsJson,
        Supplier<AbstractBaboonUebaCodecs> codecsBin,
        Supplier<AbstractBaboonConversions> conversions,
        Supplier<BaboonMeta> meta
    ) {
        registerVersion(domainVersion);
        versionsCodecsJson.put(domainVersion, Lazy.of(codecsJson));
        versionsCodecsBin.put(domainVersion, Lazy.of(codecsBin));
        versionsConversions.put(domainVersion, Lazy.of(conversions));
        versionsMeta.put(domainVersion, Lazy.of(meta));
        return domainVersion;
    }

    public BaboonDomainVersion register(
        BaboonDomainVersion domainVersion,
        Supplier<AbstractBaboonJsonCodecs> codecsJson,
        Supplier<AbstractBaboonUebaCodecs> codecsBin
    ) {
        registerVersion(domainVersion);
        versionsCodecsJson.put(domainVersion, Lazy.of(codecsJson));
        versionsCodecsBin.put(domainVersion, Lazy.of(codecsBin));
        return domainVersion;
    }

    public BaboonDomainVersion register(
        BaboonDomainVersion domainVersion,
        Supplier<AbstractBaboonJsonCodecs> codecsJson,
        Supplier<AbstractBaboonUebaCodecs> codecsBin,
        Supplier<BaboonMeta> meta
    ) {
        registerVersion(domainVersion);
        versionsCodecsJson.put(domainVersion, Lazy.of(codecsJson));
        versionsCodecsBin.put(domainVersion, Lazy.of(codecsBin));
        versionsMeta.put(domainVersion, Lazy.of(meta));
        return domainVersion;
    }

    public BaboonDomainVersion registerConversions(
        BaboonDomainVersion domainVersion,
        Supplier<AbstractBaboonConversions> conversions
    ) {
        registerVersion(domainVersion);
        versionsConversions.put(domainVersion, Lazy.of(conversions));
        return domainVersion;
    }

    public BaboonDomainVersion registerMeta(
        BaboonDomainVersion domainVersion,
        Supplier<BaboonMeta> meta
    ) {
        registerVersion(domainVersion);
        versionsMeta.put(domainVersion, Lazy.of(meta));
        return domainVersion;
    }

    public void verify() {
        if (domainVersions.isEmpty()) {
            throw new BaboonException("Baboon codecs must have at least one domain registered.");
        }
        for (List<BaboonDomainVersion> versions : domainVersions.values()) {
            for (BaboonDomainVersion dv : versions) {
                if (!versionsConversions.containsKey(dv)) {
                    throw new BaboonCodecException.ConversionNotFound(
                        "Baboon codecs must have conversion for " + dv + " registered.");
                }
                if (!versionsMeta.containsKey(dv)) {
                    throw new BaboonCodecException.CodecNotFound(
                        "Baboon codecs must have codecs for " + dv + " registered.");
                }
            }
        }
    }

    public BaboonEither<BaboonCodecException, byte[]> encodeToBin(BaboonCodecContext ctx, BaboonGenerated value) {
        return encodeToBin(ctx, value, null);
    }

    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, byte[]> encodeToBin(
        BaboonCodecContext ctx, BaboonGenerated value, BaboonTypeMeta typeMetaOverride
    ) {
        BaboonTypeMeta typeMeta = BaboonTypeMeta.from(value, value.getClass());
        BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getBinCodec(typeMeta, true);
        if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> l) {
            return BaboonEither.left(l.value());
        }
        BaboonBinCodec<BaboonGenerated> codec = (BaboonBinCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();

        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            LEDataOutputStream writer = new LEDataOutputStream(baos);
            (typeMetaOverride != null ? typeMetaOverride : typeMeta).writeBin(writer);
            codec.encode(ctx, writer, value);
            writer.flush();
            return BaboonEither.right(baos.toByteArray());
        } catch (Exception e) {
            return BaboonEither.left(
                new BaboonCodecException.EncoderFailure(
                    "Exception while trying to encode to binary form type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                    e));
        }
    }

    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromBin(LEDataInputStream reader) {
        BaboonTypeMeta typeMeta;
        try {
            typeMeta = BaboonTypeMeta.readMeta(reader);
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure("Cannot decode binary type meta", e));
        }
        if (typeMeta == null) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure("Cannot decode binary type meta"));
        }

        BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getBinCodec(typeMeta, false);
        if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> l) {
            return BaboonEither.left(l.value());
        }
        BaboonBinCodec<BaboonGenerated> codec = (BaboonBinCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();

        try {
            return BaboonEither.right(codec.decode(BaboonCodecContext.Compact, reader));
        } catch (Exception e) {
            return BaboonEither.left(
                new BaboonCodecException.DecoderFailure(
                    "Can not decode BIN form type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                    e));
        }
    }

    public BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromBin(byte[] bytes) {
        return decodeFromBin(new LEDataInputStream(new ByteArrayInputStream(bytes)));
    }

    public BaboonEither<BaboonCodecException, JsonNode> encodeToJson(BaboonGenerated value) {
        return encodeToJson(value, null);
    }

    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, JsonNode> encodeToJson(
        BaboonGenerated value, BaboonTypeMeta typeMetaOverride
    ) {
        BaboonTypeMeta typeMeta = BaboonTypeMeta.from(value, value.getClass());
        BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getJsonCodec(typeMeta, true);
        if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> l) {
            return BaboonEither.left(l.value());
        }
        BaboonJsonCodec<BaboonGenerated> codec = (BaboonJsonCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();

        try {
            JsonNode content = codec.encode(BaboonCodecContext.Compact, value);
            JsonNode metaJson = (typeMetaOverride != null ? typeMetaOverride : typeMeta).writeJson();
            if (!(metaJson instanceof ObjectNode metaObj)) {
                return BaboonEither.left(new BaboonCodecException.EncoderFailure(
                    "BaboonTypeMeta.writeJson must return an ObjectNode; got " + metaJson.getNodeType()));
            }
            metaObj.set(CONTENT_JSON_KEY, content);
            return BaboonEither.right(metaObj);
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.EncoderFailure(
                "Can not encode to json form type [" + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                e));
        }
    }

    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromJson(JsonNode value) {
        BaboonTypeMeta typeMeta = BaboonTypeMeta.readMeta(value);
        if (typeMeta == null) return BaboonEither.right(null);
        if (!(value instanceof ObjectNode obj) || obj.get(CONTENT_JSON_KEY) == null) {
            return BaboonEither.right(null);
        }
        JsonNode contentToken = obj.get(CONTENT_JSON_KEY);

        BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getJsonCodec(typeMeta, false);
        if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> l) {
            return BaboonEither.left(l.value());
        }
        BaboonJsonCodec<BaboonGenerated> codec = (BaboonJsonCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();

        try {
            return BaboonEither.right(codec.decode(BaboonCodecContext.Compact, contentToken));
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure(
                "Can not decode JSON form type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'. JSON: " + value,
                e));
        }
    }

    public BaboonEither<BaboonCodecException, BaboonGenerated> decodeFromJson(String value) {
        try {
            JsonNode parsed = JSON_PARSER.readTree(value);
            return decodeFromJson(parsed);
        } catch (JsonProcessingException e) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure("Cannot parse JSON: " + e.getMessage(), e));
        }
    }

    /**
     * Decode an `AnyOpaque` payload via the registered codec for `(meta.domain, meta.version,
     * meta.typeid)`. User-facing — `meta` must carry all three components (variant A only). For
     * variants B/C/D1/D2/D3 use the cross-format helpers (`jsonToUebaBytes` / `uebaToJson`)
     * which accept static fallbacks. PR-04-D02: errors thread through `BaboonEither` rather
     * than throwing.
     */
    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, BaboonGenerated> decodeAny(AnyOpaque opaque) {
        BaboonEither<BaboonCodecException, BaboonTypeMeta> metaResult = buildSyntheticTypeMeta(opaque.meta(), null, null, null);
        if (metaResult instanceof BaboonEither.Left<BaboonCodecException, BaboonTypeMeta> l) {
            return BaboonEither.left(l.value());
        }
        BaboonTypeMeta typeMeta = ((BaboonEither.Right<BaboonCodecException, BaboonTypeMeta>) metaResult).value();

        if (opaque instanceof AnyOpaqueUeba u) {
            BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getBinCodec(typeMeta, false);
            if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> cl) {
                return BaboonEither.left(cl.value());
            }
            BaboonBinCodec<BaboonGenerated> codec = (BaboonBinCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();
            try {
                LEDataInputStream reader = new LEDataInputStream(new ByteArrayInputStream(u.bytes()));
                return BaboonEither.right(codec.decode(BaboonCodecContext.Compact, reader));
            } catch (Exception e) {
                return BaboonEither.left(new BaboonCodecException.DecoderFailure(
                    "decodeAny: cannot decode UEBA payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                    e));
            }
        }
        if (opaque instanceof AnyOpaqueJson j) {
            BaboonEither<BaboonCodecException, BaboonCodecData> codecResult = getJsonCodec(typeMeta, false);
            if (codecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> cl) {
                return BaboonEither.left(cl.value());
            }
            BaboonJsonCodec<BaboonGenerated> codec = (BaboonJsonCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) codecResult).value();
            try {
                return BaboonEither.right(codec.decode(BaboonCodecContext.Compact, j.json()));
            } catch (Exception e) {
                return BaboonEither.left(new BaboonCodecException.DecoderFailure(
                    "decodeAny: cannot decode JSON payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                    e));
            }
        }
        return BaboonEither.left(new BaboonCodecException.DecoderFailure(
            "decodeAny: unknown AnyOpaque branch " + opaque.getClass().getName()));
    }

    /**
     * Cross-format helper: decode an `AnyOpaqueJson` payload via the registered JSON codec, then
     * re-encode it via the registered UEBA codec. The wire `meta` may omit components that the
     * field's static declaration already pins down (variants B/C/D1/D2/D3). The codec generator
     * passes the static fallbacks; runtime `meta.X` takes precedence over `staticX` (override
     * semantics). Without static fallback only variant A would work — see PR-06-D01.
     */
    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, byte[]> jsonToUebaBytes(
        AnyMeta meta, JsonNode json, String staticDomain, String staticVersion, String staticTypeid
    ) {
        BaboonEither<BaboonCodecException, BaboonTypeMeta> metaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
        if (metaResult instanceof BaboonEither.Left<BaboonCodecException, BaboonTypeMeta> l) {
            return BaboonEither.left(l.value());
        }
        BaboonTypeMeta typeMeta = ((BaboonEither.Right<BaboonCodecException, BaboonTypeMeta>) metaResult).value();

        BaboonEither<BaboonCodecException, BaboonCodecData> jsonCodecResult = getJsonCodec(typeMeta, false);
        if (jsonCodecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> jl) {
            return BaboonEither.left(jl.value());
        }
        BaboonJsonCodec<BaboonGenerated> jsonCodec = (BaboonJsonCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) jsonCodecResult).value();

        BaboonEither<BaboonCodecException, BaboonCodecData> binCodecResult = getBinCodec(typeMeta, false);
        if (binCodecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> bl) {
            return BaboonEither.left(bl.value());
        }
        BaboonBinCodec<BaboonGenerated> binCodec = (BaboonBinCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) binCodecResult).value();

        BaboonGenerated typed;
        try {
            typed = jsonCodec.decode(BaboonCodecContext.Compact, json);
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure(
                "jsonToUebaBytes: cannot decode JSON payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                e));
        }

        try {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            LEDataOutputStream writer = new LEDataOutputStream(baos);
            binCodec.encode(BaboonCodecContext.Compact, writer, typed);
            writer.flush();
            return BaboonEither.right(baos.toByteArray());
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.EncoderFailure(
                "jsonToUebaBytes: cannot encode UEBA payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                e));
        }
    }

    public BaboonEither<BaboonCodecException, byte[]> jsonToUebaBytes(AnyMeta meta, JsonNode json) {
        return jsonToUebaBytes(meta, json, null, null, null);
    }

    /** Cross-format helper symmetric to {@link #jsonToUebaBytes}. See its doc for static-fallback contract. */
    @SuppressWarnings("unchecked")
    public BaboonEither<BaboonCodecException, JsonNode> uebaToJson(
        AnyMeta meta, byte[] bytes, String staticDomain, String staticVersion, String staticTypeid
    ) {
        BaboonEither<BaboonCodecException, BaboonTypeMeta> metaResult = buildSyntheticTypeMeta(meta, staticDomain, staticVersion, staticTypeid);
        if (metaResult instanceof BaboonEither.Left<BaboonCodecException, BaboonTypeMeta> l) {
            return BaboonEither.left(l.value());
        }
        BaboonTypeMeta typeMeta = ((BaboonEither.Right<BaboonCodecException, BaboonTypeMeta>) metaResult).value();

        BaboonEither<BaboonCodecException, BaboonCodecData> binCodecResult = getBinCodec(typeMeta, false);
        if (binCodecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> bl) {
            return BaboonEither.left(bl.value());
        }
        BaboonBinCodec<BaboonGenerated> binCodec = (BaboonBinCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) binCodecResult).value();

        BaboonEither<BaboonCodecException, BaboonCodecData> jsonCodecResult = getJsonCodec(typeMeta, false);
        if (jsonCodecResult instanceof BaboonEither.Left<BaboonCodecException, BaboonCodecData> jl) {
            return BaboonEither.left(jl.value());
        }
        BaboonJsonCodec<BaboonGenerated> jsonCodec = (BaboonJsonCodec<BaboonGenerated>) ((BaboonEither.Right<BaboonCodecException, BaboonCodecData>) jsonCodecResult).value();

        BaboonGenerated typed;
        try {
            LEDataInputStream reader = new LEDataInputStream(new ByteArrayInputStream(bytes));
            typed = binCodec.decode(BaboonCodecContext.Compact, reader);
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.DecoderFailure(
                "uebaToJson: cannot decode UEBA payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                e));
        }

        try {
            return BaboonEither.right(jsonCodec.encode(BaboonCodecContext.Compact, typed));
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.EncoderFailure(
                "uebaToJson: cannot encode JSON payload of type [" + typeMeta.domainIdentifier() + "." + typeMeta.typeIdentifier() + "] of version '" + typeMeta.domainVersion() + "'.",
                e));
        }
    }

    public BaboonEither<BaboonCodecException, JsonNode> uebaToJson(AnyMeta meta, byte[] bytes) {
        return uebaToJson(meta, bytes, null, null, null);
    }

    /**
     * Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks. `AnyMeta`
     * does not carry a min-compat version; forward-version migration is unavailable for any-
     * payloads, so `domainVersionMinCompat = version`. `meta.X` takes precedence over `staticX`
     * (override semantics — wire data wins). `decodeAny` calls with all-null statics so its
     * variant-A-only contract is preserved.
     */
    private static BaboonEither<BaboonCodecException, BaboonTypeMeta> buildSyntheticTypeMeta(
        AnyMeta meta, String staticDomain, String staticVersion, String staticTypeid
    ) {
        String domain = meta.domain() != null ? meta.domain() : staticDomain;
        String version = meta.version() != null ? meta.version() : staticVersion;
        String typeid = meta.typeid() != null ? meta.typeid() : staticTypeid;

        if (domain != null && version != null && typeid != null) {
            return BaboonEither.right(new BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION, domain, version, version, typeid));
        }

        List<String> missing = new ArrayList<>();
        if (domain == null) missing.add("domain");
        if (version == null) missing.add("version");
        if (typeid == null) missing.add("typeid");
        return BaboonEither.left(new BaboonCodecException.DecoderFailure(
            "AnyMeta requires domain/version/typeid for facade resolution; got kind 0x" + Integer.toHexString(meta.kind() & 0xFF) + " which lacks: " + String.join(", ", missing)));
    }

    // ----- cross-version conversion -------------------------------------------------------------

    /**
     * Single-step convert via the registered conversion for `(TFrom, TTo)`. Mirrors a simplified
     * subset of C# `Convert<TFrom, TTo>`: the existing Java `AbstractBaboonConversions` indexes
     * conversions by `(from-class → to-class)` pair, so a generic multi-step walk would require
     * `findConversions(current)` introspection that this runtime doesn't yet expose. PR 6.1 ships
     * the trivial pair-lookup; multi-step chaining can land in a later milestone if needed.
     */
    @SuppressWarnings("unchecked")
    public <TFrom extends BaboonGenerated, TTo extends BaboonGeneratedLatest> BaboonEither<BaboonCodecException, TTo> convert(
        TFrom value, Class<TFrom> fromClass, Class<TTo> targetClass
    ) {
        if (targetClass.isInstance(value)) return BaboonEither.right((TTo) value);

        Class<?> actual = value.getClass();
        String fromDomainId;
        String fromDomainVersion;
        try {
            fromDomainId = (String) actual.getField("baboonDomainIdentifier").get(null);
            fromDomainVersion = (String) actual.getField("baboonDomainVersion").get(null);
        } catch (ReflectiveOperationException e) {
            return BaboonEither.left(new BaboonCodecException.ConverterFailure(
                "Cannot read baboon domain metadata from " + actual.getName(), e));
        }
        BaboonDomainVersion dvFrom = new BaboonDomainVersion(fromDomainId, fromDomainVersion);

        List<BaboonDomainVersion> versions = domainVersions.get(dvFrom.domainIdentifier());
        if (versions == null || versions.isEmpty()) {
            return BaboonEither.left(new BaboonCodecException.ConverterFailure(
                "Unknown domain '" + dvFrom.domainIdentifier() + "'."));
        }
        BaboonDomainVersion toVersion = versions.get(versions.size() - 1);

        Lazy<? extends AbstractBaboonConversions> lazyConv = versionsConversions.get(toVersion);
        if (lazyConv == null) {
            return BaboonEither.left(new BaboonCodecException.ConverterFailure(
                "Can not find version '" + toVersion + "' conversions."));
        }
        try {
            TTo result = lazyConv.get().convertWithContext(null, value, fromClass, targetClass);
            return BaboonEither.right(result);
        } catch (Exception e) {
            return BaboonEither.left(new BaboonCodecException.ConverterFailure(
                "Exception while converting type [" + actual.getName() + "] of version '" + dvFrom + "' to version '" + toVersion + "'.",
                e));
        }
    }

    // ----- private dispatch ---------------------------------------------------------------------

    private BaboonEither<BaboonCodecException, BaboonCodecData> getBinCodec(BaboonTypeMeta typeMeta, boolean exact) {
        return getCodec(versionsCodecsBin, typeMeta, exact);
    }

    private BaboonEither<BaboonCodecException, BaboonCodecData> getJsonCodec(BaboonTypeMeta typeMeta, boolean exact) {
        return getCodec(versionsCodecsJson, typeMeta, exact);
    }

    private <TCodecs extends AbstractBaboonCodecs> BaboonEither<BaboonCodecException, BaboonCodecData> getCodec(
        Map<BaboonDomainVersion, Lazy<? extends TCodecs>> versionsCodecs,
        BaboonTypeMeta typeMeta,
        boolean exact
    ) {
        List<BaboonDomainVersion> versions = domainVersions.get(typeMeta.domainIdentifier());
        if (versions == null || versions.isEmpty()) {
            return BaboonEither.left(new BaboonCodecException.CodecNotFound(
                "Unknown domain " + typeMeta.domainIdentifier() + "."));
        }

        BaboonDomainVersion minVersion = versions.get(0);
        BaboonDomainVersion maxVersion = versions.get(versions.size() - 1);

        BaboonDomainVersion lookupVersion = typeMeta.versionRef();
        BaboonDomainVersion minCompat = typeMeta.versionMinCompat();
        BaboonDomainVersion modelVersion = (minCompat != null && lookupVersion.version().compareTo(maxVersion.version()) > 0)
            ? minCompat
            : lookupVersion;

        BaboonVersion modelV = modelVersion.version();
        BaboonVersion maxV = maxVersion.version();
        BaboonVersion minV = minVersion.version();

        if (exact && modelV.compareTo(maxV) == 0) {
            return getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier());
        }
        // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
        // Without this arm a single-version domain (min == max == model) falls through every other
        // arm because the next one's strict `<` excludes equality, producing a misleading
        // "Unsupported domain version" error. Mirrors Scala/C#/Kotlin fix.
        if (!exact && modelV.compareTo(maxV) == 0) {
            return getCodecExact(versionsCodecs, modelVersion, typeMeta.typeIdentifier());
        }
        if (modelV.compareTo(minV) >= 0 && modelV.compareTo(maxV) < 0) {
            return getCodecMaxCompat(versionsCodecs, modelVersion, maxVersion, typeMeta.typeIdentifier());
        }
        if (modelV.compareTo(minV) < 0) {
            return getCodecMaxCompat(versionsCodecs, minVersion, maxVersion, typeMeta.typeIdentifier());
        }
        return BaboonEither.left(new BaboonCodecException.CodecNotFound(
            "Unsupported domain version '" + modelVersion + "'."));
    }

    private static <TCodecs extends AbstractBaboonCodecs> BaboonEither<BaboonCodecException, BaboonCodecData> getCodecExact(
        Map<BaboonDomainVersion, Lazy<? extends TCodecs>> versionsCodecs,
        BaboonDomainVersion domainVersion,
        String typeIdentifier
    ) {
        Lazy<? extends TCodecs> lazyCodecs = versionsCodecs.get(domainVersion);
        if (lazyCodecs == null) {
            return BaboonEither.left(new BaboonCodecException.CodecNotFound(
                "No codecs registered for domain version '" + domainVersion + "'."));
        }
        Lazy<? extends BaboonCodecData> lazyCodec = lazyCodecs.get().tryFind(typeIdentifier);
        if (lazyCodec == null) {
            return BaboonEither.left(new BaboonCodecException.CodecNotFound(
                "No codec found for type [" + domainVersion.domainVersion() + "." + typeIdentifier + "] of version '" + domainVersion.version() + "'."));
        }
        return BaboonEither.right(lazyCodec.get());
    }

    private <TCodecs extends AbstractBaboonCodecs> BaboonEither<BaboonCodecException, BaboonCodecData> getCodecMaxCompat(
        Map<BaboonDomainVersion, Lazy<? extends TCodecs>> versionsCodecs,
        BaboonDomainVersion modelVersion,
        BaboonDomainVersion maxVersion,
        String typeIdentifier
    ) {
        Lazy<? extends BaboonMeta> lazyMeta = versionsMeta.get(modelVersion);
        if (lazyMeta == null) {
            return BaboonEither.left(new BaboonCodecException.CodecNotFound(
                "Unknown domain version '" + modelVersion + "'."));
        }
        List<String> sameVersions = lazyMeta.get().sameInVersions(typeIdentifier);
        String bestSame = null;
        for (int i = sameVersions.size() - 1; i >= 0; i--) {
            String sv = sameVersions.get(i);
            if (sv.equals(maxVersion.domainVersion()) || BaboonVersion.from(sv).compareTo(maxVersion.version()) <= 0) {
                bestSame = sv;
                break;
            }
        }
        if (bestSame == null) {
            return BaboonEither.left(new BaboonCodecException.CodecNotFound(
                "No max compat codec found for type [" + modelVersion.domainIdentifier() + "." + typeIdentifier + "] of version '" + modelVersion.domainVersion() + "'."));
        }
        BaboonDomainVersion maxCompatVersion = new BaboonDomainVersion(modelVersion.domainIdentifier(), bestSame);
        return getCodecExact(versionsCodecs, maxCompatVersion, typeIdentifier);
    }

    private void registerVersion(BaboonDomainVersion domainVersion) {
        // Sort-by-version on insert so getCodec can read first/last as min/max in one pass.
        synchronized (domainVersionsLock) {
            List<BaboonDomainVersion> existing = domainVersions.get(domainVersion.domainIdentifier());
            if (existing != null) {
                if (existing.contains(domainVersion)) return;
                List<BaboonDomainVersion> updated = new ArrayList<>(existing);
                updated.add(domainVersion);
                updated.sort((a, b) -> a.version().compareTo(b.version()));
                domainVersions.put(domainVersion.domainIdentifier(), updated);
            } else {
                List<BaboonDomainVersion> fresh = new ArrayList<>();
                fresh.add(domainVersion);
                domainVersions.put(domainVersion.domainIdentifier(), fresh);
            }
        }
    }
}

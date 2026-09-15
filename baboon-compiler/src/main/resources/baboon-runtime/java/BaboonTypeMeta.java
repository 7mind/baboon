package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.List;
import java.util.Map;

public record BaboonTypeMeta(
    byte metaVersion,
    String domainIdentifier,
    String domainVersion,
    String domainVersionMinCompat,
    String typeIdentifier,
    /**
     * Oldest domain version whose JSON codec can decode the payload under the json-additive
     * contract (tolerant key lookup; fields unknown to that version are dropped). Always
     * <= domainVersionMinCompat. Published as `$rv` when it differs from the (effective)
     * minCompat; the binary v1 envelope does not carry it.
     */
    String domainVersionReadableMin
) {
    /** Five-field form: readable-min defaults to minCompat (no forward-read beyond byte-identity). */
    public BaboonTypeMeta(byte metaVersion, String domainIdentifier, String domainVersion, String domainVersionMinCompat, String typeIdentifier) {
        this(metaVersion, domainIdentifier, domainVersion, domainVersionMinCompat, typeIdentifier, domainVersionMinCompat);
    }

    /** Tier key of the JSON envelope's readable-min bound in the generated `baboonMinReaderVersions`. */
    public static final String JSON_READABLE_TIER = "json-additive";
    /** Tier keys of the UEBA prefix bounds in the generated `baboonMinReaderVersions`, per index mode. */
    public static final String UEBA_PREFIX_COMPACT_TIER = "prefix-compact";
    public static final String UEBA_PREFIX_ANY_MODE_TIER = "prefix-any-mode";

    public BaboonDomainVersion versionRef() {
        return new BaboonDomainVersion(domainIdentifier, domainVersion);
    }

    public BaboonDomainVersion versionReadableMin() {
        if (domainVersionReadableMin == null || domainVersionReadableMin.isEmpty()) return versionMinCompat();
        if (domainVersionReadableMin.equals(domainVersion)) return null;
        return new BaboonDomainVersion(domainIdentifier, domainVersionReadableMin);
    }

    public BaboonDomainVersion versionMinCompat() {
        if (domainVersionMinCompat == null || domainVersionMinCompat.isEmpty()) return null;
        if (domainVersionMinCompat.equals(domainVersion)) return null;
        return new BaboonDomainVersion(domainIdentifier, domainVersionMinCompat);
    }

    public void writeBin(LEDataOutputStream writer) throws Exception {
        BaboonTypeMetaCodec.writeBin(this, writer);
    }

    public JsonNode writeJson() {
        return BaboonTypeMetaCodec.writeJson(this);
    }

    /**
     * Codec discovery with ADT awareness, mirroring Scala/C#/Kotlin. Java codegen emits per-type
     * metadata as `public static final` fields rather than interface methods, so we read them
     * reflectively. When the user-declared static type is the ADT trait/interface and the value
     * implements `BaboonAdtMemberMeta`, use the ADT's type identifier so the encoder can wrap with
     * the ADT meta envelope. When the user-declared type is the concrete branch, use the branch
     * identifier directly.
     */
    @SuppressWarnings("unchecked")
    public static BaboonTypeMeta from(BaboonGenerated value, Class<?> declaredType) {
        Class<?> actual = value.getClass();

        String typeIdentifier;
        if (value instanceof BaboonAdtMemberMeta && declaredType != null && declaredType.isInterface()) {
            typeIdentifier = readStaticString(actual, "baboonAdtTypeIdentifier");
        } else {
            typeIdentifier = readStaticString(actual, "baboonTypeIdentifier");
        }

        String domainIdentifier = readStaticString(actual, "baboonDomainIdentifier");
        String domainVersion = readStaticString(actual, "baboonDomainVersion");

        // Codegen invariant (mirrors PR-08-D02): baboonSameInVersions is always non-empty. Index
        // directly so a violation throws IndexOutOfBoundsException rather than silently masquerading
        // as a same-version meta.
        List<String> sameIn;
        try {
            sameIn = (List<String>) actual.getField("baboonSameInVersions").get(null);
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + actual.getName() + " is missing static field 'baboonSameInVersions'", e);
        }
        String minCompat = sameIn.get(0);

        Map<String, String> minReaders = readMinReaderVersions(actual);
        String readableMin = minReaders.get(JSON_READABLE_TIER);
        if (readableMin == null) {
            throw new BaboonException("Type " + actual.getName() + ": baboonMinReaderVersions lacks '" + JSON_READABLE_TIER + "'");
        }

        return new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            domainIdentifier,
            domainVersion,
            minCompat,
            typeIdentifier,
            readableMin
        );
    }

    /**
     * Envelope for a UEBA payload written under `ctx`: {@link #from} with `domainVersionMinCompat`
     * lowered to the prefix bound of the context's index mode when the writer policy is TOLERANT.
     */
    public static BaboonTypeMeta forBin(BaboonGenerated value, Class<?> declaredType, BaboonCodecContext ctx) {
        BaboonTypeMeta meta = from(value, declaredType);
        boolean v2 = ctx.envelopeVersion() == BaboonCodecContext.BaboonEnvelopeVersion.V2;
        if (!v2 && ctx.forwardWritePolicy() == BaboonCodecContext.ForwardWritePolicy.STRICT) {
            return meta;
        }
        String tier = ctx.useIndices() ? UEBA_PREFIX_ANY_MODE_TIER : UEBA_PREFIX_COMPACT_TIER;
        String bound = readMinReaderVersions(value.getClass()).get(tier);
        if (bound == null) {
            throw new BaboonException("Type " + value.getClass().getName() + ": baboonMinReaderVersions lacks '" + tier + "'");
        }
        // V2 carries both bounds (the writer policy is irrelevant); V1 TOLERANT puts the prefix bound in its single slot
        return v2
            ? new BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION_2, meta.domainIdentifier(), meta.domainVersion(), meta.domainVersionMinCompat(), meta.typeIdentifier(), bound)
            : new BaboonTypeMeta(meta.metaVersion(), meta.domainIdentifier(), meta.domainVersion(), bound, meta.typeIdentifier(), meta.domainVersionReadableMin());
    }

    @SuppressWarnings("unchecked")
    private static Map<String, String> readMinReaderVersions(Class<?> actual) {
        try {
            return (Map<String, String>) actual.getField("baboonMinReaderVersions").get(null);
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + actual.getName() + " is missing static field 'baboonMinReaderVersions'", e);
        }
    }

    private static String readStaticString(Class<?> klass, String name) {
        try {
            Object v = klass.getField(name).get(null);
            if (!(v instanceof String s)) {
                throw new BaboonException("Type " + klass.getName() + " field '" + name + "' is not a String");
            }
            return s;
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + klass.getName() + " is missing static field '" + name + "'", e);
        }
    }

    public static BaboonTypeMeta readMeta(LEDataInputStream reader) throws Exception {
        return BaboonTypeMetaCodec.readMeta(reader);
    }

    public static BaboonTypeMeta readMeta(JsonNode json) {
        return BaboonTypeMetaCodec.readMeta(json);
    }

    public static final class BaboonTypeMetaCodec {
        private BaboonTypeMetaCodec() {}

        public static final byte META_VERSION_1 = 1;
        public static final byte META_VERSION_2 = 2;
        /** Layout written by default (binary) and always (JSON `$mv`). */
        public static final byte META_VERSION = META_VERSION_1;

        // v2 flags byte (codec-envelope.md §2.1.3): bit 0 — minCompat follows; bit 1 — readableMin follows.
        private static final int V2_FLAG_MIN_COMPAT = 0x01;
        private static final int V2_FLAG_READABLE_MIN = 0x02;
        private static final int V2_FLAGS_MASK = V2_FLAG_MIN_COMPAT | V2_FLAG_READABLE_MIN;

        public static final String META_VERSION_KEY = "$mv";
        public static final String DOMAIN_IDENTIFIER_KEY = "$d";
        public static final String DOMAIN_VERSION_KEY = "$v";
        public static final String DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv";
        public static final String DOMAIN_VERSION_READABLE_KEY = "$rv";
        public static final String TYPE_IDENTIFIER_KEY = "$t";

        public static void writeBin(BaboonTypeMeta meta, LEDataOutputStream writer) throws Exception {
            if (meta.metaVersion == META_VERSION_1) {
                writeBinV1(meta, writer);
            } else if (meta.metaVersion == META_VERSION_2) {
                writeBinV2(meta, writer);
            } else {
                throw new BaboonException("Unsupported binary envelope metaVersion " + meta.metaVersion);
            }
        }

        // v2: `02 | domainId | domainVersion | flags | [minCompat] | [readableMin] | typeId`; each bound is
        // elided exactly as in JSON (minCompat when == domainVersion, readableMin when == effective minCompat)
        private static void writeBinV2(BaboonTypeMeta meta, LEDataOutputStream writer) throws Exception {
            String minCompat = (meta.domainVersionMinCompat == null || meta.domainVersionMinCompat.isEmpty()) ? meta.domainVersion : meta.domainVersionMinCompat;
            String readableMin = (meta.domainVersionReadableMin == null || meta.domainVersionReadableMin.isEmpty()) ? minCompat : meta.domainVersionReadableMin;
            boolean hasMinCompat = !minCompat.equals(meta.domainVersion);
            boolean hasReadableMin = !readableMin.equals(minCompat);
            writer.writeByte(META_VERSION_2);
            BaboonBinTools.writeString(writer, meta.domainIdentifier);
            BaboonBinTools.writeString(writer, meta.domainVersion);
            writer.writeByte((hasMinCompat ? V2_FLAG_MIN_COMPAT : 0) | (hasReadableMin ? V2_FLAG_READABLE_MIN : 0));
            if (hasMinCompat) BaboonBinTools.writeString(writer, minCompat);
            if (hasReadableMin) BaboonBinTools.writeString(writer, readableMin);
            BaboonBinTools.writeString(writer, meta.typeIdentifier);
        }

        private static void writeBinV1(BaboonTypeMeta meta, LEDataOutputStream writer) throws Exception {
            writer.writeByte(META_VERSION_1);
            BaboonBinTools.writeString(writer, meta.domainIdentifier);
            BaboonBinTools.writeString(writer, meta.domainVersion);
            if (meta.domainVersion.equals(meta.domainVersionMinCompat)) {
                writer.writeByte(0);
            } else {
                writer.writeByte(1);
                BaboonBinTools.writeString(writer, meta.domainVersionMinCompat);
            }
            BaboonBinTools.writeString(writer, meta.typeIdentifier);
        }

        public static JsonNode writeJson(BaboonTypeMeta meta) {
            // MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
            // self-identifying without out-of-band knowledge (proposal §10.6 (a)).
            ObjectNode obj = JsonNodeFactory.instance.objectNode();
            obj.put(META_VERSION_KEY, (int) META_VERSION);
            obj.put(DOMAIN_IDENTIFIER_KEY, meta.domainIdentifier);
            obj.put(DOMAIN_VERSION_KEY, meta.domainVersion);
            obj.put(TYPE_IDENTIFIER_KEY, meta.typeIdentifier);
            if (!meta.domainVersion.equals(meta.domainVersionMinCompat)) {
                obj.put(DOMAIN_VERSION_MIN_COMPAT_KEY, meta.domainVersionMinCompat);
            }
            // `$rv` is elided when it equals the effective `$uv`: unchanged types emit no new bytes
            if (meta.domainVersionReadableMin != null && !meta.domainVersionReadableMin.isEmpty()
                && !meta.domainVersionReadableMin.equals(meta.domainVersionMinCompat)) {
                obj.put(DOMAIN_VERSION_READABLE_KEY, meta.domainVersionReadableMin);
            }
            return obj;
        }

        public static BaboonTypeMeta readMeta(LEDataInputStream reader) throws Exception {
            byte metaVersion = reader.readByte();
            if (metaVersion == META_VERSION_1) return readMetaV1(reader);
            if (metaVersion == META_VERSION_2) return readMetaV2(reader);
            return null;
        }

        private static BaboonTypeMeta readMetaV2(LEDataInputStream reader) throws Exception {
            String domainIdentifier = BaboonBinTools.readString(reader);
            String domainVersion = BaboonBinTools.readString(reader);
            int flags = reader.readByte() & 0xFF;
            // unknown flag bits are illegal; a lenient reader would misparse the strings that follow
            if ((flags & ~V2_FLAGS_MASK) != 0) return null;
            String minCompat = (flags & V2_FLAG_MIN_COMPAT) != 0 ? BaboonBinTools.readString(reader) : domainVersion;
            String readableMin = (flags & V2_FLAG_READABLE_MIN) != 0 ? BaboonBinTools.readString(reader) : minCompat;
            String typeIdentifier = BaboonBinTools.readString(reader);
            return new BaboonTypeMeta(META_VERSION_2, domainIdentifier, domainVersion, minCompat, typeIdentifier, readableMin);
        }

        private static BaboonTypeMeta readMetaV1(LEDataInputStream reader) throws Exception {
            String domainIdentifier = BaboonBinTools.readString(reader);
            String domainVersion = BaboonBinTools.readString(reader);
            byte hasMinCompat = reader.readByte();
            // codec-envelope.md §2.1: only 0x00 (elided) and 0x01 (present) are legal; anything else is rejected
            if (hasMinCompat != 0 && hasMinCompat != 1) return null;
            String domainVersionMinCompat = hasMinCompat == 1 ? BaboonBinTools.readString(reader) : domainVersion;
            String typeIdentifier = BaboonBinTools.readString(reader);

            return new BaboonTypeMeta(META_VERSION, domainIdentifier, domainVersion, domainVersionMinCompat, typeIdentifier);
        }

        // MFACADE-PR-3: accept `$mv` as either a JSON number or a string (back-compat with
        // M28-vintage fixtures); both must equal META_VERSION_1. Absent `$mv` falls through.
        public static BaboonTypeMeta readMeta(JsonNode json) {
            if (!json.isObject()) return null;
            ObjectNode obj = (ObjectNode) json;

            // MFACADE-PR-3: accept $mv as either a JSON number or a string (back-compat
            // with M28-vintage fixtures); both must equal META_VERSION_1.
            JsonNode mvNode = obj.get(META_VERSION_KEY);
            if (mvNode != null) {
                // Wire byte is unsigned 0..255 (per docs/spec/codec-envelope.md § 3); store as
                // signed `byte` for comparison with META_VERSION_1, which works bit-for-bit
                // because Java cast of int->byte preserves the low 8 bits.
                byte mv;
                if (mvNode.isIntegralNumber()) {
                    int n = mvNode.asInt();
                    if (n < 0 || n > 255) return null;
                    mv = (byte) n;
                } else if (mvNode.isTextual()) {
                    String mvStr = mvNode.asText();
                    try {
                        int n = Integer.parseInt(mvStr);
                        if (n < 0 || n > 255) return null;
                        mv = (byte) n;
                    } catch (NumberFormatException e) {
                        return null;
                    }
                } else {
                    return null;
                }
                if (mv != META_VERSION_1) return null;
            }

            JsonNode d = obj.get(DOMAIN_IDENTIFIER_KEY);
            JsonNode v = obj.get(DOMAIN_VERSION_KEY);
            JsonNode t = obj.get(TYPE_IDENTIFIER_KEY);
            if (d == null || !d.isTextual() || v == null || !v.isTextual() || t == null || !t.isTextual()) return null;

            JsonNode uvNode = obj.get(DOMAIN_VERSION_MIN_COMPAT_KEY);
            String uv = uvNode != null && uvNode.isTextual() ? uvNode.asText() : v.asText();
            JsonNode rvNode = obj.get(DOMAIN_VERSION_READABLE_KEY);
            String rv = rvNode != null && rvNode.isTextual() ? rvNode.asText() : uv;

            return new BaboonTypeMeta(META_VERSION, d.asText(), v.asText(), uv, t.asText(), rv);
        }
    }
}

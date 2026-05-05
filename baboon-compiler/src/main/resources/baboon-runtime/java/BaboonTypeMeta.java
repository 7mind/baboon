package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.List;

public record BaboonTypeMeta(
    byte metaVersion,
    String domainIdentifier,
    String domainVersion,
    String domainVersionMinCompat,
    String typeIdentifier
) {
    public BaboonDomainVersion versionRef() {
        return new BaboonDomainVersion(domainIdentifier, domainVersion);
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

        return new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            domainIdentifier,
            domainVersion,
            minCompat,
            typeIdentifier
        );
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

        public static final byte META_VERSION_1 = 16;
        public static final byte META_VERSION = META_VERSION_1;

        public static final String META_VERSION_KEY = "$mv";
        public static final String DOMAIN_IDENTIFIER_KEY = "$d";
        public static final String DOMAIN_VERSION_KEY = "$v";
        public static final String DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv";
        public static final String TYPE_IDENTIFIER_KEY = "$t";

        public static void writeBin(BaboonTypeMeta meta, LEDataOutputStream writer) throws Exception {
            writer.writeByte(META_VERSION);
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
            ObjectNode obj = JsonNodeFactory.instance.objectNode();
            obj.put(DOMAIN_IDENTIFIER_KEY, meta.domainIdentifier);
            obj.put(DOMAIN_VERSION_KEY, meta.domainVersion);
            obj.put(TYPE_IDENTIFIER_KEY, meta.typeIdentifier);
            if (!meta.domainVersion.equals(meta.domainVersionMinCompat)) {
                obj.put(DOMAIN_VERSION_MIN_COMPAT_KEY, meta.domainVersionMinCompat);
            }
            return obj;
        }

        public static BaboonTypeMeta readMeta(LEDataInputStream reader) throws Exception {
            byte metaVersion = reader.readByte();
            if (metaVersion != META_VERSION_1) return null;

            String domainIdentifier = BaboonBinTools.readString(reader);
            String domainVersion = BaboonBinTools.readString(reader);
            byte hasMinCompat = reader.readByte();
            String domainVersionMinCompat = hasMinCompat == 1 ? BaboonBinTools.readString(reader) : domainVersion;
            String typeIdentifier = BaboonBinTools.readString(reader);

            return new BaboonTypeMeta(META_VERSION, domainIdentifier, domainVersion, domainVersionMinCompat, typeIdentifier);
        }

        // Mirror Scala/C# (PR-08-D01): if `$mv` is present and not "1", reject; if absent, fall
        // through to v1 read.
        public static BaboonTypeMeta readMeta(JsonNode json) {
            if (!json.isObject()) return null;
            ObjectNode obj = (ObjectNode) json;

            JsonNode mvNode = obj.get(META_VERSION_KEY);
            if (mvNode != null) {
                if (!mvNode.isTextual()) return null;
                String mvStr = mvNode.asText();
                try {
                    byte mv = Byte.parseByte(mvStr);
                    if (mv != META_VERSION_1) return null;
                } catch (NumberFormatException e) {
                    return null;
                }
            }

            JsonNode d = obj.get(DOMAIN_IDENTIFIER_KEY);
            JsonNode v = obj.get(DOMAIN_VERSION_KEY);
            JsonNode t = obj.get(TYPE_IDENTIFIER_KEY);
            if (d == null || !d.isTextual() || v == null || !v.isTextual() || t == null || !t.isTextual()) return null;

            JsonNode uvNode = obj.get(DOMAIN_VERSION_MIN_COMPAT_KEY);
            String uv = uvNode != null && uvNode.isTextual() ? uvNode.asText() : v.asText();

            return new BaboonTypeMeta(META_VERSION, d.asText(), v.asText(), uv, t.asText());
        }
    }
}

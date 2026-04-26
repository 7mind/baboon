package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Arrays;
import java.util.Set;

/**
 * Container for the `any`-feature surface types: the `AnyMeta` envelope, the `AnyOpaque` ADT
 * (sealed: `AnyOpaqueUeba` for binary payloads, `AnyOpaqueJson` for JSON payloads), and the
 * `AnyMetaCodec` static helper. Mirrors C# `AnyOpaque.cs` and Kotlin `BaboonAnyOpaque.kt`.
 *
 * Java requires a single public top-level type per file, so we wrap the related declarations as
 * nested public static types under this container.
 */
public final class BaboonAnyOpaque {
    private BaboonAnyOpaque() {}

    /**
     * `AnyMeta` carries the locked four-byte/six-kind meta envelope for `any`-typed payloads.
     *
     * Invariants enforced at construction:
     *   - bit 2 (DOMAIN_BIT, 0x04) set ⇔ `domain != null`
     *   - bit 1 (VERSION_BIT, 0x02) set ⇔ `version != null`
     *   - bit 0 (TYPEID_BIT, 0x01) set ⇔ `typeid != null`
     *   - kind ∈ {0x00, 0x01, 0x02, 0x03, 0x06, 0x07} — 0x04/0x05 reserved (PR-04-D01)
     *
     * Construction with a reserved or mismatched kind throws `IllegalArgumentException`.
     */
    public record AnyMeta(byte kind, String domain, String version, String typeid) {
        public AnyMeta {
            boolean domainBitSet = (kind & AnyMetaCodec.DOMAIN_BIT) != 0;
            if (domainBitSet != (domain != null)) {
                throw new IllegalArgumentException(
                    "AnyMeta: domain presence (" + (domain != null) + ") does not match kind 0x" + Integer.toHexString(kind & 0xFF) + " bit 2");
            }
            boolean versionBitSet = (kind & AnyMetaCodec.VERSION_BIT) != 0;
            if (versionBitSet != (version != null)) {
                throw new IllegalArgumentException(
                    "AnyMeta: version presence (" + (version != null) + ") does not match kind 0x" + Integer.toHexString(kind & 0xFF) + " bit 1");
            }
            boolean typeidBitSet = (kind & AnyMetaCodec.TYPEID_BIT) != 0;
            if (typeidBitSet != (typeid != null)) {
                throw new IllegalArgumentException(
                    "AnyMeta: typeid presence (" + (typeid != null) + ") does not match kind 0x" + Integer.toHexString(kind & 0xFF) + " bit 0");
            }
            if (!AnyMetaCodec.VALID_KINDS.contains(kind)) {
                throw new IllegalArgumentException(
                    "AnyMeta: reserved or invalid meta-kind byte: 0x" + String.format("%02x", kind & 0xFF));
            }
        }
    }

    /**
     * `AnyOpaque` is the language-surface ADT for `any`-typed fields. Carries a meta envelope plus
     * a payload in either binary (UEBA) or JSON form. See spec §"Wire format" for the locked
     * meta-kind table.
     */
    public sealed interface AnyOpaque permits AnyOpaqueUeba, AnyOpaqueJson {
        AnyMeta meta();
    }

    /**
     * UEBA-formed payload. Hand-rolled `equals`/`hashCode` instead of a `record`: Java's record
     * auto-equals on a `byte[]` field uses `Object.equals` (reference identity), which defeats
     * round-trip equality. PR-05-D08 / PR-08-D06 lessons. Compare bytes by content via
     * `Arrays.equals`/`Arrays.hashCode`.
     */
    public static final class AnyOpaqueUeba implements AnyOpaque {
        private final AnyMeta meta;
        private final byte[] bytes;

        public AnyOpaqueUeba(AnyMeta meta, byte[] bytes) {
            this.meta = meta;
            this.bytes = bytes;
        }

        @Override public AnyMeta meta() { return meta; }
        public byte[] bytes() { return bytes; }

        @Override
        public boolean equals(Object other) {
            if (this == other) return true;
            if (!(other instanceof AnyOpaqueUeba that)) return false;
            return meta.equals(that.meta) && Arrays.equals(bytes, that.bytes);
        }

        @Override
        public int hashCode() {
            return 31 * meta.hashCode() + Arrays.hashCode(bytes);
        }

        @Override
        public String toString() {
            return "AnyOpaqueUeba(meta=" + meta + ", bytes=byte[" + bytes.length + "])";
        }
    }

    /**
     * JSON-formed payload. Jackson's `JsonNode.equals(Object)` is content-wise (object/array nodes
     * compare children recursively, primitive nodes compare values, NullNode is a singleton). The
     * record's auto-`equals` therefore yields content equality without manual override.
     */
    public record AnyOpaqueJson(AnyMeta meta, JsonNode json) implements AnyOpaque {
    }

    public static final class AnyMetaCodec {
        private AnyMetaCodec() {}

        public static final byte DOMAIN_BIT = 0x04;
        public static final byte VERSION_BIT = 0x02;
        public static final byte TYPEID_BIT = 0x01;

        public static final String ANY_KIND_KEY = "$ak";
        public static final String ANY_DOMAIN_KEY = "$ad";
        public static final String ANY_VERSION_KEY = "$av";
        public static final String ANY_TYPEID_KEY = "$at";
        public static final String ANY_CONTENT_KEY = "$c";

        public static final Set<Byte> VALID_KINDS = Set.of(
            (byte) 0x00, (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x06, (byte) 0x07
        );

        public static void writeBin(AnyMeta meta, LEDataOutputStream writer) throws Exception {
            writer.writeByte(meta.kind() & 0xFF);
            if (meta.domain() != null) BaboonBinTools.writeString(writer, meta.domain());
            if (meta.version() != null) BaboonBinTools.writeString(writer, meta.version());
            if (meta.typeid() != null) BaboonBinTools.writeString(writer, meta.typeid());
        }

        public static AnyMeta readBin(LEDataInputStream reader) throws Exception {
            byte kind = reader.readByte();
            String domain = (kind & DOMAIN_BIT) != 0 ? BaboonBinTools.readString(reader) : null;
            String version = (kind & VERSION_BIT) != 0 ? BaboonBinTools.readString(reader) : null;
            String typeid = (kind & TYPEID_BIT) != 0 ? BaboonBinTools.readString(reader) : null;
            return new AnyMeta(kind, domain, version, typeid);
        }

        /** Helper return type for {@link #readBinWithLength}. */
        public record MetaWithLength(AnyMeta meta, int bytesRead) {}

        /**
         * Reads meta and reports the number of bytes consumed. Callers that know the on-wire
         * `meta-length` window can skip any trailing bytes left in the window — that's how the
         * wire format keeps forward-compat with future meta extensions (PR-05-D01).
         */
        public static MetaWithLength readBinWithLength(LEDataInputStream reader) throws Exception {
            CountingInputStream counting = new CountingInputStream(reader);
            LEDataInputStream wrapped = new LEDataInputStream(counting);
            AnyMeta meta = readBin(wrapped);
            return new MetaWithLength(meta, counting.getCount());
        }

        /** Stream wrapper that counts bytes successfully read. Mirrors Kotlin's `CountingInputStream`. */
        private static final class CountingInputStream extends java.io.FilterInputStream {
            private int count = 0;

            CountingInputStream(java.io.InputStream in) { super(in); }

            int getCount() { return count; }

            @Override
            public int read() throws java.io.IOException {
                int b = in.read();
                if (b >= 0) count += 1;
                return b;
            }

            @Override
            public int read(byte[] b, int off, int len) throws java.io.IOException {
                int n = in.read(b, off, len);
                if (n > 0) count += n;
                return n;
            }
        }

        /**
         * Always returns an `ObjectNode`. The JSON encoder envelope build relies on this invariant
         * — adding `$c` content into a non-object would silently lose the key (PR-08-D06 analog).
         */
        public static JsonNode writeJson(AnyMeta meta) {
            ObjectNode obj = JsonNodeFactory.instance.objectNode();
            obj.set(ANY_KIND_KEY, IntNode.valueOf(meta.kind() & 0xFF));
            if (meta.domain() != null) obj.put(ANY_DOMAIN_KEY, meta.domain());
            if (meta.version() != null) obj.put(ANY_VERSION_KEY, meta.version());
            if (meta.typeid() != null) obj.put(ANY_TYPEID_KEY, meta.typeid());
            return obj;
        }

        /**
         * Returns `BaboonEither` to mirror PR-04-D02: binary decode trusts the wire and throws on
         * bad input; JSON decode is user-facing and threads errors as `BaboonEither`.
         */
        public static BaboonEither<BaboonCodecException, AnyMeta> readJson(JsonNode json) {
            if (!(json instanceof ObjectNode obj)) {
                return BaboonEither.left(
                    new BaboonCodecException.DecoderFailure(
                        "AnyMetaCodec.readJson: expected JSON object, got " + json.getNodeType()));
            }

            JsonNode kindNode = obj.get(ANY_KIND_KEY);
            if (kindNode == null || !kindNode.isIntegralNumber()) {
                return BaboonEither.left(
                    new BaboonCodecException.DecoderFailure(
                        "AnyMetaCodec.readJson: missing or non-numeric '" + ANY_KIND_KEY + "' field"));
            }
            byte kind = (byte) kindNode.intValue();

            BaboonEither<BaboonCodecException, String> domainResult = readOptString(obj, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain");
            if (domainResult instanceof BaboonEither.Left<BaboonCodecException, String> dl) {
                return BaboonEither.left(dl.value());
            }
            String domain = ((BaboonEither.Right<BaboonCodecException, String>) domainResult).value();

            BaboonEither<BaboonCodecException, String> versionResult = readOptString(obj, ANY_VERSION_KEY, kind, VERSION_BIT, "version");
            if (versionResult instanceof BaboonEither.Left<BaboonCodecException, String> vl) {
                return BaboonEither.left(vl.value());
            }
            String version = ((BaboonEither.Right<BaboonCodecException, String>) versionResult).value();

            BaboonEither<BaboonCodecException, String> typeidResult = readOptString(obj, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid");
            if (typeidResult instanceof BaboonEither.Left<BaboonCodecException, String> tl) {
                return BaboonEither.left(tl.value());
            }
            String typeid = ((BaboonEither.Right<BaboonCodecException, String>) typeidResult).value();

            try {
                return BaboonEither.right(new AnyMeta(kind, domain, version, typeid));
            } catch (IllegalArgumentException e) {
                return BaboonEither.left(
                    new BaboonCodecException.DecoderFailure(
                        "AnyMetaCodec.readJson: invalid meta: " + e.getMessage()));
            }
        }

        private static BaboonEither<BaboonCodecException, String> readOptString(
            ObjectNode obj, String key, byte kind, byte bit, String name) {
            boolean present = (kind & bit) != 0;
            JsonNode token = obj.get(key);
            String value = (token != null && token.isTextual()) ? token.asText() : null;

            if (present && value != null) return BaboonEither.right(value);
            if (!present && value == null) return BaboonEither.right(null);
            if (present) {
                return BaboonEither.left(
                    new BaboonCodecException.DecoderFailure(
                        "AnyMetaCodec.readJson: kind 0x" + Integer.toHexString(kind & 0xFF) + " requires '" + key + "' (" + name + ") but it is missing"));
            }
            return BaboonEither.left(
                new BaboonCodecException.DecoderFailure(
                    "AnyMetaCodec.readJson: kind 0x" + Integer.toHexString(kind & 0xFF) + " forbids '" + key + "' (" + name + ") but it is present"));
        }
    }
}

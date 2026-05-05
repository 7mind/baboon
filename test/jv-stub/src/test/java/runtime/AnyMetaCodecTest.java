package runtime;

import baboon.runtime.shared.AbstractBaboonConversions;
import baboon.runtime.shared.AbstractBaboonJsonCodecs;
import baboon.runtime.shared.AbstractBaboonUebaCodecs;
import baboon.runtime.shared.BaboonAnyOpaque;
import baboon.runtime.shared.BaboonAnyOpaque.AnyMeta;
import baboon.runtime.shared.BaboonAnyOpaque.AnyMetaCodec;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueJson;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueUeba;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecException;
import baboon.runtime.shared.BaboonCodecsFacade;
import baboon.runtime.shared.BaboonDomainVersion;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.BaboonMeta;
import baboon.runtime.shared.BaboonTypeMeta;
import baboon.runtime.shared.LEDataInputStream;
import baboon.runtime.shared.LEDataOutputStream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AnyMetaCodecTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final JsonNodeFactory NF = JsonNodeFactory.instance;

    private static byte[] writeBin(AnyMeta meta) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try (LEDataOutputStream out = new LEDataOutputStream(baos)) {
            AnyMetaCodec.writeBin(meta, out);
        }
        return baos.toByteArray();
    }

    private static AnyMeta readBin(byte[] bytes) throws Exception {
        try (LEDataInputStream in = new LEDataInputStream(new ByteArrayInputStream(bytes))) {
            return AnyMetaCodec.readBin(in);
        }
    }

    private static AnyMetaCodec.MetaWithLength readBinWithLength(byte[] bytes) throws Exception {
        try (LEDataInputStream in = new LEDataInputStream(new ByteArrayInputStream(bytes))) {
            return AnyMetaCodec.readBinWithLength(in);
        }
    }

    // ===== AnyMeta invariants ===================================================================

    @Test
    void invariant_rejectsBitMismatchDomain() {
        // kind 0x07 sets domain bit but domain is null
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x07, null, "1.0.0", "T"));
    }

    @Test
    void invariant_rejectsBitMismatchVersion() {
        // kind 0x03 sets version bit but version is null
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x03, null, null, "T"));
    }

    @Test
    void invariant_rejectsBitMismatchTypeid() {
        // kind 0x01 sets typeid bit but typeid is null
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x01, null, null, null));
    }

    @Test
    void invariant_rejectsExtraDomainWithoutBit() {
        // kind 0x00, domain set
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x00, "d", null, null));
    }

    @Test
    void invariant_rejectsReservedKind0x04() {
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x04, "d", null, null));
    }

    @Test
    void invariant_rejectsReservedKind0x05() {
        assertThrows(IllegalArgumentException.class, () -> new AnyMeta((byte) 0x05, "d", null, "T"));
    }

    // ===== Binary round-trip per kind ===========================================================

    @Test
    void binRoundTrip_kindA_0x07() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom", "1.0.0", "Type");
        AnyMeta result = readBin(writeBin(meta));
        assertEquals(meta, result);
    }

    @Test
    void binRoundTrip_kindB_0x03() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x03, null, "1.0.0", "Type");
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_kindC_0x01() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x01, null, null, "Type");
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_kindD1_0x06() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x06, "dom", "1.0.0", null);
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_kindD2_0x02() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x02, null, "1.0.0", null);
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_kindD3_0x00() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        assertEquals(meta, readBin(writeBin(meta)));
    }

    // ===== Binary round-trip edge cases =========================================================

    @Test
    void binRoundTrip_emptyStrings() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x07, "", "", "");
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_nonAsciiUtf8() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x07, "δωμάιν", "1.0.0", "τύπος");
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void binRoundTrip_128byteString() throws Exception {
        // Triggers multi-byte ULEB128 length.
        String big = "x".repeat(128);
        AnyMeta meta = new AnyMeta((byte) 0x07, big, "1.0.0", "T");
        assertEquals(meta, readBin(writeBin(meta)));
    }

    @Test
    void readBinWithLength_reportsBytesConsumed() throws Exception {
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom", "1.0.0", "Type");
        byte[] baseBytes = writeBin(meta);
        // Append 5 padding bytes — caller knows the meta-length window
        ByteArrayOutputStream padded = new ByteArrayOutputStream();
        padded.write(baseBytes);
        padded.write(new byte[]{(byte) 0xAA, (byte) 0xBB, (byte) 0xCC, (byte) 0xDD, (byte) 0xEE});

        AnyMetaCodec.MetaWithLength out = readBinWithLength(padded.toByteArray());
        assertEquals(meta, out.meta());
        assertEquals(baseBytes.length, out.bytesRead());
    }

    // ===== JSON round-trip ======================================================================

    @Test
    void jsonRoundTrip_kindA() {
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom", "1.0.0", "T");
        JsonNode wire = AnyMetaCodec.writeJson(meta);
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(wire);
        assertInstanceOf(BaboonEither.Right.class, result);
        assertEquals(meta, ((BaboonEither.Right<BaboonCodecException, AnyMeta>) result).value());
    }

    @Test
    void jsonRoundTrip_allKinds() {
        AnyMeta[] all = {
            new AnyMeta((byte) 0x07, "d", "1.0.0", "T"),
            new AnyMeta((byte) 0x03, null, "1.0.0", "T"),
            new AnyMeta((byte) 0x01, null, null, "T"),
            new AnyMeta((byte) 0x06, "d", "1.0.0", null),
            new AnyMeta((byte) 0x02, null, "1.0.0", null),
            new AnyMeta((byte) 0x00, null, null, null),
        };
        for (AnyMeta meta : all) {
            JsonNode wire = AnyMetaCodec.writeJson(meta);
            BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(wire);
            assertInstanceOf(BaboonEither.Right.class, result, "kind 0x" + Integer.toHexString(meta.kind() & 0xFF));
            assertEquals(meta, ((BaboonEither.Right<BaboonCodecException, AnyMeta>) result).value());
        }
    }

    @Test
    void readJson_leftOnNonObject() {
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(NF.textNode("not-an-obj"));
        assertInstanceOf(BaboonEither.Left.class, result);
    }

    @Test
    void readJson_leftOnMissingKind() {
        ObjectNode obj = NF.objectNode();
        obj.put(AnyMetaCodec.ANY_DOMAIN_KEY, "dom");
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(obj);
        assertInstanceOf(BaboonEither.Left.class, result);
    }

    @Test
    void readJson_leftOnNonNumericKind() {
        ObjectNode obj = NF.objectNode();
        obj.put(AnyMetaCodec.ANY_KIND_KEY, "seven");
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(obj);
        assertInstanceOf(BaboonEither.Left.class, result);
    }

    @Test
    void readJson_leftOnIncompleteForKind() {
        // kind 0x07 expects domain/version/typeid but only typeid present
        ObjectNode obj = NF.objectNode();
        obj.set(AnyMetaCodec.ANY_KIND_KEY, IntNode.valueOf(0x07));
        obj.put(AnyMetaCodec.ANY_TYPEID_KEY, "T");
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(obj);
        assertInstanceOf(BaboonEither.Left.class, result);
    }

    @Test
    void readJson_leftOnForbiddenForKind() {
        // kind 0x00 forbids domain/version/typeid but domain present
        ObjectNode obj = NF.objectNode();
        obj.set(AnyMetaCodec.ANY_KIND_KEY, IntNode.valueOf(0x00));
        obj.put(AnyMetaCodec.ANY_DOMAIN_KEY, "dom");
        BaboonEither<BaboonCodecException, AnyMeta> result = AnyMetaCodec.readJson(obj);
        assertInstanceOf(BaboonEither.Left.class, result);
    }

    @Test
    void writeJson_alwaysReturnsObject() {
        JsonNode wire = AnyMetaCodec.writeJson(new AnyMeta((byte) 0x00, null, null, null));
        assertTrue(wire.isObject());
    }

    // ===== AnyOpaque content equality ===========================================================

    @Test
    void anyOpaqueUeba_equalsByContent() {
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        byte[] a = {1, 2, 3};
        byte[] b = {1, 2, 3};
        AnyOpaqueUeba x = new AnyOpaqueUeba(meta, a);
        AnyOpaqueUeba y = new AnyOpaqueUeba(meta, b);
        assertEquals(x, y);
        assertEquals(x.hashCode(), y.hashCode());

        AnyOpaqueUeba z = new AnyOpaqueUeba(meta, new byte[]{1, 2, 4});
        assertNotEquals(x, z);
    }

    @Test
    void anyOpaqueJson_equalsByContent() {
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        ObjectNode a = NF.objectNode();
        a.put("x", 1);
        ObjectNode b = NF.objectNode();
        b.put("x", 1);
        AnyOpaqueJson x = new AnyOpaqueJson(meta, a);
        AnyOpaqueJson y = new AnyOpaqueJson(meta, b);
        assertNotSame(a, b); // sanity — different instances
        assertEquals(x, y);
        assertEquals(x.hashCode(), y.hashCode());

        ObjectNode c = NF.objectNode();
        c.put("x", 2);
        AnyOpaqueJson z = new AnyOpaqueJson(meta, c);
        assertNotEquals(x, z);
    }

    private static void assertNotSame(Object a, Object b) {
        assertFalse(a == b);
    }

    // ===== BaboonCodecContext withFacade ========================================================

    @Test
    void codecContext_compactHasNullFacade() {
        assertNull(BaboonCodecContext.Compact.facade());
        assertFalse(BaboonCodecContext.Compact.useIndices());
    }

    @Test
    void codecContext_indexedHasNullFacade() {
        assertNull(BaboonCodecContext.Indexed.facade());
        assertTrue(BaboonCodecContext.Indexed.useIndices());
    }

    @Test
    void codecContext_withFacadeExposesFacade() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        BaboonCodecContext ctx = BaboonCodecContext.withFacade(true, facade);
        assertSame(facade, ctx.facade());
        assertTrue(ctx.useIndices());
    }

    // ===== Cross-format helpers (no codecs registered) ==========================================

    @Test
    void jsonToUebaBytes_leftOnIncompleteMeta() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        BaboonEither<BaboonCodecException, byte[]> r = facade.jsonToUebaBytes(meta, NF.objectNode());
        assertInstanceOf(BaboonEither.Left.class, r);
    }

    @Test
    void uebaToJson_leftOnIncompleteMeta() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        BaboonEither<BaboonCodecException, JsonNode> r = facade.uebaToJson(meta, new byte[]{1, 2, 3});
        assertInstanceOf(BaboonEither.Left.class, r);
    }

    @Test
    void jsonToUebaBytes_leftOnNoCodec() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom", "1.0.0", "T");
        BaboonEither<BaboonCodecException, byte[]> r = facade.jsonToUebaBytes(meta, NF.objectNode());
        assertInstanceOf(BaboonEither.Left.class, r);
        BaboonCodecException e = ((BaboonEither.Left<BaboonCodecException, byte[]>) r).value();
        assertInstanceOf(BaboonCodecException.CodecNotFound.class, e);
    }

    @Test
    void uebaToJson_leftOnNoCodec() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom", "1.0.0", "T");
        BaboonEither<BaboonCodecException, JsonNode> r = facade.uebaToJson(meta, new byte[]{1, 2, 3});
        assertInstanceOf(BaboonEither.Left.class, r);
        BaboonCodecException e = ((BaboonEither.Left<BaboonCodecException, JsonNode>) r).value();
        assertInstanceOf(BaboonCodecException.CodecNotFound.class, e);
    }

    @Test
    void decodeAny_leftOnIncompleteMeta() {
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        AnyOpaqueUeba opaque = new AnyOpaqueUeba(meta, new byte[]{0});
        BaboonEither<BaboonCodecException, ?> r = facade.decodeAny(opaque);
        assertInstanceOf(BaboonEither.Left.class, r);
    }

    // ===== Static-fallback semantics (PR-06-D01) ================================================

    @Test
    void staticFallback_kindD3_filledByStatics() {
        // kind 0x00 carries no meta fields; statics must fill all three. Lookup will fail (no codec
        // registered) but it should fail with CodecNotFound, not "AnyMeta requires ...".
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x00, null, null, null);
        BaboonEither<BaboonCodecException, byte[]> r = facade.jsonToUebaBytes(meta, NF.objectNode(), "dom", "1.0.0", "T");
        assertInstanceOf(BaboonEither.Left.class, r);
        BaboonCodecException e = ((BaboonEither.Left<BaboonCodecException, byte[]>) r).value();
        assertInstanceOf(BaboonCodecException.CodecNotFound.class, e);
    }

    @Test
    void staticFallback_overridesByWire() {
        // Wire meta domain "dom-wire" must override the static "dom-static". Lookup still fails
        // (no codec registered) but the failure must mention the wire-derived domain.
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        AnyMeta meta = new AnyMeta((byte) 0x07, "dom-wire", "1.0.0", "T");
        BaboonEither<BaboonCodecException, byte[]> r = facade.jsonToUebaBytes(meta, NF.objectNode(), "dom-static", "0.9.0", "Other");
        assertInstanceOf(BaboonEither.Left.class, r);
        assertTrue(((BaboonEither.Left<BaboonCodecException, byte[]>) r).value().getMessage().contains("dom-wire"));
    }

    // ===== Single-version-domain regression (PR-07-D02) =========================================

    @Test
    void getCodec_singleVersionDomain_routesToExact() {
        // Register a single-version domain with a stub UEBA codec for type "T".
        BaboonCodecsFacade facade = new BaboonCodecsFacade();
        BaboonDomainVersion dv = new BaboonDomainVersion("dom", "1.0.0");
        StubBin binCodecs = new StubBin();
        binCodecs.add("T", new StubBinCodec());
        StubJson jsonCodecs = new StubJson();
        jsonCodecs.add("T", new StubJsonCodec());
        StubMeta meta = new StubMeta(List.of("1.0.0"));
        StubConv conv = new StubConv();

        facade.register(dv, () -> jsonCodecs, () -> binCodecs, () -> conv, () -> meta);

        // Non-exact lookup at version 1.0.0 must succeed (PR-07-D02 fix).
        AnyMeta any = new AnyMeta((byte) 0x07, "dom", "1.0.0", "T");
        byte[] payload = new byte[]{0x42};
        BaboonEither<BaboonCodecException, JsonNode> r = facade.uebaToJson(any, payload);
        assertInstanceOf(BaboonEither.Right.class, r);
    }

    // ===== BaboonTypeMeta JSON $mv handling (PR-08-D01) =========================================

    @Test
    void typeMetaReadJson_acceptsAbsentMv() {
        ObjectNode obj = NF.objectNode();
        obj.put("$d", "dom");
        obj.put("$v", "1.0.0");
        obj.put("$t", "T");
        BaboonTypeMeta meta = BaboonTypeMeta.readMeta(obj);
        assertNotNull(meta);
        assertEquals("dom", meta.domainIdentifier());
    }

    @Test
    void typeMetaReadJson_acceptsExplicitMv1() {
        ObjectNode obj = NF.objectNode();
        obj.put("$mv", "16");
        obj.put("$d", "dom");
        obj.put("$v", "1.0.0");
        obj.put("$t", "T");
        BaboonTypeMeta meta = BaboonTypeMeta.readMeta(obj);
        assertNotNull(meta);
    }

    @Test
    void typeMetaReadJson_rejectsMv2() {
        ObjectNode obj = NF.objectNode();
        obj.put("$mv", "2");
        obj.put("$d", "dom");
        obj.put("$v", "1.0.0");
        obj.put("$t", "T");
        BaboonTypeMeta meta = BaboonTypeMeta.readMeta(obj);
        assertNull(meta);
    }

    // ===== JsonNode content equality (Jackson sanity / PR-08-D06) ==============================

    @Test
    void jsonNode_equalsIsContentWise() {
        ObjectNode a = NF.objectNode();
        a.put("x", 1);
        a.put("y", "z");
        ObjectNode b = NF.objectNode();
        b.put("x", 1);
        b.put("y", "z");
        assertEquals(a, b); // Jackson invariant — record auto-equals in AnyOpaqueJson relies on this
    }

    // ===== Test stubs ===========================================================================

    private static final class StubBinCodec implements baboon.runtime.shared.BaboonBinCodec<baboon.runtime.shared.BaboonGenerated> {
        @Override
        public void encode(BaboonCodecContext ctx, LEDataOutputStream output, baboon.runtime.shared.BaboonGenerated value) throws Exception {
            output.writeByte(0x42);
        }
        @Override
        public baboon.runtime.shared.BaboonGenerated decode(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            input.readByte();
            return new baboon.runtime.shared.BaboonGenerated() {};
        }
    }

    private static final class StubJsonCodec implements baboon.runtime.shared.BaboonJsonCodec<baboon.runtime.shared.BaboonGenerated> {
        @Override
        public JsonNode encode(BaboonCodecContext ctx, baboon.runtime.shared.BaboonGenerated value) {
            return NF.textNode("ok");
        }
        @Override
        public baboon.runtime.shared.BaboonGenerated decode(BaboonCodecContext ctx, JsonNode wire) {
            return new baboon.runtime.shared.BaboonGenerated() {};
        }
    }

    private static final class StubBin extends AbstractBaboonUebaCodecs {
        void add(String typeId, baboon.runtime.shared.BaboonBinCodec<?> codec) {
            register(typeId, baboon.runtime.shared.Lazy.of(() -> codec));
        }
    }

    private static final class StubJson extends AbstractBaboonJsonCodecs {
        void add(String typeId, baboon.runtime.shared.BaboonJsonCodec<?> codec) {
            register(typeId, baboon.runtime.shared.Lazy.of(() -> codec));
        }
    }

    private static final class StubMeta implements BaboonMeta {
        private final List<String> versions;
        StubMeta(List<String> v) { this.versions = v; }
        @Override
        public List<String> sameInVersions(String typeId) { return versions; }
    }

    private static final class StubConv extends AbstractBaboonConversions {
        @Override
        public List<String> versionsFrom() { return List.of(); }
        @Override
        public String versionTo() { return "1.0.0"; }
    }
}

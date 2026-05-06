// NOTE: This test references generated symbols (my.ok.*) that are only present after
// `mdl :build :test-gen-regular-adt` populates target/test-regular/jv-stub/. Running
// `mvn test` directly from the source tree will fail with missing symbols.
//
// Tests for BaboonCodecsFacade.preload(), decodeFromBinLatest(), and decodeFromJsonLatest()
// (MFACADE-PR-4). Mirrors the patterns established in AnyRoundTripTest using the my.ok
// fixture domain.
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecException;
import baboon.runtime.shared.BaboonCodecsFacade;
import baboon.runtime.shared.BaboonDomainVersion;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.LEDataInputStream;

import com.fasterxml.jackson.databind.JsonNode;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;

class FacadeDecodeLatestTest {

    private static BaboonCodecsFacade freshFacade() {
        BaboonCodecsFacade f = new BaboonCodecsFacade();
        BaboonDomainVersion dv = new BaboonDomainVersion(my.ok.Inner.baboonDomainIdentifier, my.ok.Inner.baboonDomainVersion);
        f.register(
            dv,
            () -> new my.ok.BaboonCodecsJson(),
            () -> new my.ok.BaboonCodecsUeba(),
            () -> new my.ok.BaboonConversions(),
            () -> new my.ok.BaboonMetadata());
        return f;
    }

    private static byte[] encodeInnerToBin(my.ok.Inner inner) throws Exception {
        BaboonCodecsFacade facade = freshFacade();
        BaboonEither<BaboonCodecException, byte[]> r = facade.encodeToBin(BaboonCodecContext.Compact, inner);
        assertInstanceOf(BaboonEither.Right.class, r, "encodeToBin must succeed");
        return ((BaboonEither.Right<BaboonCodecException, byte[]>) r).value();
    }

    private static JsonNode encodeInnerToJson(my.ok.Inner inner) {
        BaboonCodecsFacade facade = freshFacade();
        BaboonEither<BaboonCodecException, JsonNode> r = facade.encodeToJson(inner);
        assertInstanceOf(BaboonEither.Right.class, r, "encodeToJson must succeed");
        return ((BaboonEither.Right<BaboonCodecException, JsonNode>) r).value();
    }

    private static final my.ok.Inner SAMPLE = new my.ok.Inner(99);

    // ===== preload =====

    @Test
    void preload_completesWithoutException() throws Exception {
        BaboonCodecsFacade facade = freshFacade();
        // preload() is fire-and-forget; we just assert it does not throw synchronously
        // and returns before any subsequent call fails.
        facade.preload();
        // Brief cooperative yield so the async task can complete before we use the facade.
        Thread.sleep(200);
        // Subsequent encode must still succeed (registries intact after preload).
        BaboonEither<BaboonCodecException, byte[]> r = facade.encodeToBin(BaboonCodecContext.Compact, SAMPLE);
        assertInstanceOf(BaboonEither.Right.class, r, "encode after preload must succeed");
    }

    // ===== decodeFromBinLatest (bytes overload) =====

    @Test
    void decodeFromBinLatest_bytes_roundTripsInner() throws Exception {
        BaboonCodecsFacade facade = freshFacade();
        byte[] encoded = encodeInnerToBin(SAMPLE);

        BaboonEither<BaboonCodecException, my.ok.Inner> result =
            facade.decodeFromBinLatest(encoded, my.ok.Inner.class);

        assertInstanceOf(BaboonEither.Right.class, result, "decodeFromBinLatest must be Right: " + result);
        my.ok.Inner decoded = ((BaboonEither.Right<BaboonCodecException, my.ok.Inner>) result).value();
        assertEquals(SAMPLE, decoded);
    }

    // ===== decodeFromJsonLatest (JsonNode overload) =====

    @Test
    void decodeFromJsonLatest_jsonNode_roundTripsInner() {
        BaboonCodecsFacade facade = freshFacade();
        JsonNode encoded = encodeInnerToJson(SAMPLE);

        BaboonEither<BaboonCodecException, my.ok.Inner> result =
            facade.decodeFromJsonLatest(encoded, my.ok.Inner.class);

        assertInstanceOf(BaboonEither.Right.class, result, "decodeFromJsonLatest must be Right: " + result);
        my.ok.Inner decoded = ((BaboonEither.Right<BaboonCodecException, my.ok.Inner>) result).value();
        assertEquals(SAMPLE, decoded);
    }

    // ===== decodeFromJsonLatest (String overload) =====

    @Test
    void decodeFromJsonLatest_string_roundTripsInner() throws Exception {
        BaboonCodecsFacade facade = freshFacade();
        JsonNode encoded = encodeInnerToJson(SAMPLE);
        String jsonStr = encoded.toString();

        BaboonEither<BaboonCodecException, my.ok.Inner> result =
            facade.decodeFromJsonLatest(jsonStr, my.ok.Inner.class);

        assertInstanceOf(BaboonEither.Right.class, result, "decodeFromJsonLatest(String) must be Right: " + result);
        my.ok.Inner decoded = ((BaboonEither.Right<BaboonCodecException, my.ok.Inner>) result).value();
        assertEquals(SAMPLE, decoded);
    }

    // ===== null-envelope contract (absent $c field) =====

    @Test
    void decodeFromJsonLatest_absentEnvelope_returnsRightNull() {
        BaboonCodecsFacade facade = freshFacade();
        // An empty JSON object has no type-meta keys — decodeFromJson returns Right(null),
        // and decodeFromJsonLatest must propagate that as Right(null).
        com.fasterxml.jackson.databind.node.ObjectNode empty =
            com.fasterxml.jackson.databind.node.JsonNodeFactory.instance.objectNode();

        BaboonEither<BaboonCodecException, my.ok.Inner> result =
            facade.decodeFromJsonLatest(empty, my.ok.Inner.class);

        assertInstanceOf(BaboonEither.Right.class, result);
        assertNull(((BaboonEither.Right<BaboonCodecException, my.ok.Inner>) result).value());
    }
}

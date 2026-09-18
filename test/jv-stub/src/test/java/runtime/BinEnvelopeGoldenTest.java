// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecException;
import baboon.runtime.shared.BaboonCodecsFacade;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.BaboonGenerated;
import fwde2e.chain.ChainAppend;
import fwde2e.chain.DomainFwde2eChainFacade;
import fwde2e.fwd.DomainFwde2eFwdFacade;
import fwde2e.fwd.FwdAppendVar;
import fwde2e.fwd.FwdEnumGrows;
import fwde2e.fwd.FwdEnumHost;
import fwde2e.fwd.FwdStable;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

class BinEnvelopeGoldenTest {
    private static String hx(byte[] b) {
        return IntStream.range(0, b.length).mapToObj(i -> String.format("%02X", b[i] & 0xFF)).collect(Collectors.joining(" "));
    }

    private static byte[] enc(BaboonCodecsFacade f, BaboonCodecContext ctx, BaboonGenerated v) {
        BaboonEither<BaboonCodecException, byte[]> encoded = f.encodeToBin(ctx, v);
        assertInstanceOf(BaboonEither.Right.class, encoded, "encodeToBin must succeed");
        return ((BaboonEither.Right<BaboonCodecException, byte[]>) encoded).value();
    }

    private static final BaboonCodecContext V1_TOLERANT = BaboonCodecContext.custom(false, BaboonCodecContext.ForwardWritePolicy.TOLERANT, BaboonCodecContext.BaboonEnvelopeVersion.V1, null);
    private static final BaboonCodecContext V2_COMPACT = BaboonCodecContext.custom(false, BaboonCodecContext.ForwardWritePolicy.STRICT, BaboonCodecContext.BaboonEnvelopeVersion.V2, null);
    private static final BaboonCodecContext V2_INDEXED = BaboonCodecContext.custom(true, BaboonCodecContext.ForwardWritePolicy.STRICT, BaboonCodecContext.BaboonEnvelopeVersion.V2, null);

    @Test
    void defaultContextsWriteV1Strict() {
        assertEquals(BaboonCodecContext.BaboonEnvelopeVersion.V1, BaboonCodecContext.Compact.envelopeVersion());
        assertEquals(BaboonCodecContext.ForwardWritePolicy.STRICT, BaboonCodecContext.Compact.forwardWritePolicy());
        assertEquals(BaboonCodecContext.BaboonEnvelopeVersion.V1, BaboonCodecContext.Indexed.envelopeVersion());
    }

    @Test
    void envelopesMatchTheCrossLanguageGoldenBytes() {
        DomainFwde2eFwdFacade fwd = new DomainFwde2eFwdFacade();
        DomainFwde2eChainFacade chain = new DomainFwde2eChainFacade();
        FwdAppendVar app = new FwdAppendVar(42, "hi", Optional.of("t"));
        // FwdAppendVar, v1 Strict (default) compact: identical bound elided
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, BaboonCodecContext.Compact, app)));
        // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, V1_TOLERANT, app)));
        // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, V2_COMPACT, app)));
        // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, V2_INDEXED, app)));
        // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(enc(fwd, BaboonCodecContext.Compact, new FwdStable("s"))));
        // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(enc(fwd, V2_COMPACT, new FwdStable("s"))));
        // FwdEnumHost, v2 compact: flags 0, no bound
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02", hx(enc(fwd, V2_COMPACT, new FwdEnumHost(FwdEnumGrows.C))));
        // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
        assertEquals("02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63", hx(enc(chain, V2_COMPACT, new ChainAppend(1, Optional.of("b"), Optional.of("c")))));
    }

    @Test
    void v2EnvelopeRoundTripsThroughItsOwnFacade() {
        DomainFwde2eFwdFacade fwd = new DomainFwde2eFwdFacade();
        FwdAppendVar app = new FwdAppendVar(42, "hi", Optional.of("t"));
        BaboonEither<BaboonCodecException, BaboonGenerated> decoded = fwd.decodeFromBin(enc(fwd, V2_COMPACT, app));
        assertInstanceOf(BaboonEither.Right.class, decoded, "decodeFromBin must succeed");
        assertEquals(app, ((BaboonEither.Right<BaboonCodecException, BaboonGenerated>) decoded).value());
    }
}

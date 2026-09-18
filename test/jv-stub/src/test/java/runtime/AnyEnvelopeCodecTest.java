package runtime;

import baboon.runtime.shared.*;
import baboon.runtime.shared.BaboonAnyOpaque.*;
import com.fasterxml.jackson.databind.node.NullNode;
import org.junit.jupiter.api.Test;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import static org.junit.jupiter.api.Assertions.*;

public class AnyEnvelopeCodecTest {
    @Test
    public void nativeFormatsRoundTripAllKindsAndLeaveTheTrailer() throws Exception {
        for (byte kind : new byte[] {0, 1, 2, 3, 6, 7}) {
            var meta = new AnyMeta(kind, (kind & 4) != 0 ? "domain" : null, (kind & 2) != 0 ? "1.0.0" : null, (kind & 1) != 0 ? "type" : null);
            var json = new AnyOpaqueJson(meta, NullNode.getInstance());
            assertEquals(json, BaboonAnyJsonCodec.decode(kind, BaboonAnyJsonCodec.encode(BaboonCodecContext.Compact, kind, null, null, null, json)));
            var binary = new AnyOpaqueUeba(meta, new byte[] {(byte) 0xaa, (byte) 0xbb});
            var buffer = new ByteArrayOutputStream();
            var writer = new LEDataOutputStream(buffer);
            BaboonAnyBinCodec.encode(BaboonCodecContext.Compact, writer, kind, null, null, null, binary);
            writer.writeByte(42);
            if (kind == 0) assertArrayEquals(new byte[] {7,0,0,0,1,0,0,0,0,(byte) 0xaa,(byte) 0xbb,42}, buffer.toByteArray());
            var reader = new LEDataInputStream(new ByteArrayInputStream(buffer.toByteArray()));
            assertEquals(binary, BaboonAnyBinCodec.decode(reader, kind));
            assertEquals(42, reader.readByte());
        }
    }

    @Test
    public void consumesMetadataExtensionsAndRejectsTruncatedPayloads() throws Exception {
        var extended = new LEDataInputStream(new ByteArrayInputStream(new byte[] {8,0,0,0,3,0,0,0,0,17,34,51,42}));
        assertArrayEquals(new byte[] {51}, BaboonAnyBinCodec.decode(extended, (byte) 0).bytes());
        assertEquals(42, extended.readByte());
        var truncated = new LEDataInputStream(new ByteArrayInputStream(new byte[] {8,0,0,0,3,0,0,0,0,17}));
        var failure = assertThrows(BaboonCodecException.DecoderFailure.class, () -> BaboonAnyBinCodec.decode(truncated, (byte) 0));
        assertTrue(failure.getMessage().contains("short read while skipping meta-extension bytes"));
    }
}

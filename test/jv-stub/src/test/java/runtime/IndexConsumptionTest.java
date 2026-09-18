package runtime;

import baboon.runtime.shared.BaboonBinCodec;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.LEDataInputStream;
import baboon.runtime.shared.LEDataOutputStream;
import org.junit.jupiter.api.Test;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import static org.junit.jupiter.api.Assertions.*;

public class IndexConsumptionTest {
    @Test
    public void rejectsInvalidIndexEntriesWithoutAssertions() throws Exception {
        int[][] cases = {{0, 0, 1, 1}, {-1, 1, 1, 1}, {0, -1, 1, 1}, {0, 2, 1, 1}, {Integer.MAX_VALUE, 1, 0, 1}};
        for (int[] entries : cases) {
            var buffer = new ByteArrayOutputStream();
            var writer = new LEDataOutputStream(buffer);
            writer.writeByte(1);
            for (int value : entries) writer.writeInt(value);
            var bytes = buffer.toByteArray();
            assertThrows(IllegalArgumentException.class, () -> BaboonBinCodec.readIndex(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(bytes)), 2));
            assertThrows(IllegalArgumentException.class, () -> BaboonBinCodec.consumeIndex(BaboonCodecContext.Compact, new LEDataInputStream(new ByteArrayInputStream(bytes)), 2));
        }
    }

    @Test
    public void streamCopyOverloadsAreObservableToPublicSubclasses() throws Exception {
        class ObservingOutput extends LEDataOutputStream {
            int wholeWrites;
            int sliceWrites;
            ObservingOutput() { super(new ByteArrayOutputStream()); }
            @Override public void write(byte[] bytes) { wholeWrites++; }
            @Override public void write(byte[] bytes, int offset, int length) { sliceWrites++; }
        }
        var buffer = new ByteArrayOutputStream();
        buffer.write(42);
        var output = new ObservingOutput();
        output.write(buffer.toByteArray());
        assertEquals(1, output.wholeWrites);
        assertEquals(0, output.sliceWrites);
        buffer.writeTo(output);
        assertEquals(1, output.wholeWrites);
        assertEquals(1, output.sliceWrites);
    }

    @Test
    public void materializationConsumesOnlyHeaderAndExpectedPairs() throws Exception {
        byte[] indexed = {1, 0,0,0,0, 2,0,0,0, 2,0,0,0, 3,0,0,0, 42};
        for (byte[] bytes : new byte[][] {indexed, {0, 42}}) {
            var input = new LEDataInputStream(new ByteArrayInputStream(bytes));
            var entries = BaboonBinCodec.readIndex(BaboonCodecContext.Compact, input, 2);
            assertEquals(bytes[0] == 1 ? 2 : 0, entries.size());
            assertEquals(42, input.readByte());
            var countInput = new LEDataInputStream(new ByteArrayInputStream(bytes));
            assertEquals(entries.size(), BaboonBinCodec.consumeIndex(BaboonCodecContext.Compact, countInput, 2));
            assertEquals(42, countInput.readByte());
        }
    }

    @Test
    public void materializationRetainsTruncationAndNegativeCountBehavior() throws Exception {
        var input = new LEDataInputStream(new ByteArrayInputStream(new byte[] {1, 0}));
        assertThrows(EOFException.class, () -> BaboonBinCodec.readIndex(BaboonCodecContext.Compact, input, 2));
        var negative = new LEDataInputStream(new ByteArrayInputStream(new byte[] {1, 42}));
        assertEquals(0, BaboonBinCodec.readIndex(BaboonCodecContext.Compact, negative, -1).size());
        assertEquals(42, negative.readByte());
        var countInput = new LEDataInputStream(new ByteArrayInputStream(new byte[] {1, 0}));
        assertThrows(EOFException.class, () -> BaboonBinCodec.consumeIndex(BaboonCodecContext.Compact, countInput, 2));
        var negativeCount = new LEDataInputStream(new ByteArrayInputStream(new byte[] {1, 42}));
        assertEquals(0, BaboonBinCodec.consumeIndex(BaboonCodecContext.Compact, negativeCount, -1));
        assertEquals(42, negativeCount.readByte());
    }
}

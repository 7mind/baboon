package baboon.runtime.shared;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;

public final class BaboonBinTools {
    private BaboonBinTools() {}

    @FunctionalInterface
    public interface ThrowingSupplier<T> {
        T get() throws Exception;
    }

    public static UUID readUid(LEDataInputStream s) throws Exception {
        byte[] bytes = new byte[16];
        s.readFully(bytes);

        // Convert from .NET GUID byte order to Java UUID byte order
        byte b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0;
        byte b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1;
        byte b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4;
        byte b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6;

        long msb = ((long)(bytes[0] & 0xFF) << 56) |
                   ((long)(bytes[1] & 0xFF) << 48) |
                   ((long)(bytes[2] & 0xFF) << 40) |
                   ((long)(bytes[3] & 0xFF) << 32) |
                   ((long)(bytes[4] & 0xFF) << 24) |
                   ((long)(bytes[5] & 0xFF) << 16) |
                   ((long)(bytes[6] & 0xFF) << 8) |
                   ((long)(bytes[7] & 0xFF));

        long lsb = ((long)(bytes[8] & 0xFF) << 56) |
                   ((long)(bytes[9] & 0xFF) << 48) |
                   ((long)(bytes[10] & 0xFF) << 40) |
                   ((long)(bytes[11] & 0xFF) << 32) |
                   ((long)(bytes[12] & 0xFF) << 24) |
                   ((long)(bytes[13] & 0xFF) << 16) |
                   ((long)(bytes[14] & 0xFF) << 8) |
                   ((long)(bytes[15] & 0xFF));

        return new UUID(msb, lsb);
    }

    public static void writeUid(LEDataOutputStream writer, UUID v) throws Exception {
        long msb = v.getMostSignificantBits();
        long lsb = v.getLeastSignificantBits();

        byte[] bytes = new byte[16];
        bytes[0] = (byte)((msb >>> 56) & 0xFF);
        bytes[1] = (byte)((msb >>> 48) & 0xFF);
        bytes[2] = (byte)((msb >>> 40) & 0xFF);
        bytes[3] = (byte)((msb >>> 32) & 0xFF);
        bytes[4] = (byte)((msb >>> 24) & 0xFF);
        bytes[5] = (byte)((msb >>> 16) & 0xFF);
        bytes[6] = (byte)((msb >>> 8) & 0xFF);
        bytes[7] = (byte)(msb & 0xFF);
        bytes[8] = (byte)((lsb >>> 56) & 0xFF);
        bytes[9] = (byte)((lsb >>> 48) & 0xFF);
        bytes[10] = (byte)((lsb >>> 40) & 0xFF);
        bytes[11] = (byte)((lsb >>> 32) & 0xFF);
        bytes[12] = (byte)((lsb >>> 24) & 0xFF);
        bytes[13] = (byte)((lsb >>> 16) & 0xFF);
        bytes[14] = (byte)((lsb >>> 8) & 0xFF);
        bytes[15] = (byte)(lsb & 0xFF);

        // Convert to .NET GUID byte order
        byte b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0;
        byte b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1;
        byte b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4;
        byte b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6;

        writer.write(bytes);
    }

    public static ByteString readByteString(LEDataInputStream input) throws Exception {
        int length = input.readInt();
        byte[] bytes = new byte[length];
        input.readFully(bytes);
        return ByteString.of(bytes);
    }

    public static void writeByteString(LEDataOutputStream output, ByteString bs) throws Exception {
        output.writeInt(bs.length());
        output.write(bs.underlyingUnsafe());
    }

    public static String readString(LEDataInputStream input) throws Exception {
        int length = 0;
        int shift = 0;
        int byteRead;

        do {
            byteRead = input.readByte() & 0xFF;
            length = length | ((byteRead & 0x7F) << shift);
            shift += 7;
        } while ((byteRead & 0x80) != 0);

        byte[] buffer = new byte[length];
        input.readFully(buffer);
        return new String(buffer, StandardCharsets.UTF_8);
    }

    public static void writeString(LEDataOutputStream output, String s) throws Exception {
        byte[] bytes = s.getBytes(StandardCharsets.UTF_8);
        int value = bytes.length;

        do {
            byte currentByte = (byte)(value & 0x7F);
            value >>>= 7;
            if (value != 0) currentByte = (byte)(currentByte | 0x80);
            output.writeByte(currentByte);
        } while (value != 0);

        output.write(bytes);
    }

    public static BigDecimal readBigDecimal(LEDataInputStream input) throws Exception {
        int lo = input.readInt();
        int mid = input.readInt();
        int hi = input.readInt();
        int flags = input.readInt();

        int scale = (flags >>> 16) & 0xFF;
        boolean isNegative = (flags & 0x80000000) != 0;

        long loLong = lo & 0xFFFFFFFFL;
        long midLong = mid & 0xFFFFFFFFL;
        long hiLong = hi & 0xFFFFFFFFL;

        BigInteger mantissa = BigInteger.valueOf(hiLong).shiftLeft(64)
            .or(BigInteger.valueOf(midLong).shiftLeft(32))
            .or(BigInteger.valueOf(loLong));
        BigInteger signedMantissa = isNegative ? mantissa.negate() : mantissa;

        return new BigDecimal(signedMantissa, scale);
    }

    public static void writeBigDecimal(LEDataOutputStream output, BigDecimal value) throws Exception {
        BigInteger unscaled = value.unscaledValue();
        int scale = value.scale();
        assert scale >= 0 && scale <= 28 : "C# Decimal supports scale 0-28";
        assert unscaled.abs().bitLength() <= 96 : "C# Decimal mantissa must fit in 96 bits";

        BigInteger absUnscaled = unscaled.abs();
        int lo = absUnscaled.intValue();
        int mid = absUnscaled.shiftRight(32).intValue();
        int hi = absUnscaled.shiftRight(64).intValue();

        int sign = unscaled.signum() < 0 ? 0x80000000 : 0;
        int flags = sign | (scale << 16);

        output.writeInt(lo);
        output.writeInt(mid);
        output.writeInt(hi);
        output.writeInt(flags);
    }

    public static OffsetDateTime readTimestamp(LEDataInputStream wire) throws Exception {
        long b0 = wire.readByte() & 0xFFL;
        long b1 = wire.readByte() & 0xFFL;
        long b2 = wire.readByte() & 0xFFL;
        long b3 = wire.readByte() & 0xFFL;
        long b4 = wire.readByte() & 0xFFL;
        long b5 = wire.readByte() & 0xFFL;
        long b6 = wire.readByte() & 0xFFL;
        long b7 = wire.readByte() & 0xFFL;
        long dotNetLocalTicksMs = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24) | (b4 << 32) | (b5 << 40) | (b6 << 48) | (b7 << 56);

        long b8 = wire.readByte() & 0xFFL;
        long b9 = wire.readByte() & 0xFFL;
        long b10 = wire.readByte() & 0xFFL;
        long b11 = wire.readByte() & 0xFFL;
        long b12 = wire.readByte() & 0xFFL;
        long b13 = wire.readByte() & 0xFFL;
        long b14 = wire.readByte() & 0xFFL;
        long b15 = wire.readByte() & 0xFFL;
        long offsetMs = b8 | (b9 << 8) | (b10 << 16) | (b11 << 24) | (b12 << 32) | (b13 << 40) | (b14 << 48) | (b15 << 56);

        @SuppressWarnings("unused")
        byte kind = wire.readByte();

        long dotNetUtcTicksMs = dotNetLocalTicksMs - offsetMs;
        long epochMs = dotNetUtcTicksMs - 62135596800000L;
        int offsetSeconds = (int)(offsetMs / 1000);
        ZoneOffset offset = ZoneOffset.ofTotalSeconds(offsetSeconds);
        Instant instant = Instant.ofEpochMilli(epochMs);
        return OffsetDateTime.ofInstant(instant, offset);
    }

    public static void writeTimestamp(LEDataOutputStream writer, OffsetDateTime ref) throws Exception {
        long epochMs = ref.toInstant().toEpochMilli();
        long dotNetUtcTicksMs = epochMs + 62135596800000L;
        long offsetMs = ref.getOffset().getTotalSeconds() * 1000L;
        long dotNetLocalTicksMs = dotNetUtcTicksMs + offsetMs;
        byte kind = (byte)(ref.getOffset().getTotalSeconds() == 0 ? 1 : 0);

        writer.writeByte((int)(dotNetLocalTicksMs & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 8) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 16) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 24) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 32) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 40) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 48) & 0xFF));
        writer.writeByte((int)((dotNetLocalTicksMs >>> 56) & 0xFF));

        writer.writeByte((int)(offsetMs & 0xFF));
        writer.writeByte((int)((offsetMs >>> 8) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 16) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 24) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 32) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 40) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 48) & 0xFF));
        writer.writeByte((int)((offsetMs >>> 56) & 0xFF));

        writer.writeByte(kind);
    }

    public static <T> List<T> readList(int count, ThrowingSupplier<T> reader) throws Exception {
        List<T> result = new ArrayList<>(count);
        for (int i = 0; i < count; i++) {
            result.add(reader.get());
        }
        return List.copyOf(result);
    }

    public static <T> Set<T> readSet(int count, ThrowingSupplier<T> reader) throws Exception {
        LinkedHashSet<T> result = new LinkedHashSet<>();
        for (int i = 0; i < count; i++) {
            result.add(reader.get());
        }
        return Set.copyOf(result);
    }

    public static <K, V> Map<K, V> readMap(int count, ThrowingSupplier<K> keyReader, ThrowingSupplier<V> valueReader) throws Exception {
        LinkedHashMap<K, V> result = new LinkedHashMap<>();
        for (int i = 0; i < count; i++) {
            result.put(keyReader.get(), valueReader.get());
        }
        return Map.copyOf(result);
    }
}

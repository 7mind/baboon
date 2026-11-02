package baboon.runtime.shared;

import java.util.Arrays;

public final class ByteString {
    private final byte[] data;

    private ByteString(byte[] data) {
        this.data = data;
    }

    public static ByteString of(byte[] data) {
        return new ByteString(Arrays.copyOf(data, data.length));
    }

    public byte[] toByteArray() {
        return Arrays.copyOf(data, data.length);
    }

    public byte[] underlyingUnsafe() {
        return data;
    }

    public int length() {
        return data.length;
    }

    public String toHex() {
        StringBuilder sb = new StringBuilder(data.length * 2);
        for (byte b : data) {
            sb.append(String.format("%02x", b & 0xFF));
        }
        return sb.toString();
    }

    public static ByteString fromHex(String hex) {
        byte[] nibbles = new byte[hex.length()];
        int count = 0;
        for (int i = 0; i < hex.length(); i++) {
            char c = hex.charAt(i);
            if (isHexSeparator(c)) continue;
            int nibble = hexNibble(c);
            if (nibble < 0) {
                throw new IllegalArgumentException(String.format("Invalid hex character '%c' (U+%04X) at index %d", c, (int) c, i));
            }
            nibbles[count++] = (byte) nibble;
        }
        if (count % 2 != 0) {
            throw new IllegalArgumentException("Invalid hex string length: " + count + ". Hex string must have even length.");
        }
        byte[] bytes = new byte[count / 2];
        for (int i = 0; i < bytes.length; i++) {
            bytes[i] = (byte) ((nibbles[2 * i] << 4) | nibbles[2 * i + 1]);
        }
        return new ByteString(bytes);
    }

    // Separators: ASCII space, TAB, LF, VT, FF, CR, ':' and '-'
    private static boolean isHexSeparator(char c) {
        return c == ' ' || c == ':' || c == '-' || (c >= 0x09 && c <= 0x0D);
    }

    private static int hexNibble(char c) {
        if (c >= '0' && c <= '9') return c - '0';
        if (c >= 'a' && c <= 'f') return c - 'a' + 10;
        if (c >= 'A' && c <= 'F') return c - 'A' + 10;
        return -1;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof ByteString other)) return false;
        return Arrays.equals(data, other.data);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(data);
    }

    @Override
    public String toString() {
        return "ByteString(" + toHex() + ")";
    }
}

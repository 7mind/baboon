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
        int len = hex.length();
        byte[] bytes = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            bytes[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4) + Character.digit(hex.charAt(i + 1), 16));
        }
        return new ByteString(bytes);
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

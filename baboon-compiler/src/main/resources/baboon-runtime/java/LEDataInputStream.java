package baboon.runtime.shared;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class LEDataInputStream extends InputStream implements DataInput {
    private final DataInputStream dataIn;
    private final ByteBuffer buffer = ByteBuffer.allocate(8);

    public LEDataInputStream(InputStream stream) {
        this.dataIn = new DataInputStream(stream);
    }

    @Override public int read() throws IOException { return readByte() & 0xFF; }
    @Override public int read(byte[] b) throws IOException { return dataIn.read(b); }
    @Override public int read(byte[] b, int off, int len) throws IOException { return dataIn.read(b, off, len); }
    @Override public boolean markSupported() { return dataIn.markSupported(); }
    @Override public synchronized void mark(int readlimit) { dataIn.mark(readlimit); }
    @Override public synchronized void reset() throws IOException { dataIn.reset(); }
    @Override public void close() throws IOException { dataIn.close(); }

    @Override public boolean readBoolean() throws IOException { return dataIn.readBoolean(); }
    @Override public byte readByte() throws IOException { return dataIn.readByte(); }
    @Override public int readUnsignedByte() throws IOException { return dataIn.readByte() & 0xFF; }
    @Override public char readChar() throws IOException { return dataIn.readChar(); }
    @Override public void readFully(byte[] b) throws IOException { dataIn.readFully(b); }
    @Override public void readFully(byte[] b, int off, int len) throws IOException { dataIn.readFully(b, off, len); }
    @Override public String readUTF() throws IOException { return dataIn.readUTF(); }
    @Override public int skipBytes(int n) throws IOException { return dataIn.skipBytes(n); }
    @SuppressWarnings("deprecation")
    @Override public String readLine() throws IOException { return dataIn.readLine(); }

    @Override
    public short readShort() throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.BIG_ENDIAN).putShort(dataIn.readShort()).flip();
        return buffer.order(ByteOrder.LITTLE_ENDIAN).getShort();
    }

    @Override
    public int readUnsignedShort() throws IOException {
        return readShort() & 0xFFFF;
    }

    @Override
    public int readInt() throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.BIG_ENDIAN).putInt(dataIn.readInt()).flip();
        return buffer.order(ByteOrder.LITTLE_ENDIAN).getInt();
    }

    @Override
    public long readLong() throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.BIG_ENDIAN).putLong(dataIn.readLong()).flip();
        return buffer.order(ByteOrder.LITTLE_ENDIAN).getLong();
    }

    @Override
    public float readFloat() throws IOException {
        int tmp = readInt();
        return Float.intBitsToFloat(tmp);
    }

    @Override
    public double readDouble() throws IOException {
        long tmp = readLong();
        return Double.longBitsToDouble(tmp);
    }
}

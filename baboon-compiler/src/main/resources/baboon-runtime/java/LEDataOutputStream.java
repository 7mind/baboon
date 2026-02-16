package baboon.runtime.shared;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class LEDataOutputStream extends OutputStream implements DataOutput {
    private final DataOutputStream dataOut;
    private final ByteBuffer buffer = ByteBuffer.allocate(8);

    public LEDataOutputStream(OutputStream stream) {
        this.dataOut = new DataOutputStream(stream);
    }

    @Override public void write(int b) throws IOException { dataOut.write(b); }
    @Override public void write(byte[] b) throws IOException { dataOut.write(b); }
    @Override public void write(byte[] b, int off, int len) throws IOException { dataOut.write(b, off, len); }
    @Override public void flush() throws IOException { dataOut.flush(); }
    @Override public void close() throws IOException { dataOut.close(); }

    @Override public void writeBoolean(boolean v) throws IOException { dataOut.writeBoolean(v); }
    @Override public void writeByte(int v) throws IOException { dataOut.writeByte(v); }
    @Override public void writeBytes(String s) throws IOException { dataOut.writeBytes(s); }
    @Override public void writeUTF(String s) throws IOException { dataOut.writeUTF(s); }

    @Override
    public void writeChar(int v) throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.LITTLE_ENDIAN).putChar((char)v);
        buffer.flip();
        dataOut.write(buffer.array(), 0, 2);
    }

    @Override
    public void writeChars(String s) throws IOException {
        for (int i = 0; i < s.length(); i++) {
            writeChar(s.charAt(i));
        }
    }

    @Override
    public void writeShort(int v) throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.LITTLE_ENDIAN).putShort((short)v);
        buffer.flip();
        dataOut.write(buffer.array(), 0, 2);
    }

    @Override
    public void writeInt(int v) throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.LITTLE_ENDIAN).putInt(v);
        buffer.flip();
        dataOut.write(buffer.array(), 0, 4);
    }

    @Override
    public void writeLong(long v) throws IOException {
        buffer.clear();
        buffer.order(ByteOrder.LITTLE_ENDIAN).putLong(v);
        buffer.flip();
        dataOut.write(buffer.array(), 0, 8);
    }

    @Override
    public void writeFloat(float v) throws IOException {
        writeInt(Float.floatToIntBits(v));
    }

    @Override
    public void writeDouble(double v) throws IOException {
        writeLong(Double.doubleToLongBits(v));
    }
}

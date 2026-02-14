package baboon.runtime.shared;

import java.util.ArrayList;
import java.util.List;

public interface BaboonBinCodec<T> {
    void encode(BaboonCodecContext ctx, LEDataOutputStream output, T value) throws Exception;
    T decode(BaboonCodecContext ctx, LEDataInputStream input) throws Exception;

    abstract class Base<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    abstract class BaseGenerated<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    abstract class BaseGeneratedAdt<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    abstract class NoEncoder<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        @Override
        public void encode(BaboonCodecContext ctx, LEDataOutputStream output, T value) throws Exception {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }

        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    abstract class NoEncoderGenerated<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        @Override
        public void encode(BaboonCodecContext ctx, LEDataOutputStream output, T value) throws Exception {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }

        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    abstract class NoEncoderGeneratedAdt<T, C extends BaboonBinCodec<T>> implements BaboonBinCodec<T> {
        @Override
        public void encode(BaboonCodecContext ctx, LEDataOutputStream output, T value) throws Exception {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }

        protected List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input) throws Exception {
            return BaboonBinCodec.readIndex(ctx, input, this instanceof BaboonBinCodecIndexed ? ((BaboonBinCodecIndexed) this).indexElementsCount(ctx) : 0);
        }
    }

    static List<int[]> readIndex(BaboonCodecContext ctx, LEDataInputStream input, int expectedElements) throws Exception {
        byte header = input.readByte();
        boolean hasIndex = (header & 1) != 0;
        List<int[]> index = new ArrayList<>();
        if (hasIndex) {
            for (int i = 0; i < expectedElements; i++) {
                int offset = input.readInt();
                int length = input.readInt();
                index.add(new int[]{offset, length});
            }
        }
        return index;
    }
}

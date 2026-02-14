package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;

public interface BaboonJsonCodec<T> {
    JsonNode encode(BaboonCodecContext ctx, T value);
    T decode(BaboonCodecContext ctx, JsonNode wire);

    abstract class Base<T> implements BaboonJsonCodec<T> {
    }

    abstract class BaseGenerated<T> implements BaboonJsonCodec<T> {
    }

    abstract class BaseGeneratedAdt<T> implements BaboonJsonCodec<T> {
    }

    abstract class NoEncoder<T> implements BaboonJsonCodec<T> {
        @Override
        public JsonNode encode(BaboonCodecContext ctx, T value) {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }
    }

    abstract class NoEncoderGenerated<T> implements BaboonJsonCodec<T> {
        @Override
        public JsonNode encode(BaboonCodecContext ctx, T value) {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }
    }

    abstract class NoEncoderGeneratedAdt<T> implements BaboonJsonCodec<T> {
        @Override
        public JsonNode encode(BaboonCodecContext ctx, T value) {
            throw new UnsupportedOperationException("Encoding not supported for deprecated version");
        }
    }
}

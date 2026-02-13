package baboon.runtime.shared;

public sealed interface BaboonEither<L, R> {
    record Left<L, R>(L value) implements BaboonEither<L, R> {}
    record Right<L, R>(R value) implements BaboonEither<L, R> {}

    static <L, R> BaboonEither<L, R> left(L value) {
        return new Left<>(value);
    }

    static <L, R> BaboonEither<L, R> right(R value) {
        return new Right<>(value);
    }
}

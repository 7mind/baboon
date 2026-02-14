package baboon.fixture;

import baboon.runtime.shared.ByteString;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

public abstract class BaboonRandom {
    protected final Random rnd;

    protected BaboonRandom(Random rnd) {
        this.rnd = rnd;
    }

    public abstract boolean nextBit();

    public abstract byte nextI08();
    public abstract short nextI16();
    public abstract int nextI32();
    public abstract long nextI64();

    public abstract short nextU08();
    public abstract int nextU16();
    public abstract long nextU32();
    public abstract long nextU64();

    public abstract float nextF32();
    public abstract double nextF64();
    public abstract BigDecimal nextF128();

    public abstract OffsetDateTime nextTsu();
    public abstract OffsetDateTime nextTso();

    public abstract UUID nextUid();
    public abstract String nextString();
    public abstract ByteString nextByteString();

    public abstract <T> Optional<T> mkOptional(Supplier<T> element);
    public abstract <T> T mkEnum(Class<T> clazz, T[] values);
    public abstract <T> List<T> mkList(Supplier<T> element);
    public abstract <T> Set<T> mkSet(Supplier<T> element);
    public abstract <K, V> Map<K, V> mkMap(Supplier<K> k, Supplier<V> v);

    public abstract <T> T oneOf(List<Function<BaboonRandom, T>> elements);

    public int nextInt(int bound) {
        return rnd.nextInt(bound);
    }
}

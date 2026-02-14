package baboon.fixture;

import baboon.runtime.shared.ByteString;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

final class BaboonRandomImpl extends BaboonRandom {
    BaboonRandomImpl(Random rnd) {
        super(rnd);
    }

    @Override public boolean nextBit() { return rnd.nextBoolean(); }

    @Override public byte nextI08() { return (byte) rnd.nextInt(); }
    @Override public short nextI16() { return (short) rnd.nextInt(); }
    @Override public int nextI32() { return rnd.nextInt(); }
    @Override public long nextI64() { return rnd.nextLong(); }

    @Override public short nextU08() { return (short) (rnd.nextInt() & 0xFF); }
    @Override public int nextU16() { return rnd.nextInt() & 0xFFFF; }
    @Override public long nextU32() { return rnd.nextInt() & 0xFFFFFFFFL; }
    @Override public long nextU64() { return rnd.nextLong(); }

    @Override public float nextF32() { return rnd.nextFloat(); }
    @Override public double nextF64() { return rnd.nextDouble(); }
    @Override public BigDecimal nextF128() { return BigDecimal.valueOf(rnd.nextDouble()); }

    private static final OffsetDateTime EARLIEST = OffsetDateTime.parse("2020-01-01T00:00:00Z");
    private static final OffsetDateTime LATEST = OffsetDateTime.parse("2099-12-31T23:59:59Z");

    private OffsetDateTime generateRandomOffsetDateTime(OffsetDateTime start, OffsetDateTime end) {
        long minSeconds = start.toEpochSecond();
        long maxSeconds = end.toEpochSecond();
        long range = maxSeconds - minSeconds;
        long randomSeconds = minSeconds + (rnd.nextLong() % range + range) % range;
        return OffsetDateTime.ofInstant(Instant.ofEpochSecond(randomSeconds), ZoneOffset.UTC);
    }

    @Override
    public OffsetDateTime nextTsu() {
        return generateRandomOffsetDateTime(EARLIEST, LATEST).withOffsetSameInstant(ZoneOffset.UTC);
    }

    @Override
    public OffsetDateTime nextTso() {
        return generateRandomOffsetDateTime(EARLIEST, LATEST).withOffsetSameInstant(ZoneOffset.ofHours(rnd.nextInt(37) - 18));
    }

    @Override public UUID nextUid() { return UUID.randomUUID(); }

    @Override
    public String nextString() {
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        StringBuilder sb = new StringBuilder(10);
        for (int i = 0; i < 10; i++) {
            sb.append(chars.charAt(rnd.nextInt(chars.length())));
        }
        return sb.toString();
    }

    @Override
    public ByteString nextByteString() {
        int length = rnd.nextInt(21);
        byte[] bytes = new byte[length];
        rnd.nextBytes(bytes);
        return ByteString.of(bytes);
    }

    @Override
    public <T> List<T> mkList(Supplier<T> element) {
        int size = rnd.nextInt(20);
        List<T> list = new ArrayList<>(size);
        for (int i = 0; i < size; i++) list.add(element.get());
        Collections.shuffle(list, rnd);
        return List.copyOf(list);
    }

    @Override
    public <T> Set<T> mkSet(Supplier<T> element) {
        int size = rnd.nextInt(20);
        LinkedHashSet<T> set = new LinkedHashSet<>();
        for (int i = 0; i < size; i++) set.add(element.get());
        return Set.copyOf(set);
    }

    @Override
    public <K, V> Map<K, V> mkMap(Supplier<K> k, Supplier<V> v) {
        int size = rnd.nextInt(20);
        LinkedHashMap<K, V> map = new LinkedHashMap<>();
        for (int i = 0; i < size; i++) map.put(k.get(), v.get());
        return Map.copyOf(map);
    }

    @Override
    public <T> Optional<T> mkOptional(Supplier<T> element) {
        return Optional.of(element.get());
    }

    @Override
    public <T> T mkEnum(Class<T> clazz, T[] values) {
        return values[rnd.nextInt(values.length)];
    }

    @Override
    public <T> T oneOf(List<Function<BaboonRandom, T>> elements) {
        return elements.get(rnd.nextInt(elements.size())).apply(this);
    }
}

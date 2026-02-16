package baboon.runtime.shared;

import java.util.function.Supplier;

public final class Lazy<T> {
    private volatile T value;
    private Supplier<T> supplier;

    public Lazy(Supplier<T> supplier) {
        this.supplier = supplier;
    }

    public T get() {
        T result = value;
        if (result == null) {
            synchronized (this) {
                result = value;
                if (result == null) {
                    result = supplier.get();
                    value = result;
                    supplier = null;
                }
            }
        }
        return result;
    }

    public static <T> Lazy<T> of(Supplier<T> supplier) {
        return new Lazy<>(supplier);
    }
}

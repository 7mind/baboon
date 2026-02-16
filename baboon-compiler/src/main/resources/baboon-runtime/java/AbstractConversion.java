package baboon.runtime.shared;

public abstract class AbstractConversion<From, To> {
    private final Class<From> fromClass;
    private final Class<To> toClass;

    protected AbstractConversion(Class<From> fromClass, Class<To> toClass) {
        this.fromClass = fromClass;
        this.toClass = toClass;
    }

    public abstract <C> To doConvert(C context, AbstractBaboonConversions conversions, From from);

    public abstract String versionFrom();
    public abstract String versionTo();
    public abstract String typeId();

    public Class<From> getFromClass() { return fromClass; }
    public Class<To> getToClass() { return toClass; }
}

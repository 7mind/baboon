package baboon.runtime.shared;

import java.util.*;

public abstract class AbstractBaboonConversions {
    private final Map<String, AbstractConversion<?, ?>> conversions = new LinkedHashMap<>();

    protected <From, To> void register(AbstractConversion<From, To> conversion) {
        String key = conversion.getFromClass().getName() + "->" + conversion.getToClass().getName();
        conversions.put(key, conversion);
    }

    @SuppressWarnings("unchecked")
    public <C, From, To> To convertWithContext(C context, From from, Class<From> fromClass, Class<To> toClass) {
        String key = fromClass.getName() + "->" + toClass.getName();
        AbstractConversion<From, To> conversion = (AbstractConversion<From, To>) conversions.get(key);
        if (conversion == null) {
            throw new BaboonException("No conversion registered from " + fromClass.getName() + " to " + toClass.getName());
        }
        return conversion.doConvert(context, this, from);
    }

    public abstract List<String> versionsFrom();
    public abstract String versionTo();
}

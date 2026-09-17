package baboon.runtime.shared;

import java.util.List;
import java.util.Map;

final class BaboonMetadataAccess {
    private BaboonMetadataAccess() {}

    static String requiredString(Class<?> klass, String name) {
        Object value = requiredField(klass, name);
        if (!(value instanceof String text)) {
            throw new BaboonException("Type " + klass.getName() + " field '" + name + "' is not a String");
        }
        return text;
    }

    @SuppressWarnings("unchecked")
    static List<String> sameInVersions(Class<?> klass) {
        return (List<String>) requiredField(klass, "baboonSameInVersions");
    }

    @SuppressWarnings("unchecked")
    static Map<String, String> minReaderVersions(Class<?> klass) {
        return (Map<String, String>) requiredField(klass, "baboonMinReaderVersions");
    }

    // Conversion has a distinct checked-exception boundary and historically uses
    // casts rather than the envelope accessor's explicit String validation.
    static BaboonDomainVersion conversionDomainVersion(Class<?> klass) throws ReflectiveOperationException {
        String identifier = (String) readField(klass, "baboonDomainIdentifier");
        String version = (String) readField(klass, "baboonDomainVersion");
        return new BaboonDomainVersion(identifier, version);
    }

    private static Object requiredField(Class<?> klass, String name) {
        try {
            return readField(klass, name);
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + klass.getName() + " is missing static field '" + name + "'", e);
        }
    }

    private static Object readField(Class<?> klass, String name) throws ReflectiveOperationException {
        return klass.getField(name).get(null);
    }
}

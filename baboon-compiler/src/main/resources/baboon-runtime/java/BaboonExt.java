package baboon.runtime.shared;

import java.util.List;

public final class BaboonExt {
    private BaboonExt() {}

    public static BaboonDomainVersion domainVersion(BaboonGenerated g) {
        Class<?> klass = g.getClass();
        String identifier = readStaticString(klass, "baboonDomainIdentifier");
        String version = readStaticString(klass, "baboonDomainVersion");
        return new BaboonDomainVersion(identifier, version);
    }

    @SuppressWarnings("unchecked")
    public static String baboonUnmodifiedSinceVersion(BaboonGenerated g) {
        Class<?> klass = g.getClass();
        List<String> sameIn;
        try {
            sameIn = (List<String>) klass.getField("baboonSameInVersions").get(null);
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + klass.getName() + " is missing static field 'baboonSameInVersions'", e);
        }
        return sameIn.get(0);
    }

    public static String unmodifiedSinceVersion(BaboonMeta meta, String typeId) {
        return meta.sameInVersions(typeId).get(0);
    }

    private static String readStaticString(Class<?> klass, String name) {
        try {
            Object v = klass.getField(name).get(null);
            if (!(v instanceof String s)) {
                throw new BaboonException("Type " + klass.getName() + " field '" + name + "' is not a String");
            }
            return s;
        } catch (ReflectiveOperationException e) {
            throw new BaboonException("Type " + klass.getName() + " is missing static field '" + name + "'", e);
        }
    }
}

package baboon.runtime.shared;

public final class BaboonExt {
    private BaboonExt() {}

    public static BaboonDomainVersion domainVersion(BaboonGenerated g) {
        Class<?> klass = g.getClass();
        String identifier = BaboonMetadataAccess.requiredString(klass, "baboonDomainIdentifier");
        String version = BaboonMetadataAccess.requiredString(klass, "baboonDomainVersion");
        return new BaboonDomainVersion(identifier, version);
    }

    public static String baboonUnmodifiedSinceVersion(BaboonGenerated g) {
        return BaboonMetadataAccess.sameInVersions(g.getClass()).get(0);
    }

    public static String unmodifiedSinceVersion(BaboonMeta meta, String typeId) {
        return meta.sameInVersions(typeId).get(0);
    }

}

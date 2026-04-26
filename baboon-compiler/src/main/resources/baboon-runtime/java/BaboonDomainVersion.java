package baboon.runtime.shared;

public record BaboonDomainVersion(String domainIdentifier, String domainVersion) {
    public BaboonVersion version() {
        return BaboonVersion.from(domainVersion);
    }
}

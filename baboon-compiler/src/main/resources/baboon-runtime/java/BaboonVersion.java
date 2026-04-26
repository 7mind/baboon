package baboon.runtime.shared;

/**
 * Parsed semver-shaped version (`major.minor.patch`). Named `BaboonVersion` (not `Version`) to
 * avoid clashes with `java.lang.Runtime.Version` and `java.lang.module.ModuleDescriptor.Version`
 * — same defensive renaming as PR-08-D05 (C#).
 */
public record BaboonVersion(int major, int minor, int patch) implements Comparable<BaboonVersion> {
    @Override
    public int compareTo(BaboonVersion other) {
        int c = Integer.compare(major, other.major);
        if (c != 0) return c;
        c = Integer.compare(minor, other.minor);
        if (c != 0) return c;
        return Integer.compare(patch, other.patch);
    }

    public static BaboonVersion from(String version) {
        String[] chunks = version.split("[.]");
        if (chunks.length < 3) {
            throw new BaboonException("Expected to have version in format x.y.z, got " + version);
        }
        return new BaboonVersion(parse(chunks[0], "major", version), parse(chunks[1], "minor", version), parse(chunks[2], "patch", version));
    }

    private static int parse(String s, String slot, String version) {
        try {
            return Integer.parseInt(s.trim());
        } catch (NumberFormatException e) {
            throw new BaboonException("Expected to have version in format x.y.z, got " + version + ". Invalid " + slot + " value.");
        }
    }
}

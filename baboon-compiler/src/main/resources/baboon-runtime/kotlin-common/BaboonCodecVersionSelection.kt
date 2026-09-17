package baboon.runtime.shared

internal sealed class BaboonCodecVersionSelection {
    data class Exact(val version: BaboonDomainVersion) : BaboonCodecVersionSelection()
    data class Compatible(val version: BaboonDomainVersion) : BaboonCodecVersionSelection()
    data class UnsupportedForward(val version: BaboonDomainVersion) : BaboonCodecVersionSelection()
    data class Unsupported(val version: BaboonDomainVersion) : BaboonCodecVersionSelection()

    companion object {
        fun select(
            typeMeta: BaboonTypeMeta,
            minVersion: BaboonDomainVersion,
            maxVersion: BaboonDomainVersion,
            exact: Boolean,
            tolerant: Boolean,
        ): BaboonCodecVersionSelection {
            val modelVersion = typeMeta.version()
            if (!exact && modelVersion.version > maxVersion.version) {
                // A newer payload is readable when its published compatibility
                // bound reaches the newest registered codec. Tolerant reads use
                // the readable bound; lossless reads use the byte-identical bound.
                val lowerBound = if (tolerant) typeMeta.versionReadableMin() else typeMeta.versionMinCompat()
                return if (lowerBound != null && lowerBound.version <= maxVersion.version) {
                    Exact(maxVersion)
                } else {
                    UnsupportedForward(modelVersion)
                }
            }
            return when {
                // Equality must use exact lookup even for a non-exact request,
                // including the single-version case (PR-07-D02).
                modelVersion.version == maxVersion.version -> Exact(modelVersion)
                modelVersion.version >= minVersion.version && modelVersion.version < maxVersion.version -> Compatible(modelVersion)
                modelVersion.version < minVersion.version -> Compatible(minVersion)
                else -> Unsupported(modelVersion)
            }
        }
    }
}

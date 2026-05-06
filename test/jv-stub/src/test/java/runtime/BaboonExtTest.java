// NOTE: This test references generated DTO/codec symbols (my.ok.Inner, my.ok.BaboonMetadata, ...)
// which are copied/generated into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/jv-stub/). Running `mvn test` directly from
// the source tree may fail with missing symbols; run the test suite from the codegen'd copy.
package runtime;

import baboon.runtime.shared.BaboonDomainVersion;
import baboon.runtime.shared.BaboonExt;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BaboonExtTest {

    private static final my.ok.Inner SUBJECT = new my.ok.Inner(42);

    @Test
    void domainVersion_returnsExpectedIdentifierAndVersion() {
        BaboonDomainVersion dv = BaboonExt.domainVersion(SUBJECT);
        assertEquals(my.ok.Inner.baboonDomainIdentifier, dv.domainIdentifier());
        assertEquals(my.ok.Inner.baboonDomainVersion, dv.domainVersion());
    }

    @Test
    void baboonUnmodifiedSinceVersion_returnsFirstSameInVersion() {
        String result = BaboonExt.baboonUnmodifiedSinceVersion(SUBJECT);
        assertEquals(my.ok.Inner.baboonSameInVersions.get(0), result);
    }

    @Test
    void unmodifiedSinceVersion_returnsFirstSameInVersionForTypeId() {
        my.ok.BaboonMetadata meta = new my.ok.BaboonMetadata();
        String typeId = my.ok.Inner.baboonTypeIdentifier;
        String result = BaboonExt.unmodifiedSinceVersion(meta, typeId);
        assertEquals(meta.sameInVersions(typeId).get(0), result);
    }
}

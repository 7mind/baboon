// T44 — Interface-shape assertions for extracted contracts (Kotlin KMP stub).
//
// Mirrors test/kt-stub/src/test/kotlin/runtime/ExtractedContractsShapeTest.kt.
// Generated symbols are emitted by `mdl :build :test-gen-regular-adt`; run
// from the codegen'd copy under target/test-regular/kt-stub-kmp/.
//
// Coverage: see kt-stub variant for full commentary.
//   (a) Contract variant — host assignable to B; B-declared member readable.
//   (b) Mirror variant — probe implements B; mirror host does NOT.
//   (c) Member-set spot-check.
package runtime

import my.ok.extracted.contracts.*
import org.junit.jupiter.api.Test

class ExtractedContractsShapeTest {

    private fun require(condition: Boolean, message: String) {
        if (!condition) throw RuntimeException("ExtractedContractsShapeTest(kmp): $message")
    }

    // ── (a) Contract variant ─────────────────────────────────────────────────

    @Test
    fun iBox_contractVariant_intBoxAssignableAndCountReadable() {
        val host = IntBox(count = 7, item = 42)
        val b: IBox = host
        val countViaB: Int = b.count
        require(countViaB == 7, "IBox.count must equal 7 but was $countViaB")
    }

    @Test
    fun iBox_contractVariant_strBoxAlsoAssignableToSameB_req8() {
        val strHost = StrBox(count = 3, item = "hello")
        val b: IBox = strHost
        val countViaB: Int = b.count
        require(countViaB == 3, "IBox.count (via StrBox) must equal 3 but was $countViaB")
    }

    @Test
    fun iKey_contractVariant_intKeyAssignableAndKeyReadable() {
        val host = IntKey(key = 999L, v = 1)
        val b: IKey = host
        val keyViaB: Long = b.key
        require(keyViaB == 999L, "IKey.key must equal 999 but was $keyViaB")
    }

    @Test
    fun iTagged_contractVariant_intTaggedAssignableAndLabelReadable() {
        val host = IntTagged(label = "hello", extra = 5)
        val b: ITagged = host
        val labelViaB: String = b.label
        require(labelViaB == "hello", "ITagged.label must equal 'hello' but was '$labelViaB'")
    }

    @Test
    fun iContainer_contractVariant_intContainerAssignableAndMembersReadable() {
        val host = IntContainer(own = 1, item = 99, first = 2, second = 3, base_field = 4)
        val b: IContainer = host
        require(b.own == 1, "IContainer.own must equal 1 but was ${b.own}")
        require(b.second == 3, "IContainer.second must equal 3 but was ${b.second}")
        require(b.base_field == 4, "IContainer.base_field must equal 4 but was ${b.base_field}")
    }

    @Test
    fun iResult_contractVariant_okBranchAssignableAndTagReadable() {
        val host: IntResult = IntResult.Ok(tag = "ok", result = 0)
        val b: IResult = host
        val tagViaB: String = b.tag
        require(tagViaB == "ok", "IResult.tag must equal 'ok' but was '$tagViaB'")
    }

    // ── (b) Mirror variant ───────────────────────────────────────────────────

    @Test
    fun iMirroredPayload_mirrorVariant_probeAssignableAndLabelReadable() {
        val probe = IntPayloadProbe(label = "mirror-test")
        val b: IMirroredPayload = probe
        val labelViaB: String = b.label
        require(labelViaB == "mirror-test",
            "IMirroredPayload.label must equal 'mirror-test' but was '$labelViaB'")
    }

    @Test
    fun iMirroredPayload_mirrorVariant_intPayloadDoesNotImplementIMirroredPayload() {
        val host = IntPayload(label = "x", value = 1)
        val hostIsB = (host as Any) is IMirroredPayload
        require(!hostIsB,
            "IntPayload must NOT implement IMirroredPayload (it is the mirror host, not a probe)")
    }

    @Test
    fun iTagMirror_mirrorVariant_probeAssignableAndLabelReadable() {
        val probe = IntTagMirrorProbe(label = "tag-mirror")
        val b: ITagMirror = probe
        val labelViaB: String = b.label
        require(labelViaB == "tag-mirror",
            "ITagMirror.label must equal 'tag-mirror' but was '$labelViaB'")
    }

    @Test
    fun iTagMirror_mirrorVariant_intTaggedDoesNotImplementITagMirror() {
        val host = IntTagged(label = "x", extra = 1)
        val hostIsB = (host as Any) is ITagMirror
        require(!hostIsB,
            "IntTagged must NOT implement ITagMirror (it carries ITagged, the contract variant)")
    }

    // ── (c) Member-set spot-checks ────────────────────────────────────────────

    @Test
    fun iMirroredPayload_memberSet_hasExactlyLabel() {
        val probe = IntPayloadProbe(label = "spot")
        val b: IMirroredPayload = probe
        val label: String = b.label
        require(label == "spot", "IMirroredPayload.label spot-check failed: '$label'")
    }

    @Test
    fun iBox_memberSet_hasExactlyCount() {
        val box = IntBox(count = 11, item = 0)
        val b: IBox = box
        val count: Int = b.count
        require(count == 11, "IBox.count spot-check failed: $count")
    }

    @Test
    fun iKey_memberSet_hasExactlyKey() {
        val key = IntKey(key = 12345L, v = 0)
        val b: IKey = key
        val k: Long = b.key
        require(k == 12345L, "IKey.key spot-check failed: $k")
    }

    @Test
    fun iContainer_memberSet_hasOwnSecondBaseField() {
        val c = IntContainer(own = 10, item = 0, first = 0, second = 20, base_field = 30)
        val b: IContainer = c
        require(b.own == 10, "IContainer.own spot-check failed: ${b.own}")
        require(b.second == 20, "IContainer.second spot-check failed: ${b.second}")
        require(b.base_field == 30, "IContainer.base_field spot-check failed: ${b.base_field}")
    }
}

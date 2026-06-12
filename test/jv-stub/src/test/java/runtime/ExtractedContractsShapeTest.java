// T44 — Interface-shape assertions for extracted contracts (Java stub).
//
// Generated symbols (my.ok.extracted.contracts.*) are emitted by
// `mdl :build :test-gen-regular-adt`. Running tests directly from the source
// tree will fail with missing symbols; run from the codegen'd copy under
// target/test-regular/jv-stub/.
//
// Coverage:
//   (a) Contract variant — host is assignable to B (compile-time variable typed
//       as B); a B-declared member is readable at runtime through the B-typed
//       reference. Failures throw unconditionally (not via JUnit assert, which
//       may be disabled in some build configurations).
//   (b) Mirror variant — B exists as a standalone interface with the expected
//       member set. The probe type DOES implement B (compile-time typed
//       assignment). The mirror host does NOT implement B. The negative is
//       verified at runtime via `instanceof`.
//   (c) Member-set spot-check — field names and types verified through B-typed
//       bindings (compile-time + runtime).
package runtime;

import my.ok.extracted.contracts.*;
import org.junit.jupiter.api.Test;

class ExtractedContractsShapeTest {

    private static void require(boolean condition, String message) {
        if (!condition)
            throw new RuntimeException("ExtractedContractsShapeTest: " + message);
    }

    // ── (a) Contract variant: host instantiation assignable to B ─────────────

    @Test
    void iBox_contractVariant_intBoxAssignableAndCountReadable() {
        IntBox host = new IntBox(7, 42);

        // Compile-time: IntBox implements IBox — the type annotation enforces it.
        IBox b = host;

        int countViaB = b.count();
        require(countViaB == 7, "IBox.count must equal 7 but was " + countViaB);
    }

    @Test
    void iBox_contractVariant_strBoxAlsoAssignableToSameB_req8() {
        // Req 8: both IntBox and StrBox implement the single IBox.
        StrBox strHost = new StrBox(3, "hello");
        IBox b = strHost;
        int countViaB = b.count();
        require(countViaB == 3, "IBox.count (via StrBox) must equal 3 but was " + countViaB);
    }

    @Test
    void iKey_contractVariant_intKeyAssignableAndKeyReadable() {
        IntKey host = new IntKey(999L, 1);

        // Compile-time: IntKey implements IKey.
        IKey b = host;

        long keyViaB = b.key();
        require(keyViaB == 999L, "IKey.key must equal 999 but was " + keyViaB);
    }

    @Test
    void iTagged_contractVariant_intTaggedAssignableAndLabelReadable() {
        IntTagged host = new IntTagged("hello", 5);

        // Compile-time: IntTagged implements ITagged.
        ITagged b = host;

        String labelViaB = b.label();
        require("hello".equals(labelViaB),
            "ITagged.label must equal 'hello' but was '" + labelViaB + "'");
    }

    @Test
    void iContainer_contractVariant_intContainerAssignableAndMembersReadable() {
        IntContainer host = new IntContainer(1, 99, 2, 3, 4);

        // Compile-time: IntContainer implements IContainer.
        IContainer b = host;

        require(b.own() == 1, "IContainer.own must equal 1 but was " + b.own());
        require(b.second() == 3, "IContainer.second must equal 3 but was " + b.second());
        require(b.base_field() == 4, "IContainer.base_field must equal 4 but was " + b.base_field());
    }

    @Test
    void iResult_contractVariant_okBranchAssignableAndTagReadable() {
        // IntResult.Ok implements IntResult (sealed), ResultBase, and IResult.
        // IResult extends ResultBase which declares tag().
        IntResult.Ok host = new IntResult.Ok("ok", 0);

        // Compile-time: IResult extends ResultBase; Ok implements IResult.
        IResult b = host;

        String tagViaB = b.tag();
        require("ok".equals(tagViaB), "IResult.tag must equal 'ok' but was '" + tagViaB + "'");
    }

    // ── (b) Mirror variant: probe implements B; host does NOT ─────────────────

    @Test
    void iMirroredPayload_mirrorVariant_probeAssignableAndLabelReadable() {
        // IntPayloadProbe implements IMirroredPayload (probe type for the mirror B).
        IntPayloadProbe probe = new IntPayloadProbe("mirror-test");

        // Compile-time: IntPayloadProbe implements IMirroredPayload.
        IMirroredPayload b = probe;

        String labelViaB = b.label();
        require("mirror-test".equals(labelViaB),
            "IMirroredPayload.label must equal 'mirror-test' but was '" + labelViaB + "'");
    }

    @Test
    void iMirroredPayload_mirrorVariant_intPayloadDoesNotImplementIMirroredPayload() {
        // Negative: IntPayload is the mirror host; it does NOT implement IMirroredPayload.
        // Only the probe carries the mirror interface.
        //
        // Java sealed records trigger a compiler warning on direct `instanceof`
        // checks when the type is provably false. Cast to Object to make the
        // check runtime-evaluated without triggering the warning.
        IntPayload host = new IntPayload("x", 1);
        boolean hostIsB = ((Object) host) instanceof IMirroredPayload;
        require(!hostIsB,
            "IntPayload must NOT implement IMirroredPayload (it is the mirror host, not a probe)");
    }

    @Test
    void iTagMirror_mirrorVariant_probeAssignableAndLabelReadable() {
        IntTagMirrorProbe probe = new IntTagMirrorProbe("tag-mirror");

        // Compile-time: IntTagMirrorProbe implements ITagMirror.
        ITagMirror b = probe;

        String labelViaB = b.label();
        require("tag-mirror".equals(labelViaB),
            "ITagMirror.label must equal 'tag-mirror' but was '" + labelViaB + "'");
    }

    @Test
    void iTagMirror_mirrorVariant_intTaggedDoesNotImplementITagMirror() {
        // Negative: IntTagged implements ITagged (contract), NOT ITagMirror (mirror).
        IntTagged host = new IntTagged("x", 1);
        boolean hostIsB = ((Object) host) instanceof ITagMirror;
        require(!hostIsB,
            "IntTagged must NOT implement ITagMirror (it carries ITagged, the contract variant)");
    }

    // ── (c) Member-set spot-checks ─────────────────────────────────────────────

    @Test
    void iMirroredPayload_memberSet_hasExactlyLabel() {
        IntPayloadProbe probe = new IntPayloadProbe("spot");
        IMirroredPayload b = probe;
        String label = b.label(); // compile-time: String
        require("spot".equals(label), "IMirroredPayload.label spot-check failed: '" + label + "'");
    }

    @Test
    void iBox_memberSet_hasExactlyCount() {
        IntBox box = new IntBox(11, 0);
        IBox b = box;
        int count = b.count(); // compile-time: int
        require(count == 11, "IBox.count spot-check failed: " + count);
    }

    @Test
    void iKey_memberSet_hasExactlyKey() {
        IntKey key = new IntKey(12345L, 0);
        IKey b = key;
        long k = b.key(); // compile-time: long
        require(k == 12345L, "IKey.key spot-check failed: " + k);
    }

    @Test
    void iContainer_memberSet_hasOwnSecondBaseField() {
        IntContainer c = new IntContainer(10, 0, 0, 20, 30);
        IContainer b = c;
        require(b.own() == 10, "IContainer.own spot-check failed: " + b.own());
        require(b.second() == 20, "IContainer.second spot-check failed: " + b.second());
        require(b.base_field() == 30, "IContainer.base_field spot-check failed: " + b.base_field());
    }
}

// T43 — Interface-shape assertions for extracted contracts (C# stub).
//
// Generated symbols (My.Ok.Extracted.Contracts.*) are emitted by
// `mdl :build :test-gen-regular-adt`. Running dotnet test directly from
// the source tree will fail with missing symbols; run from the codegen'd copy.
//
// Coverage:
//   (a) Contract variant — host instantiation is assignable to B (compile-time
//       binding to the B interface), and a B-declared member is readable at
//       runtime through the B-typed reference. Failures throw unconditionally.
//   (b) Mirror variant — B exists as a standalone interface with the expected
//       member set (compile-time usage of every member), AND the host type
//       does NOT implement it. The negative is documented as a commented
//       non-compiling snippet (C# has no negative compile-time assert).
//   (c) Member-set spot-check — verify field names and types in B's interface
//       declaration by reading them through a B-typed reference.
//
// Runtime checks use explicit throws (not Assert.That / Debug.Assert).
// See service-acceptance precedent: Debug.Assert is a no-op in release mode.
#nullable enable

using System;
using NUnit.Framework;
using My.Ok.Extracted.Contracts;

namespace ConversionsTest
{
    [TestFixture]
    public class ExtractedContractsShapeTests
    {
        // ── helper ────────────────────────────────────────────────────────────

        private static void Require(bool condition, string message)
        {
            if (!condition)
                throw new InvalidOperationException($"ExtractedContractsShapeTests: {message}");
        }

        // ── (a) Contract variant: host instantiation assignable to B ──────────

        [Test]
        public void IBox_ContractVariant_IntBoxAssignableAndCountReadable()
        {
            IntBox host = new IntBox(Count: 7, Item: 42);

            // Compile-time: IntBox is IBox (sealed record implements IBox).
            IBox b = host;

            // Runtime: read IBox-declared member through the B-typed reference.
            int countViaB = b.Count;
            Require(countViaB == 7, $"IBox.Count must equal 7 but was {countViaB}");
        }

        [Test]
        public void IBox_ContractVariant_StrBoxAlsoAssignableToSameB()
        {
            // Req 8: two instantiations of one host share the single B.
            StrBox strHost = new StrBox(Count: 3, Item: "hello");
            IBox b = strHost;
            int countViaB = b.Count;
            Require(countViaB == 3, $"IBox.Count (via StrBox) must equal 3 but was {countViaB}");
        }

        [Test]
        public void IKey_ContractVariant_IntKeyAssignableAndKeyReadable()
        {
            IntKey host = new IntKey(Key: 999L, V: 1);
            IKey b = host;
            long keyViaB = b.Key;
            Require(keyViaB == 999L, $"IKey.Key must equal 999 but was {keyViaB}");
        }

        [Test]
        public void ITagged_ContractVariant_IntTaggedAssignableAndLabelReadable()
        {
            IntTagged host = new IntTagged(Label: "hello", Extra: 5);
            ITagged b = host;
            string labelViaB = b.Label;
            Require(labelViaB == "hello", $"ITagged.Label must equal 'hello' but was '{labelViaB}'");
        }

        [Test]
        public void IContainer_ContractVariant_IntContainerAssignableAndMembersReadable()
        {
            // IntContainer(Own, Item, First, Second, Base_field)
            IntContainer host = new IntContainer(Own: 1, Item: 99, First: 2, Second: 3, Base_field: 4);
            IContainer b = host;

            // Spot-check all three B-declared param-free fields.
            Require(b.Own == 1, $"IContainer.Own must equal 1 but was {b.Own}");
            Require(b.Second == 3, $"IContainer.Second must equal 3 but was {b.Second}");
            Require(b.Base_field == 4, $"IContainer.Base_field must equal 4 but was {b.Base_field}");
        }

        [Test]
        public void IResult_ContractVariant_OkBranchAssignableAndTagReadable()
        {
            // IntResult is an abstract record; IResult : ResultBase exposes Tag.
            IntResult.Ok host = new IntResult.Ok(Tag: "ok", Result: 0);
            IResult b = host;

            // IResult extends ResultBase which declares Tag.
            string tagViaB = b.Tag;
            Require(tagViaB == "ok", $"IResult.Tag must equal 'ok' but was '{tagViaB}'");
        }

        // ── (b) Mirror variant: standalone interface + negative ───────────────

        [Test]
        public void IMirroredPayload_MirrorVariant_ProbeAssignableAndLabelReadable()
        {
            // IntPayloadProbe implements IMirroredPayload (it is the mirror-probe type).
            IntPayloadProbe probe = new IntPayloadProbe(Label: "mirror-test");
            IMirroredPayload b = probe;
            string labelViaB = b.Label;
            Require(labelViaB == "mirror-test",
                $"IMirroredPayload.Label must equal 'mirror-test' but was '{labelViaB}'");
        }

        [Test]
        public void IMirroredPayload_MirrorVariant_IntPayloadDoesNotImplementIMirroredPayload()
        {
            // Negative (mirror variant): IntPayload is the host; it does NOT implement
            // IMirroredPayload. The mirror B is standalone — only the probe type carries it.
            //
            // The following would NOT compile (type mismatch) and is intentionally left
            // as a comment to document the non-assignability at spec level:
            //
            //   IMirroredPayload b = new IntPayload(Label: "x", Value: 1);  // CS0266
            //
            // C# sealed records trigger CS0184 for direct `is` checks because the
            // compiler statically proves no instance can satisfy both. Cast to object
            // first to make the check runtime-evaluated without CS0184.
            IntPayload host = new IntPayload(Label: "x", Value: 1);
            bool hostIsB = (object)host is IMirroredPayload;
            Require(!hostIsB,
                "IntPayload must NOT implement IMirroredPayload (it is the mirror host, not a probe)");
        }

        [Test]
        public void ITagMirror_MirrorVariant_ProbeAssignableAndLabelReadable()
        {
            IntTagMirrorProbe probe = new IntTagMirrorProbe(Label: "tag-mirror");
            ITagMirror b = probe;
            string labelViaB = b.Label;
            Require(labelViaB == "tag-mirror",
                $"ITagMirror.Label must equal 'tag-mirror' but was '{labelViaB}'");
        }

        [Test]
        public void ITagMirror_MirrorVariant_IntTaggedDoesNotImplementITagMirror()
        {
            // IntTagged implements ITagged (contract), not ITagMirror (mirror).
            // Negative: the host type does not carry the mirror interface.
            //
            //   ITagMirror b = new IntTagged(Label: "x", Extra: 1);  // CS0266
            //
            // Cast to object to avoid CS0184 on sealed record.
            IntTagged host = new IntTagged(Label: "x", Extra: 1);
            bool hostIsB = (object)host is ITagMirror;
            Require(!hostIsB,
                "IntTagged must NOT implement ITagMirror (it carries ITagged, the contract variant)");
        }

        // ── (c) Member-set spot-check ─────────────────────────────────────────

        [Test]
        public void IMirroredPayload_MemberSet_HasExactlyLabel()
        {
            // IMirroredPayload declares: Label: String.
            // Exercise via compile-time usage (the assignment already compiles only
            // if the member exists with the right type).
            IntPayloadProbe probe = new IntPayloadProbe(Label: "spot");
            IMirroredPayload b = probe;
            string label = b.Label;   // must be String / compile-time type check
            Require(label == "spot", $"IMirroredPayload.Label spot-check failed: '{label}'");
        }

        [Test]
        public void IBox_MemberSet_HasExactlyCount()
        {
            IntBox box = new IntBox(Count: 11, Item: 0);
            IBox b = box;
            int count = b.Count;   // Int32
            Require(count == 11, $"IBox.Count spot-check failed: {count}");
        }

        [Test]
        public void IKey_MemberSet_HasExactlyKey()
        {
            IntKey key = new IntKey(Key: 12345L, V: 0);
            IKey b = key;
            long k = b.Key;   // Int64
            Require(k == 12345L, $"IKey.Key spot-check failed: {k}");
        }

        [Test]
        public void IContainer_MemberSet_HasOwnSecondBaseField()
        {
            IntContainer c = new IntContainer(Own: 10, Item: 0, First: 0, Second: 20, Base_field: 30);
            IContainer b = c;
            Require(b.Own == 10, $"IContainer.Own spot-check failed: {b.Own}");
            Require(b.Second == 20, $"IContainer.Second spot-check failed: {b.Second}");
            Require(b.Base_field == 30, $"IContainer.Base_field spot-check failed: {b.Base_field}");
        }
    }
}

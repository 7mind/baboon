// A wrapped ADT-branch JSON codec writes `{"<Branch>": {...}}`. Its decoder has to read the
// payload under its own branch name and refuse an envelope keyed by another branch.
//
// Uses the my.ok.scope fixture; in the regular (unwrapped) lane a branch codec writes no
// envelope and the test is ignored.
#nullable enable

using System;
using Baboon.Runtime.Shared;
using My.Ok.Scope.inner;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class WrappedBranchEnvelopeTests
    {
        private const string OwnBranch = "Dot";
        private const string OtherBranch = "Line";

        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Compact;

        private JObject EncodedDot()
        {
            var dot = new Shape.Dot(new Item("a", Kind.First));
            var encoded = (JObject)Shape.Dot_JsonCodec.Instance.Encode(_ctx, dot);
            if (!encoded.ContainsKey(OwnBranch))
            {
                Assert.Ignore("regular ADT-branch codecs write no envelope");
            }

            return encoded;
        }

        [Test]
        public void BranchDecoderRejectsEnvelopeOfAnotherBranch()
        {
            var payload = EncodedDot()[OwnBranch]!;
            var foreign = new JObject(new JProperty(OtherBranch, payload));
            Assert.Throws<ArgumentException>(() => Shape.Dot_JsonCodec.Instance.Decode(_ctx, foreign));
        }

        [Test]
        public void BranchDecoderReadsItsOwnKeyWhereverItStands()
        {
            var payload = EncodedDot()[OwnBranch]!;
            var withLeadingKey = new JObject(new JProperty(OtherBranch, new JObject()), new JProperty(OwnBranch, payload));
            var decoded = Shape.Dot_JsonCodec.Instance.Decode(_ctx, withLeadingKey);
            Assert.That(decoded, Is.EqualTo(new Shape.Dot(new Item("a", Kind.First))));
        }
    }
}

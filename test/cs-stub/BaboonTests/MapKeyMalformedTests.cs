// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonCodecException.DecoderFailure with message containing
// "malformed key". Replaces the prior unchecked cast that produced InvalidCastException.
//
// Uses the my.ok.m19.singleid fixture; generated symbols are produced by
// mdl :test-gen-regular-adt under target/test-regular/cs-stub/.
#nullable enable

using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class MapKeyMalformedTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Compact;

        [Test]
        public void HolderJsonDecodeThrowsDecoderFailureForMalformedMapKey()
        {
            var badJson = "{\"m\":{\"not_a_valid_id\":\"v\"}}";
            var node = JToken.Parse(badJson);
            var ex = Assert.Throws<BaboonCodecException.DecoderFailure>(() =>
                My.Ok.M19.Singleid.Holder_JsonCodec.Instance.Decode(_ctx, node));
            Assert.That(ex!.Message, Does.Contain("malformed key"));
        }
    }
}

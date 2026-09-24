// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned. It is the only thing standing between a reader-side
// simplification and silently dropping backward compatibility.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/cs-stub/.
#nullable enable

using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class LegacyInt64WireTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Compact;

        [Test]
        public void I64DecodesFromTheLegacyNumericForm()
        {
            var wire = JToken.Parse("{\"x\":-9007199254740991}");
            var decoded = Identifier.Ok.LongId_JsonCodec.Instance.Decode(_ctx, wire);
            Assert.That(decoded.X, Is.EqualTo(-9007199254740991L));
        }

        [Test]
        public void I64DecodesFromTheStringForm()
        {
            var wire = JToken.Parse("{\"x\":\"-9223372036854775808\"}");
            var decoded = Identifier.Ok.LongId_JsonCodec.Instance.Decode(_ctx, wire);
            Assert.That(decoded.X, Is.EqualTo(long.MinValue));
        }

        [Test]
        public void U64DecodesFromTheLegacyNumericForm()
        {
            var wire = JToken.Parse("{\"a\":1,\"b\":2,\"c\":3,\"d\":42}");
            var decoded = Identifier.Ok.UInts_JsonCodec.Instance.Decode(_ctx, wire);
            Assert.That(decoded.D, Is.EqualTo(42UL));
        }

        [Test]
        public void U64DecodesFromTheStringForm()
        {
            var wire = JToken.Parse("{\"a\":1,\"b\":2,\"c\":3,\"d\":\"42\"}");
            var decoded = Identifier.Ok.UInts_JsonCodec.Instance.Decode(_ctx, wire);
            Assert.That(decoded.D, Is.EqualTo(42UL));
        }
    }
}

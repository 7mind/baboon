// JSON wire form of `bytes` (issues #102 / #103): hex digits, with ASCII space, \t, \n, \v,
// \f, \r, ':' and '-' ignored anywhere; anything else, or an odd digit count, is a decode
// failure. Every backend runs the same vectors.
//
// The codec-level case uses testpkg.pkg0 T1_D1 (f4: bytes, f5: opt[bytes], f6: map[bytes, str]); generated symbols
// are produced by mdl :test-gen-regular-adt under target/test-regular/cs-stub/.
#nullable enable

using System;
using System.Collections.Generic;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class ByteStringHexTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Compact;

        private static IEnumerable<TestCaseData> Valid()
        {
            var abcd = new byte[] {0xAB, 0xCD};
            yield return new TestCaseData("", new byte[0]);
            yield return new TestCaseData("ABCD", abcd);
            yield return new TestCaseData("abcd", abcd);
            yield return new TestCaseData("aBcD", abcd);
            yield return new TestCaseData("AB CD", abcd);
            yield return new TestCaseData("AB:CD", abcd);
            yield return new TestCaseData("AB-CD", abcd);
            yield return new TestCaseData("A B C D", abcd);
            yield return new TestCaseData("\tAB\r\nCD ", abcd);
            yield return new TestCaseData("A:B-C D", abcd);
            yield return new TestCaseData("\vA\fB", new byte[] {0xAB});
            yield return new TestCaseData("00FF7f", new byte[] {0x00, 0xFF, 0x7F});
            yield return new TestCaseData(" - : ", new byte[0]);
        }

        private static readonly string[] Invalid =
        {
            "ZZ", "AQ", "+1", "0x01", "q80=", "ABC", "A", "-1",
            "٣٣", "ＡＡ", "aéb", "AB CD",
            "AB\t1CD", "AB1\nCD",
        };

        [TestCaseSource(nameof(Valid))]
        public void ParseAcceptsValidHex(string input, byte[] expected)
        {
            Assert.That(ByteString.Parse(input).ToArray(), Is.EqualTo(expected));
            Assert.That(ByteString.FromHex(input).ToArray(), Is.EqualTo(expected));
        }

        [TestCaseSource(nameof(Invalid))]
        public void ParseRejectsInvalidHex(string input)
        {
            Assert.Throws<FormatException>(() => ByteString.Parse(input));
            Assert.Throws<FormatException>(() => ByteString.FromHex(input));
        }

        [Test]
        public void JsonCodecAcceptsSeparatedHex()
        {
            var wire = JToken.Parse("{\"f0\":{\"f\":\"x\",\"f1\":null},\"f1\":1,\"f3\":\"s\",\"f4\":\"AB:CD\",\"f5\":\"\\tab-cd \",\"f6\":{\"ab:CD\":\"v\"}}");
            var decoded = Testpkg.Pkg0.T1_D1_JsonCodec.Instance.Decode(_ctx, wire);
            Assert.That(decoded.F4.ToArray(), Is.EqualTo(new byte[] {0xAB, 0xCD}));
            Assert.That(decoded.F5!.ToArray(), Is.EqualTo(new byte[] {0xAB, 0xCD}));
            Assert.That(decoded.F6[new ByteString(new byte[] {0xAB, 0xCD})], Is.EqualTo("v"));
        }

        [TestCase("AB\t1CD")]
        [TestCase("0x01")]
        public void JsonCodecRejectsMalformedHex(string f4)
        {
            var wire = new JObject(
                new JProperty("f0", new JObject(new JProperty("f", "x"), new JProperty("f1", JValue.CreateNull()))),
                new JProperty("f1", 1),
                new JProperty("f3", "s"),
                new JProperty("f4", f4),
                new JProperty("f5", JValue.CreateNull()),
                new JProperty("f6", new JObject()));
            Assert.Throws<FormatException>(() => Testpkg.Pkg0.T1_D1_JsonCodec.Instance.Decode(_ctx, wire));
        }
    }
}

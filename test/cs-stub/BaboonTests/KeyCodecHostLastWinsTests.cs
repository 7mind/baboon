// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.Register(impl) overwrites the previously registered
// impl (last-wins). C# already used a `volatile static` mutable singleton
// pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/cs-stub/.
#nullable enable

using System.Collections.Generic;
using Baboon.Runtime.Shared;
using My.Ok.M19.Foreign;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class KeyCodecHostLastWinsTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Compact;

        private sealed class PrefixCodec : FStr_KeyCodec
        {
            private readonly string _tag;
            public PrefixCodec(string tag) { _tag = tag; }
            public string EncodeKey(string value) => _tag + ":" + value;
            public string DecodeKey(string s)
            {
                var pfx = _tag + ":";
                return s.StartsWith(pfx) ? s.Substring(pfx.Length) : s;
            }
        }

        private sealed class IdentityCodec : FStr_KeyCodec
        {
            public string EncodeKey(string value) => value;
            public string DecodeKey(string s) => s;
        }

        [Test]
        public void RegisterBAfterRegisterAObservesB()
        {
            var m = new Dictionary<ItemKey, string> { [new ItemKey("k")] = "v" };
            var original = new Holder(m);

            FStr_KeyCodecHost.Register(new PrefixCodec("A"));
            var encodedA = Holder_JsonCodec.Instance.Encode(_ctx, original).ToString();
            Assert.That(encodedA, Does.Contain("A:k"),
                "expected A: prefix in encoded wire form");

            FStr_KeyCodecHost.Register(new PrefixCodec("B"));
            var encodedB = Holder_JsonCodec.Instance.Encode(_ctx, original).ToString();
            Assert.That(encodedB, Does.Contain("B:k"),
                "PR-26.2 last-wins regression: expected B: prefix after re-register");
            Assert.That(encodedB, Does.Not.Contain("A:k"),
                "PR-26.2 last-wins regression: A: prefix still present after B re-register");

            // Restore identity-encoding default for any subsequent tests in this AppDomain.
            FStr_KeyCodecHost.Register(new IdentityCodec());
        }
    }
}

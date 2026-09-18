#nullable enable
using System;
using System.Collections.Generic;
using System.Linq;
using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class CollectionHashTests
    {
        private const int Seed = 0x1EAFDEAD;

        [Test]
        public void SetHashPreservesOrderIndependentSignedHashAccumulation()
        {
            var hashes = new[] { int.MinValue, -42, -1, 0, 1, 42, int.MaxValue };
            Assert.That(BaboonTools.SetHashcode(new HashSet<int>(), v => v), Is.EqualTo(Seed));
            foreach (var a in hashes)
            foreach (var b in hashes)
            foreach (var c in hashes)
            {
                var values = new HashSet<int> { a, b, c };
                var expected = values.OrderBy(v => v).Aggregate(Seed, (acc, v) => acc ^ v);
                Assert.That(BaboonTools.SetHashcode(values, v => v), Is.EqualTo(expected));
                Assert.That(BaboonTools.SetHashcode(new HashSet<int> { c, b, a }, v => v), Is.EqualTo(expected));
            }
        }

        [Test]
        public void MapHashPreservesDuplicatesAndCallbackOrder()
        {
            var values = new[]
            {
                new KeyValuePair<int, int>(3, -9),
                new KeyValuePair<int, int>(1, int.MinValue),
                new KeyValuePair<int, int>(3, -9),
            };
            var calls = new List<string>();
            var expected = values.Select(v => HashCode.Combine(v.Key, v.Value))
                .OrderBy(v => v).Aggregate(Seed, (acc, v) => acc ^ v);
            var actual = BaboonTools.MapHashcode(values,
                key => { calls.Add($"key:{key}"); return key; },
                value => { calls.Add($"value:{value}"); return value; });
            Assert.That(actual, Is.EqualTo(expected));
            Assert.That(calls, Is.EqualTo(new[] { "key:3", "value:-9", "key:1", "value:-2147483648", "key:3", "value:-9" }));
            Assert.That(BaboonTools.MapHashcode(Array.Empty<KeyValuePair<int, int>>(), v => v, v => v), Is.EqualTo(Seed));
        }

        [Test]
        public void ThrowingHashStopsAtTheSameElement()
        {
            var values = new HashSet<int> { 1, 2, 3 };
            var calls = new List<int>();
            Assert.Throws<InvalidOperationException>(() => BaboonTools.SetHashcode(values, value =>
            {
                calls.Add(value);
                if (value == 2) throw new InvalidOperationException("hash failed");
                return value;
            }));
            Assert.That(calls, Is.EqualTo(new[] { 1, 2 }));
        }
    }
}

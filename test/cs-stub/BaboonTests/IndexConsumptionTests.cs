#nullable enable
using System;
using System.IO;
using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class IndexConsumptionTests
    {
        private sealed class TwoEntries : IBaboonBinCodecIndexed
        {
            public ushort IndexElementsCount(BaboonCodecContext ctx) => 2;
        }

        private sealed class NonSeekingStream : MemoryStream
        {
            public NonSeekingStream(byte[] bytes) : base(bytes) { }
            public override bool CanSeek => false;
            public override long Position
            {
                get => throw new NotSupportedException();
                set => throw new NotSupportedException();
            }
        }

        [TestCase(false, false)]
        [TestCase(true, false)]
        [TestCase(false, true)]
        [TestCase(true, true)]
        public void ConsumesOnlyHeaderAndEntries(bool indexed, bool consumeOnly)
        {
            using var data = new MemoryStream();
            using (var writer = new BinaryWriter(data, System.Text.Encoding.UTF8, true))
            {
                writer.Write((byte)(indexed ? 1 : 0));
                if (indexed)
                {
                    writer.Write((uint)0);
                    writer.Write((uint)2);
                    writer.Write((uint)2);
                    writer.Write((uint)3);
                }
                writer.Write((byte)0x42);
            }
            IBaboonBinCodecIndexed codec = new TwoEntries();
            using var reader = new BinaryReader(new NonSeekingStream(data.ToArray()));
            var count = consumeOnly ? codec.ConsumeIndex(BaboonCodecContext.Compact, reader) : codec.ReadIndex(BaboonCodecContext.Compact, reader).Count;
            Assert.That(count, Is.EqualTo(indexed ? 2 : 0));
            Assert.That(reader.ReadByte(), Is.EqualTo(0x42));
        }

        [TestCase(false)]
        [TestCase(true)]
        public void RejectsTruncatedPairs(bool consumeOnly)
        {
            IBaboonBinCodecIndexed codec = new TwoEntries();
            using var reader = new BinaryReader(new MemoryStream(new byte[] { 1, 0 }));
            Assert.Throws<EndOfStreamException>(() =>
            {
                if (consumeOnly) codec.ConsumeIndex(BaboonCodecContext.Compact, reader);
                else codec.ReadIndex(BaboonCodecContext.Compact, reader);
            });
        }
    }
}

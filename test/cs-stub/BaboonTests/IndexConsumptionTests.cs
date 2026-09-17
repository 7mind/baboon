#nullable enable
using System;
using System.IO;
using Baboon.Runtime.Shared;
using NUnit.Framework;
using Testpkg.Pkg0;

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

        [TestCase(0, 0, 1, 1)]
        [TestCase(-1, 1, 1, 1)]
        [TestCase(0, -1, 1, 1)]
        [TestCase(0, 2, 1, 1)]
        [TestCase(int.MaxValue, 1, 0, 1)]
        public void RejectsInvalidEntriesInRelease(int firstOffset, int firstLength, int secondOffset, int secondLength)
        {
            using var data = new MemoryStream();
            using (var writer = new BinaryWriter(data, System.Text.Encoding.UTF8, true))
            {
                writer.Write((byte)1);
                writer.Write(firstOffset);
                writer.Write(firstLength);
                writer.Write(secondOffset);
                writer.Write(secondLength);
            }
            IBaboonBinCodecIndexed codec = new TwoEntries();
            foreach (var consumeOnly in new[] { false, true })
            {
                using var reader = new BinaryReader(new MemoryStream(data.ToArray()));
                Assert.Throws<InvalidDataException>(() =>
                {
                    if (consumeOnly) codec.ConsumeIndex(BaboonCodecContext.Compact, reader);
                    else codec.ReadIndex(BaboonCodecContext.Compact, reader);
                });
            }
        }

        [TestCase(0)]
        [TestCase(2)]
        public void RejectsIncorrectFixedLengthsInRelease(int actual)
        {
            IBaboonBinCodecIndexed codec = new TwoEntries();
            using var buffer = new MemoryStream();
            using var writer = new BinaryWriter(buffer);
            Assert.Throws<InvalidDataException>(() => codec.WriteIndexFixedLenField(writer, 1, () => writer.Write(new byte[actual])));
        }

        [Test]
        public void RejectsEmptyVariableFieldsInRelease()
        {
            IBaboonBinCodecIndexed codec = new TwoEntries();
            using var buffer = new MemoryStream();
            using var index = new MemoryStream();
            using var writer = new BinaryWriter(index);
            using var fakeWriter = new BinaryWriter(buffer);
            Assert.Throws<InvalidDataException>(() => codec.WriteIndexVarLenField(writer, fakeWriter, () => { }));
        }

        [Test]
        public void GeneratedDecoderRejectsMissingRequiredIndexInRelease()
        {
            using var buffer = new MemoryStream();
            using var writer = new BinaryWriter(buffer);
            var codec = T1_E2_RET_UEBACodec.Instance;
            codec.Encode(BaboonCodecContext.Compact, writer, new T1_E2_RET(T1_E2.A, null));
            buffer.Position = 0;
            using var reader = new BinaryReader(buffer);
            Assert.Catch<Exception>(() => codec.Decode(BaboonCodecContext.Indexed, reader));
        }

        [Test]
        public void GeneratedBranchDecoderRejectsCorruptPrefixInRelease()
        {
            using var buffer = new MemoryStream();
            using var writer = new BinaryWriter(buffer);
            var codec = T5_A1.B1_UEBACodec.Instance;
            codec.Encode(BaboonCodecContext.Compact, writer, new T5_A1.B1("x"));
            var bytes = buffer.ToArray();
            bytes[0] = 255;
            using var reader = new BinaryReader(new MemoryStream(bytes));
            Assert.Catch<Exception>(() => codec.Decode(BaboonCodecContext.Compact, reader));
        }
    }
}

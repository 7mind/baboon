#nullable enable

using System.Collections.Generic;
using System.IO;
using System.Linq;
using System;
using System.Collections.Immutable;
using System.Diagnostics;
using Newtonsoft.Json.Linq;

// ReSharper disable UnusedTypeParameter
// ReSharper disable CheckNamespace
// ReSharper disable UnusedAutoPropertyAccessor.Global
// ReSharper disable MemberCanBeProtected.Global
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable ConvertToPrimaryConstructor
// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedMemberInSuper.Global
// ReSharper disable UseCollectionExpression
// ReSharper disable ReplaceAutoPropertyWithComputedProperty
// ReSharper disable ArrangeNamespaceBody
// ReSharper disable UnusedType.Global
// ReSharper disable InconsistentNaming
// ReSharper disable ClassCanBeSealed.Global

namespace Baboon.Runtime.Shared
{
    public interface IBaboonCodecData
    {
        public string BaboonDomainVersion();
        public string BaboonDomainIdentifier();
        public string BaboonTypeIdentifier();
    }

    public class BaboonCodecContext
    {
        private BaboonCodecContext(bool useIndexes)
        {
            UseIndices = useIndexes;
        }

        public bool UseIndices { get; }

        public static BaboonCodecContext Indexed { get; } = new(true);
        public static BaboonCodecContext Compact { get; } = new(false);
        public static BaboonCodecContext Default { get; } = Compact;
    }

    public interface IBaboonCodec<T> : IBaboonCodecData
    {
    }

    public interface IBaboonValueCodec<T, TWire> : IBaboonCodec<T>
    {
        // json codec should always ignore context
        TWire Encode(BaboonCodecContext ctx, T instance);
        T Decode(BaboonCodecContext ctx, TWire wire);
    }

    public interface IBaboonJsonCodec<T> : IBaboonValueCodec<T, JToken>
    {
    }

    public static class IBaboonJsonCodec
    {
        public abstract class Base<T, TCodec> : BaboonSingleton<IBaboonJsonCodec<T>, TCodec>, IBaboonJsonCodec<T>
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            public abstract string BaboonDomainVersion();
            public abstract string BaboonDomainIdentifier();
            public abstract string BaboonTypeIdentifier();

            public abstract JToken Encode(BaboonCodecContext ctx, T instance);
            public abstract T Decode(BaboonCodecContext ctx, JToken wire);
        }

        public abstract class BaseGenerated<T, TCodec> : Base<T, TCodec>, IBaboonJsonCodec<IBaboonGenerated>
            where T : IBaboonGenerated
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            JToken IBaboonValueCodec<IBaboonGenerated, JToken>.Encode(BaboonCodecContext ctx, IBaboonGenerated value)
            {
                if (value is not T cast) throw new Exception($"Expected to have {(typeof(T)).Name} type");
                return Encode(ctx, cast);
            }

            IBaboonGenerated IBaboonValueCodec<IBaboonGenerated, JToken>.Decode(BaboonCodecContext ctx, JToken wire)
            {
                return Decode(ctx, wire);
            }
        }

        public abstract class BaseGeneratedAdt<T, TCodec> : BaseGenerated<T, TCodec>, IBaboonAdtMemberMeta
            where T : IBaboonGenerated
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            public abstract string BaboonAdtTypeIdentifier();
        }

        public abstract class NoEncoder<T, TCodec> : Base<T, TCodec>
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            public override JToken Encode(BaboonCodecContext ctx, T instance)
            {
                if (this != LazyInstance.Value) return LazyInstance.Value.Encode(ctx, instance);
                throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
            }
        }

        public abstract class NoEncoderGenerated<T, TCodec> : BaseGenerated<T, TCodec>
            where T : IBaboonGenerated
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            public override JToken Encode(BaboonCodecContext ctx, T instance)
            {
                if (this != LazyInstance.Value) return LazyInstance.Value.Encode(ctx, instance);
                throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
            }
        }

        public abstract class NoEncoderGeneratedAdt<T, TCodec> : BaseGeneratedAdt<T, TCodec>
            where T : IBaboonGenerated
            where TCodec : IBaboonJsonCodec<T>, new()
        {
            public override JToken Encode(BaboonCodecContext ctx, T instance)
            {
                if (this != LazyInstance.Value) return LazyInstance.Value.Encode(ctx, instance);
                throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
            }
        }
    }

    public interface IBaboonStreamCodec<T, in TOut, in TIn> : IBaboonCodec<T>
    {
        void Encode(BaboonCodecContext ctx, TOut writer, T instance);
        T Decode(BaboonCodecContext ctx, TIn wire);
    }

    public interface IBaboonBinCodecIndexed
    {
        // ReSharper disable once UnusedParameter.Global
        ushort IndexElementsCount(BaboonCodecContext ctx);

        List<BaboonIndexEntry> ReadIndex(BaboonCodecContext ctx, BinaryReader wire)
        {
            var header = wire.ReadByte();
            var isIndexed = (header & 0b0000001) != 0;
            var result = new List<BaboonIndexEntry>();
            uint prevoffset = 0;
            uint prevlen = 0;
            // ReSharper disable once InvertIf
            if (isIndexed)
            {
                var left = IndexElementsCount(ctx);
                while (left > 0)
                {
                    var offset = wire.ReadUInt32();
                    var len = wire.ReadUInt32();
                    Debug.Assert(len > 0);
                    Debug.Assert(offset >= prevoffset + prevlen);
                    result.Add(new BaboonIndexEntry(offset, len));
                    left = (ushort) (left - 1);
                    prevoffset = offset;
                    prevlen = len;
                }
            }

            return result;
        }

        void WriteIndexFixedLenField(BinaryWriter writer, int expected, Action doWrite)
        {
            var before = (uint) writer.BaseStream.Position;
            doWrite();
            var after = (uint) writer.BaseStream.Position;
            var length = after - before;
            Debug.Assert(length == expected);
            Debug.Assert(after >= before, $"Got after={after}, before={before}");
        }

        uint WriteIndexVarLenField(BinaryWriter writer, BinaryWriter fakeWriter, Action doWrite)
        {
            var before = (uint) fakeWriter.BaseStream.Position;
            doWrite();
            var after = (uint) fakeWriter.BaseStream.Position;
            var length = after - before;
            writer.Write(before);
            writer.Write(length);
            Debug.Assert(after >= before, $"Got after={after}, before={before}");
            return length;
        }
    }

    public interface IBaboonBinCodec<T> : IBaboonStreamCodec<T, BinaryWriter, BinaryReader>
    {
        void EncodeIndexed(BinaryWriter writer, T instance)
        {
            var ctx = BaboonCodecContext.Indexed;
            Encode(ctx, writer, instance);
        }

        void EncodeCompact(BinaryWriter writer, T instance)
        {
            var ctx = BaboonCodecContext.Compact;
            Encode(ctx, writer, instance);
        }

        T DecodeAny(BinaryReader wire)
        {
            var ctx = BaboonCodecContext.Default;
            return Decode(ctx, wire);
        }
    }

    public static class IBaboonBinCodec
    {
        public abstract class Base<T, TCodec> : BaboonSingleton<IBaboonBinCodec<T>, TCodec>, IBaboonBinCodec<T>, IBaboonBinCodecIndexed
            where TCodec : IBaboonBinCodec<T>, new()
        {
            public abstract string BaboonDomainVersion();
            public abstract string BaboonDomainIdentifier();
            public abstract string BaboonTypeIdentifier();

            public abstract void Encode(BaboonCodecContext ctx, BinaryWriter writer, T instance);
            public abstract T Decode(BaboonCodecContext ctx, BinaryReader wire);
            public abstract ushort IndexElementsCount(BaboonCodecContext ctx);
        }

        public abstract class BaseGenerated<T, TCodec> : Base<T, TCodec>, IBaboonBinCodec<IBaboonGenerated>
            where T : IBaboonGenerated
            where TCodec : IBaboonBinCodec<T>, new()
        {
            void IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>.Encode(BaboonCodecContext ctx, BinaryWriter writer, IBaboonGenerated value)
            {
                if (value is not T cast) throw new Exception($"Expected to have {typeof(T).Name} type");
                Encode(ctx, writer, cast);
            }

            IBaboonGenerated IBaboonStreamCodec<IBaboonGenerated, BinaryWriter, BinaryReader>.Decode(BaboonCodecContext ctx, BinaryReader wire)
            {
                return Decode(ctx, wire);
            }
        }

        public abstract class BaseGeneratedAdt<T, TCodec> : BaseGenerated<T, TCodec>, IBaboonAdtMemberMeta
            where T : IBaboonGenerated
            where TCodec : IBaboonBinCodec<T>, new()
        {
            public abstract string BaboonAdtTypeIdentifier();
        }

        public abstract class NoEncoder<T, TCodec> : Base<T, TCodec>
            where TCodec : IBaboonBinCodec<T>, new()
        {
            public override void Encode(BaboonCodecContext ctx, BinaryWriter writer, T instance)
            {
                if (this == LazyInstance.Value)
                {
                    throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
                }

                LazyInstance.Value.Encode(ctx, writer, instance);
            }
        }

        public abstract class NoEncoderGenerated<T, TCodec> : BaseGenerated<T, TCodec>
            where T : IBaboonGenerated
            where TCodec : IBaboonBinCodec<T>, new()
        {
            public override void Encode(BaboonCodecContext ctx, BinaryWriter writer, T instance)
            {
                if (this == LazyInstance.Value)
                {
                    throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
                }

                LazyInstance.Value.Encode(ctx, writer, instance);
            }
        }

        public abstract class NoEncoderGeneratedAdt<T, TCodec> : BaseGeneratedAdt<T, TCodec>
            where T : IBaboonGenerated
            where TCodec : IBaboonBinCodec<T>, new()
        {
            public override void Encode(BaboonCodecContext ctx, BinaryWriter writer, T instance)
            {
                if (this == LazyInstance.Value)
                {
                    throw new Exception($"Type {BaboonTypeIdentifier()}@{{{BaboonDomainVersion()}}} is deprecated, encoder was not generated.");
                }

                LazyInstance.Value.Encode(ctx, writer, instance);
            }
        }
    }

    public class BaboonIndexEntry
    {
        public uint Offset { get; }
        public uint Length { get; }

        public BaboonIndexEntry(uint offset, uint length)
        {
            Offset = offset;
            Length = length;
        }
    }

    public interface IBaboonTypeCodecs
    {
        public string Id { get; }
        public IBaboonCodecData Impl { get; }
    }

    public sealed record BaboonTypeCodecs(string Id, Lazy<IBaboonCodecData> LazyValue) : IBaboonTypeCodecs
    {
        public IBaboonCodecData Impl => LazyValue.Value;
    }

    public abstract class AbstractBaboonCodecs
    {
        private readonly Dictionary<string, IBaboonTypeCodecs> _codecs = new();

        public void Register(string id, Lazy<IBaboonCodecData> impl)
        {
            _codecs[id] = new BaboonTypeCodecs(id, impl);
        }

        public IBaboonTypeCodecs Find(string id)
        {
            return _codecs[id];
        }

        public bool TryFind(string id, out IBaboonTypeCodecs? value)
        {
            return _codecs.TryGetValue(id, out value);
        }
    }

}
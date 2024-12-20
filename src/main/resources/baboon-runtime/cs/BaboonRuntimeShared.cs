#nullable enable

#pragma warning disable 612,618

using System.Linq;

#nullable enable

#pragma warning disable 612,618

using System.Linq;

#nullable enable

#pragma warning disable 612,618

using System.Linq;

using System.Collections.Generic;
using System.IO;
using Newtonsoft.Json.Linq;
using System;
using System.Diagnostics;

namespace Baboon.Runtime.Shared {
    public interface IBaboonGenerated {
        public String BaboonDomainVersion();
        public String BaboonDomainIdentifier();
        public String BaboonUnmodifiedSinceVersion();
        public String BaboonTypeIdentifier();
    }

    public interface IBaboonAdtMemberMeta {
        public String BaboonAdtTypeIdentifier();
    }

    public interface IBaboonMeta {
        public String UnmodifiedSince(String typeIdString);
    }

    public interface IBaboonGeneratedLatest : IBaboonGenerated {}

    public interface IConversion {
        public Type TypeFrom();
        public String VersionFrom();
        public Type TypeTo();
        public String VersionTo();
        public String TypeId();
    }

    public interface IBaboonGeneratedConversion : IConversion
    {
        public IBaboonGenerated Convert<TC>(TC? context, AbstractBaboonConversions conversions, IBaboonGenerated from);
    }

    public abstract class AbstractConversion<TFrom, TTo> : IBaboonGeneratedConversion
    {
        public TTo Convert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, TFrom from) {
            if (from is IBaboonGenerated bgf)
            {
                if (TypeId() != bgf.BaboonTypeIdentifier())
                {
                    throw new ArgumentException($"Provided instance is {bgf.BaboonTypeIdentifier()} but must be {TypeId()}");
                }
            }

            var result = DoConvert(context, conversions, from);

            if (result is IBaboonGenerated bgr)
            {
                if (TypeId() != bgr.BaboonTypeIdentifier())
                {
                    throw new ArgumentException($"Produced instance is {bgr.BaboonTypeIdentifier()} but must be {TypeId()}");
                }
            }

            return result;
        }

        protected abstract TTo DoConvert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, TFrom from);

        IBaboonGenerated IBaboonGeneratedConversion.Convert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, IBaboonGenerated from) where TCtx : default
        {
            if (from is not TFrom fr)
            {
                throw new Exception($"Can't use IBaboonGeneratedConversion interface when 'from' is not of type {typeof(TFrom).FullName}");
            }
            var res = Convert(context, conversions, fr);

            if (res is not IBaboonGenerated bg)
            {
                throw new ArgumentException($"Can't use IBaboonGeneratedConversion interface for non IBaboonGenerated return type To = {typeof(TTo).FullName}");
            }
            return bg;
        }

        public Type TypeFrom() {
             return typeof(TFrom);
        }

         public Type TypeTo() {
             return typeof(TTo);
         }

         public abstract String VersionFrom();

         public abstract String VersionTo();

         public abstract String TypeId();
    }

    public interface IBaboonCodecData {
        public String BaboonDomainVersion();
        public String BaboonDomainIdentifier();
        public String BaboonTypeIdentifier();
    }

    public class BaboonCodecContext {
        public BaboonCodecContext(bool useIndexes)
        {
            UseIndices = useIndexes;
        }

        public Boolean UseIndices { get; }

        public static BaboonCodecContext Indexed { get; } = new BaboonCodecContext(true);
        public static BaboonCodecContext Compact { get; } = new BaboonCodecContext(false);
        public static BaboonCodecContext Default { get; } = Compact;

    }

    public interface IBaboonCodec<T> : IBaboonCodecData {}

    public interface IBaboonValueCodec<T, TWire> : IBaboonCodec<T>
    {
        // json codec should always ignore context
        TWire Encode(BaboonCodecContext ctx, T instance);
        T Decode(BaboonCodecContext ctx, TWire wire);
    }

    public interface IBaboonJsonCodec<T> : IBaboonValueCodec<T, JToken> {}

    public interface IBaboonStreamCodec<T, in TOut, in TIn> : IBaboonCodec<T>
    {
        void Encode(BaboonCodecContext ctx, TOut writer, T instance);
        T Decode(BaboonCodecContext ctx, TIn wire);
    }

    public interface IBaboonBinCodecIndexed
    {
        UInt16 IndexElementsCount(BaboonCodecContext ctx);

        List<BaboonIndexEntry> ReadIndex(BaboonCodecContext ctx, BinaryReader wire)
        {
            var header = wire.ReadByte();
            var isIndexed = (header & 0b0000001) != 0;
            var result = new List<BaboonIndexEntry>();
            uint prevoffset = 0;
            uint prevlen = 0;
            if (isIndexed)
            {
                var left = IndexElementsCount(ctx);
                while (left > 0)
                {
                    var offset = wire.ReadUInt32();
                    var len = wire.ReadUInt32();
                    Debug.Assert(offset >= 0);
                    Debug.Assert(len > 0);
                    Debug.Assert(offset >= prevoffset + prevlen);
                    result.Add(new BaboonIndexEntry(offset, len));
                    left = (ushort)(left - 1);
                    prevoffset = offset;
                    prevlen = len;
                }
            }
            return result;
        }
    }

    public interface IBaboonBinCodec<T> : IBaboonStreamCodec<T, BinaryWriter, BinaryReader>
    {
        void EncodeIndexed(BinaryWriter writer, T instance)
        {
            BaboonCodecContext ctx = BaboonCodecContext.Indexed;
            Encode(ctx, writer, instance);
        }

        void EncodeCompact(BinaryWriter writer, T instance)
        {
            BaboonCodecContext ctx = BaboonCodecContext.Compact;
            Encode(ctx, writer, instance);
        }

        T DecodeAny(BinaryReader wire)
        {
            BaboonCodecContext ctx = BaboonCodecContext.Default;
            return Decode(ctx, wire);
        }
    }

    public class BaboonIndexEntry
    {
        public uint Offset { get; }
        public uint Length { get; }

        public BaboonIndexEntry(uint offset, uint length)
        {
            this.Offset = offset;
            this.Length = length;
        }
    }

    public interface IBaboonTypeCodecs {
        public String Id { get; }
        public IBaboonCodecData Json { get; }
        public IBaboonCodecData Ueba { get; }
    }

    public sealed record BaboonTypeCodecs(String Id, Lazy<IBaboonCodecData> LazyJson, Lazy<IBaboonCodecData> LazyUeba) : IBaboonTypeCodecs {
        public IBaboonCodecData Json => LazyJson.Value;
        public IBaboonCodecData Ueba => LazyUeba.Value;
    };

    public sealed record BaboonTypeCodecs<T>(String Id, Lazy<IBaboonJsonCodec<T>> LazyJson, Lazy<IBaboonBinCodec<T>> LazyUeba) : IBaboonTypeCodecs {
        public IBaboonCodecData Json => LazyJson.Value;
        public IBaboonCodecData Ueba => LazyUeba.Value;
    };

    public abstract class AbstractBaboonCodecs
    {

        private readonly Dictionary<String, IBaboonTypeCodecs> _codecs = new ();

        public void Register(IBaboonTypeCodecs impls)
        {
            _codecs[impls.Id] = impls;
        }

        public IBaboonTypeCodecs Find(String id)
        {
            return _codecs[id];
        }

        public bool TryFind(String id, out IBaboonTypeCodecs? value)
        {
            return _codecs.TryGetValue(id, out value);
        }
    }

    public static class BaboonTools {
        public static T? ReadNullableValueType<T>(Boolean ifNot, Func<T> thenReturn) where T: struct
        {
            if (ifNot) return null;
            return thenReturn();
        }

        public static T? ReadNullableValueType<T>(JToken? token, Func<JToken, T> readValue) where T: struct
        {
            if (token == null || token.Type == JTokenType.Null) return null;
            return readValue(token);
        }

        public static T? ReadNullableReferentialType<T>(JToken? token, Func<JToken, T> readValue) where T: class
        {
            if (token == null || token.Type == JTokenType.Null) return null;
            return readValue(token);
        }
    }


    public sealed class ConversionKey
    {
        protected bool Equals(ConversionKey other)
        {
            return TypeFrom.Equals(other.TypeFrom) && TypeTo.Equals(other.TypeTo);
        }

        public override bool Equals(object? obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            if (obj.GetType() != this.GetType()) return false;
            return Equals((ConversionKey)obj);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(TypeFrom, TypeTo);
        }

        public ConversionKey(Type typeFrom, Type typeTo)
        {
            TypeFrom = typeFrom;
            TypeTo = typeTo;
        }

        public Type TypeFrom { get; }
        public Type TypeTo {get; }
    }

    public abstract class AbstractBaboonConversions
    {
        private readonly Dictionary<ConversionKey, IConversion> _convs = new ();
        private readonly Dictionary<Type, List<IConversion>> _convsWild = new ();

        public abstract List<String> VersionsFrom();

        public abstract String VersionTo();

        public List<IConversion> AllConversions()
        {
            return _convs.Values.ToList();
        }

        public void Register(IConversion conversion)
        {
            var fromType = conversion.TypeFrom();
            var key = new ConversionKey(fromType, conversion.TypeTo());
            var wild = _convsWild.TryGetValue(fromType, out var v) ? v : new List<IConversion>();
            wild.Add(conversion);
            _convs[key] = conversion;
            _convsWild[fromType] = wild;
        }

        public void Register<TFrom, TTo>(AbstractConversion<TFrom, TTo> conversion)
        {
            var tFrom = typeof(TFrom);
            var tTo = typeof(TTo);
            var key = new ConversionKey(tFrom, tTo);
            var wild = _convsWild.TryGetValue(tFrom, out var v) ? v : new List<IConversion>();
            wild.Add(conversion);
            _convs[key] = conversion;
            _convsWild[tFrom] = wild;
        }

        public IBaboonGenerated ConvertWithContext<T>(T? c, IBaboonGenerated from, IConversion conversion)
        {
            var tconv = (IBaboonGeneratedConversion)conversion;
            return tconv.Convert<T>(c, this, from);
        }

        public IBaboonGenerated Convert(IBaboonGenerated from, IConversion conversion)
        {
            var tconv = (IBaboonGeneratedConversion)conversion;
            return tconv.Convert<Object>(null, this, from);
        }

        public IReadOnlyList<IConversion> FindConversions(IBaboonGenerated value)
        {
            return !_convsWild.TryGetValue(value.GetType(), out var res) ? new List<IConversion>() : res;
        }

        public TTo ConvertWithContext<T, TFrom, TTo>(T? c, TFrom from)
        {
            var tFrom = typeof(TFrom);
            var tTo = typeof(TTo);

            if (from is TTo direct)
            {
                return direct;
            }
            var key = new ConversionKey(tFrom, tTo);

            var conv = _convs[key];
            var tconv = ((AbstractConversion<TFrom, TTo>)conv);
            return tconv.Convert(c, this, from);
        }

        public TTo Convert<TFrom, TTo>(TFrom from)
            where TFrom : IBaboonGenerated
            where TTo : IBaboonGenerated
        {
            return ConvertWithContext<Object, TFrom, TTo>(null, from);
        }

    }
}
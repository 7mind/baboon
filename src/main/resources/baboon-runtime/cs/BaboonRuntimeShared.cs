using System.Collections.Generic;
using System.IO;
using System.Linq;
using System;
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

namespace Baboon.Runtime.Shared {
    public static class BaboonEnumerable
    {
        public static List<TSource> BbnToList<TSource>(this IEnumerable<TSource> source)
        {
            if (source is List<TSource> l)
            {
                return l;
            }

            return source.ToList();
        }

        public static Dictionary<TKey, TElement> BbnToDictionary<TKey, TElement>(
            this IEnumerable<KeyValuePair<TKey, TElement>> source)
            where TKey : notnull
        {
            if (source is Dictionary<TKey, TElement> d)
            {
                return d;
            }

            return new Dictionary<TKey, TElement>(source);
        }
    }

    public interface IBaboonGenerated {
        public string BaboonDomainVersion();
        public string BaboonDomainIdentifier();
        public string BaboonUnmodifiedSinceVersion();
        public string BaboonTypeIdentifier();
    }

    public interface IBaboonAdtMemberMeta {
        public string BaboonAdtTypeIdentifier();
    }

    public interface IBaboonMeta {
        public string UnmodifiedSince(string typeIdString);
    }

    public interface IBaboonGeneratedLatest : IBaboonGenerated
    {
    }

    public interface IConversion {
        public Type TypeFrom();
        public string VersionFrom();
        public Type TypeTo();
        public string VersionTo();
        public string TypeId();
    }

    public interface IBaboonGeneratedConversion : IConversion
    {
        public IBaboonGenerated Convert<TC>(TC? context, AbstractBaboonConversions conversions, IBaboonGenerated from);
    }

    public abstract class AbstractConversion<TFrom, TTo> : IBaboonGeneratedConversion
    {
        protected void ValidateBaboonType(object? obj) {
            if (obj is IBaboonGenerated bgf)
            {
                var tid = TypeId();
                var conversionTypeIsExactType = tid == bgf.BaboonTypeIdentifier(); 
                if (obj is IBaboonAdtMemberMeta bga) {
                    var conversionTypeIsAdtType = tid == bga.BaboonAdtTypeIdentifier(); 
                    if (!conversionTypeIsAdtType && !conversionTypeIsExactType)
                    {
                        throw new ArgumentException($"Provided instance is adt={bga.BaboonAdtTypeIdentifier()} exact={bgf.BaboonTypeIdentifier()} one of which must be {tid}");
                    }
                }
                else
                if (!conversionTypeIsExactType)
                {
                    throw new ArgumentException($"Provided instance is {bgf.BaboonTypeIdentifier()} but must be {tid}");
                }
            }
        
        }
        public TTo Convert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, TFrom from) {
            ValidateBaboonType(from);            
            var result = DoConvert(context, conversions, from);
            ValidateBaboonType(result);
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

         public abstract string VersionFrom();

         public abstract string VersionTo();

         public abstract string TypeId();
    }

    public interface IBaboonCodecData {
        public string BaboonDomainVersion();
        public string BaboonDomainIdentifier();
        public string BaboonTypeIdentifier();
    }

    public class BaboonCodecContext {
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
                    left = (ushort)(left - 1);
                    prevoffset = offset;
                    prevlen = len;
                }
            }
            return result;
        }
        
        void WriteIndexFixedLenField(BinaryWriter writer, int expected, Action doWrite)
        {
            var before = (uint)writer.BaseStream.Position;
            doWrite();
            var after = (uint)writer.BaseStream.Position;
            var length = after - before;
            Debug.Assert(length == expected);
            Debug.Assert(after >= before, $"Got after={after}, before={before}");
        }
        
        uint WriteIndexVarLenField(BinaryWriter writer, BinaryWriter fakeWriter, Action doWrite)
        {
            var before = (uint)fakeWriter.BaseStream.Position;
            doWrite();
            var after = (uint)fakeWriter.BaseStream.Position;
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

    public interface IBaboonTypeCodecs {
        public string Id { get; }
        public IBaboonCodecData Json { get; }
        public IBaboonCodecData Ueba { get; }
    }

    public sealed record BaboonTypeCodecs(string Id, Lazy<IBaboonCodecData> LazyJson, Lazy<IBaboonCodecData> LazyUeba) : IBaboonTypeCodecs {
        public IBaboonCodecData Json => LazyJson.Value;
        public IBaboonCodecData Ueba => LazyUeba.Value;
    }

    public sealed record BaboonTypeCodecs<T>(string Id, Lazy<IBaboonJsonCodec<T>> LazyJson, Lazy<IBaboonBinCodec<T>> LazyUeba) : IBaboonTypeCodecs {
        public IBaboonCodecData Json => LazyJson.Value;
        public IBaboonCodecData Ueba => LazyUeba.Value;
    }

    public abstract class AbstractBaboonCodecs
    {

        private readonly Dictionary<string, IBaboonTypeCodecs> _codecs = new ();

        public void Register(IBaboonTypeCodecs impls)
        {
            _codecs[impls.Id] = impls;
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

    public static class BaboonTools {
        public static T? ReadNullableValueType<T>(bool ifNot, Func<T> thenReturn) where T: struct
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
        
        public static int OptionHashcode<T>(T? value, Func<T, int> mk)
        {
            return value == null ? 0 : mk(value);
        }
        
        public static int SeqHashcode<T>(IEnumerable<T> value, Func<T, int> mk)
        {
             return value.Aggregate(0x1EAFDEAD, (current, item) => current ^ mk(item));
        }
        
        public static int SetHashcode<T>(ISet<T> value, Func<T, int> mk)
        {
             return value.Select(item => mk(item)).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, item) => current ^ item);
        }
        
        public static int MapHashcode<TKey, TValue>(IEnumerable<KeyValuePair<TKey, TValue>> value, Func<TKey, int> mkk, Func<TValue, int> mkv) {
            return (value.Select(item0 => HashCode.Combine(mkk(item0.Key), mkv(item0.Value))).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, item0) => current ^ item0));
        }
    }


    public sealed class ConversionKey
    {
        private bool Equals(ConversionKey other)
        {
            // reference checks are considered safe but there are assembly-related edgecases
            // ReSharper disable CheckForReferenceEqualityInstead.1
            return TypeFrom.Equals(other.TypeFrom) && TypeTo.Equals(other.TypeTo);
            // ReSharper restore CheckForReferenceEqualityInstead.1
        }

        public override bool Equals(object? obj)
        {
            if (ReferenceEquals(null, obj)) return false;
            if (ReferenceEquals(this, obj)) return true;
            return obj.GetType() == GetType() && Equals((ConversionKey)obj);
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
        
        public override string ToString()
        {
            return $"{TypeFrom}=>{TypeTo}";
        }

        public Type TypeFrom { get; }
        public Type TypeTo {get; }
    }

    public abstract class AbstractBaboonConversions
    {
        private readonly Dictionary<ConversionKey, IConversion> _convs = new ();
        private readonly Dictionary<Type, List<IConversion>> _convsWild = new ();

        public abstract List<string> VersionsFrom();

        public abstract string VersionTo();

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
            return tconv.Convert(c, this, from);
        }

        public IBaboonGenerated Convert(IBaboonGenerated from, IConversion conversion)
        {
            var tconv = (IBaboonGeneratedConversion)conversion;
            return tconv.Convert<object>(null, this, from);
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
            var tconv = (AbstractConversion<TFrom, TTo>)conv;
            return tconv.Convert(c, this, from);
        }

        public TTo Convert<TFrom, TTo>(TFrom from)
            where TFrom : IBaboonGenerated
            where TTo : IBaboonGenerated
        {
            return ConvertWithContext<object, TFrom, TTo>(null, from);
        }
        
        public sealed class ConvertDslFrom<TFrom>
            where TFrom : IBaboonGenerated
        {
            private readonly TFrom _from;
            private readonly AbstractBaboonConversions _convs;

            public ConvertDslFrom(TFrom from, AbstractBaboonConversions convs)
            {
                this._from = from;
                this._convs = convs;
            }

            public TTo To<TTo>() 
                where TTo : IBaboonGenerated
            {
                return _convs.Convert<TFrom, TTo>(_from);
            }
        }

        public ConvertDslFrom<TFrom> Convert<TFrom>(TFrom from)
            where TFrom : IBaboonGenerated
        {
            return new ConvertDslFrom<TFrom>(from, this);
        }
    }
    
    public abstract record Either<TLeft, TRight>
    {
        private Either() {}
    
        public sealed record Left(TLeft Value) : Either<TLeft, TRight>;
    
        public sealed record Right(TRight Value) : Either<TLeft, TRight>;
    }
    
    public static class Either
    {
        public static Either<TLeft, TRight> Left<TLeft, TRight>(TLeft value) => new Either<TLeft, TRight>.Left(value);
        public static Either<TLeft, TRight> Right<TLeft, TRight>(TRight value) => new Either<TLeft, TRight>.Right(value);
    }
    
    public class Unit
    {
        private Unit() { }
        public static readonly Unit Default = new Unit();
    
        public override string ToString()
        {
            return "()";
        }
    }
    
    public static class BaboonTestTools {
        public static void WriteBinaryFile(string filePath, byte[] data)
        {
            string directoryPath = Path.GetDirectoryName(filePath);
            if (!string.IsNullOrEmpty(directoryPath) && !Directory.Exists(directoryPath))
            {
                Directory.CreateDirectory(directoryPath);
            }
            File.WriteAllBytes(filePath, data);
        }
    }

    public abstract class BaboonSingleton<T, IMPL>  where IMPL : T, new() {
        protected static Lazy<T> LazyInstance = new Lazy<T>(() => new IMPL());
        public static T Instance { get { return LazyInstance.Value; } set { LazyInstance = new Lazy<T>(() => value); } }
    }
}
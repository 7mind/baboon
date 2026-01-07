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
    public interface IConversion
    {
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
        public abstract string VersionFrom();

        public abstract string VersionTo();

        public abstract string TypeId();

        public Type TypeFrom()
        {
            return typeof(TFrom);
        }

        public Type TypeTo()
        {
            return typeof(TTo);
        }

        protected void ValidateBaboonType(object? obj)
        {
            if (obj is IBaboonGenerated bgf)
            {
                var tid = TypeId();
                var conversionTypeIsExactType = tid == bgf.BaboonTypeIdentifier();
                if (obj is IBaboonAdtMemberMeta bga)
                {
                    var conversionTypeIsAdtType = tid == bga.BaboonAdtTypeIdentifier();
                    if (!conversionTypeIsAdtType && !conversionTypeIsExactType)
                    {
                        throw new ArgumentException(
                            $"Provided instance is adt={bga.BaboonAdtTypeIdentifier()} exact={bgf.BaboonTypeIdentifier()} one of which must be {tid}");
                    }
                }
                else if (!conversionTypeIsExactType)
                {
                    throw new ArgumentException($"Provided instance is {bgf.BaboonTypeIdentifier()} but must be {tid}");
                }
            }
        }

        protected virtual bool ConversionValidationEnabled()
        {
            return true;
        }

        public TTo Convert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, TFrom from)
        {
            var result = DoConvert(context, conversions, from);
            if (ConversionValidationEnabled())
            {
              ValidateBaboonType(from);
              ValidateBaboonType(result);
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
            return obj.GetType() == GetType() && Equals((ConversionKey) obj);
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
        public Type TypeTo { get; }
    }

    public abstract class AbstractBaboonConversions
    {
        private readonly Dictionary<ConversionKey, IConversion> _convs = new();
        private readonly Dictionary<Type, List<IConversion>> _convsWild = new();

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
            var tconv = (IBaboonGeneratedConversion) conversion;
            return tconv.Convert(c, this, from);
        }

        public IBaboonGenerated Convert(IBaboonGenerated from, IConversion conversion)
        {
            var tconv = (IBaboonGeneratedConversion) conversion;
            return tconv.Convert<object>(null, this, from);
        }

        public IReadOnlyList<IConversion> FindConversions(IBaboonGenerated value)
        {
            if (_convsWild.TryGetValue(value.GetType(), out var tpeConv))
            {
                return tpeConv;
            }

            if (value is IBaboonAdtMemberMeta branch)
            {
                if (_convsWild.TryGetValue(branch.BaboonAdtType(), out var branchConv))
                {
                    return branchConv;
                }   
            }
            
            return new List<IConversion>();
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
            var tconv = (AbstractConversion<TFrom, TTo>) conv;
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
                _from = from;
                _convs = convs;
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
}
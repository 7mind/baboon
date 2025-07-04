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

        public static Dictionary<TKey, TElement> BbnToDictionary<TKey, TElement>(this IEnumerable<KeyValuePair<TKey, TElement>> source)
            where TKey : notnull
        {
            if (source is Dictionary<TKey, TElement> d)
            {
                return d;
            }

            return new Dictionary<TKey, TElement>(source);
        }
    }

    public interface IBaboonGenerated
    {
        public string BaboonDomainVersion();
        public string BaboonDomainIdentifier();
        public IReadOnlyList<string> BaboonSameInVersions();
        public string BaboonTypeIdentifier();
    }

    public interface IBaboonAdtMemberMeta
    {
        public string BaboonAdtTypeIdentifier();
        public Type BaboonAdtType();
    }

    public interface IBaboonMeta
    {
        public IReadOnlyList<string> SameInVersions(string typeIdString);
    }

    public interface IBaboonGeneratedLatest : IBaboonGenerated
    {
    }

    public abstract record Either<TLeft, TRight>
    {
        private Either()
        {
        }

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
        private Unit()
        {
        }

        public static readonly Unit Default = new Unit();

        public override string ToString()
        {
            return "()";
        }
    }

    public abstract class BaboonSingleton<T, IMPL> where IMPL : T, new()
    {
        protected static Lazy<T> LazyInstance = new Lazy<T>(() => new IMPL());

        public static T Instance
        {
            get => LazyInstance.Value;
            set { LazyInstance = new Lazy<T>(() => value); }
        }
    }
}
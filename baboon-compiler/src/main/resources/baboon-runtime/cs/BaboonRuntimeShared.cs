#nullable enable

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;

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
        public abstract bool IsLeft { get; }
        public abstract TLeft GetLeft();

        public abstract bool IsRight { get; }
        public abstract TRight GetRight();

        private Either()
        {
        }

        public sealed record Left(TLeft Value) : Either<TLeft, TRight>
        {
            public override bool IsLeft => true;
            public override TLeft GetLeft() => Value;

            public override bool IsRight => false;
            public override TRight GetRight() => throw new Exception($"Right either projection expected, but got left: {Value}");
        }

        public sealed record Right(TRight Value) : Either<TLeft, TRight>
        {
            public override bool IsLeft => false;
            public override TLeft GetLeft() => throw new Exception($"Left either projection expected, but got right: {Value}");

            public override bool IsRight => true;
            public override TRight GetRight() => Value;
        }

        public bool Contains(TRight other)
        {
            return IsRight && Equals(GetRight(), other);
        }

        public bool Exists(Func<TRight, bool> predicate)
        {
            return IsRight && predicate(GetRight());
        }

        public bool Forall(Func<TRight, bool> predicate)
        {
            return IsLeft || predicate(GetRight());
        }

        public Either<TLeft, TR1> Map<TR1>(Func<TRight, TR1> func)
        {
            return IsRight ? Either.Right<TLeft, TR1>(func(GetRight())) : Either.Left<TLeft, TR1>(GetLeft());
        }

        public Either<TLeft, TR1> FlatMap<TR1>(Func<TRight, Either<TLeft, TR1>> func)
        {
            return IsRight ? func(GetRight()) : Either.Left<TLeft, TR1>(GetLeft());
        }

        public Either<TL1, TRight> LeftMap<TL1>(Func<TLeft, TL1> func)
        {
            return IsLeft ? Either.Left<TL1, TRight>(func(GetLeft())) : Either.Right<TL1, TRight>(GetRight());
        }

        public Either<TL1, TRight> LeftFlatMap<TL1>(Func<TLeft, Either<TL1, TRight>> func)
        {
            return IsLeft ? func(GetLeft()) : Either.Right<TL1, TRight>(GetRight());
        }

        public TOut Fold<TOut>(Func<TLeft, TOut> onLeft, Func<TRight, TOut> onRight)
        {
            return IsRight ? onRight(GetRight()) : onLeft(GetLeft());
        }

        public void Match(Action<TLeft> onLeft, Action<TRight> onRight)
        {
            if (IsRight) onRight(GetRight());
            else onLeft(GetLeft());
        }

        public void Foreach(Action<TRight> action)
        {
            if (IsRight) action(GetRight());
        }

        public TRight GetOrElse(TRight defaultValue)
        {
            return IsRight ? GetRight() : defaultValue;
        }

        public TRight GetOrElse(Func<TRight> defaultValue)
        {
            return IsRight ? GetRight() : defaultValue();
        }

        public TRight GetOrElseRecover(Func<TLeft, TRight> recover)
        {
            return IsRight ? GetRight() : recover(GetLeft());
        }

        public Either<TLeft, TRight> OrElse(Either<TLeft, TRight> alternative)
        {
            return IsRight ? this : alternative;
        }

        public Either<TLeft, TRight> OrElse(Func<Either<TLeft, TRight>> alternative)
        {
            return IsRight ? this : alternative();
        }

        public bool TryGetRight([MaybeNullWhen(returnValue: false)] out TRight value)
        {
            if (IsRight)
            {
                value = GetRight();
                return true;
            }

            value = default!;
            return false;
        }

        public bool TryGetLeft([MaybeNullWhen(returnValue: false)] out TLeft value)
        {
            if (IsLeft)
            {
                value = GetLeft();
                return true;
            }

            value = default!;
            return false;
        }

        public Either<TRight, TLeft> Swap()
        {
            return IsRight ? Either.Left<TRight, TLeft>(GetRight()) : Either.Right<TRight, TLeft>(GetLeft());
        }
    }

    public static class Either
    {
        public static Either<TLeft, TRight> Left<TLeft, TRight>(TLeft value) => new Either<TLeft, TRight>.Left(value);
        public static Either<TLeft, TRight> Right<TLeft, TRight>(TRight value) => new Either<TLeft, TRight>.Right(value);

        public static Either<Exception, TRight> FromTry<TRight>(Func<TRight> func)
        {
            try { return Right<Exception, TRight>(func()); }
            catch (Exception ex) { return Left<Exception, TRight>(ex); }
        }

        public static Either<TLeft, TRight> When<TLeft, TRight>(bool predicate, Func<TRight> ifTrue, Func<TLeft> ifFalse)
        {
            return predicate ? Right<TLeft, TRight>(ifTrue()) : Left<TLeft, TRight>(ifFalse());
        }

        public static Either<TLeft, TRight> Flatten<TLeft, TRight>(this Either<TLeft, Either<TLeft, TRight>> either)
        {
            return either.FlatMap(inner => inner);
        }
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
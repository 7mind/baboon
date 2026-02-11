#nullable enable

using System;

namespace CustomContainers
{
    public abstract record Outcome<TSuccess>
    {
        private Outcome() {}

        public sealed record Success(TSuccess Value) : Outcome<TSuccess>;
        public sealed record Failure(object Error) : Outcome<TSuccess>;
    }

    // IBaboonServiceRt implementation for Outcome<$success> (single-param container)
    //
    // The generated interface uses renderContainer("L","R") = Outcome<R>, etc.
    // L/A/C type parameters are phantom (not present in the container type).
    // Errors are tracked as `object` inside Failure, with casts for LeftMap.
    //
    // Generated interface shape:
    //   Outcome<R> Pure<L, R>(R value)
    //   Outcome<R> Fail<L, R>(L error)
    //   Outcome<B> LeftMap<A, B, C>(Outcome<B> value, Func<A, C> f)
    //   Outcome<C> FlatMap<A, B, C>(Outcome<B> value, Func<B, Outcome<C>> f)
    public class OutcomeServiceRt : Testpkg.Pkg0.IBaboonServiceRt
    {
        public static readonly OutcomeServiceRt Instance = new OutcomeServiceRt();

        public Outcome<R> Pure<L, R>(R value)
        {
            return new Outcome<R>.Success(value);
        }

        public Outcome<R> Fail<L, R>(L error)
        {
            return new Outcome<R>.Failure(error!);
        }

        public Outcome<B> LeftMap<A, B, C>(Outcome<B> value, Func<A, C> f)
        {
            if (value is Outcome<B>.Failure failure)
            {
                var mapped = f((A)failure.Error);
                return new Outcome<B>.Failure(mapped!);
            }
            if (value is Outcome<B>.Success success)
            {
                return new Outcome<B>.Success(success.Value);
            }
            throw new InvalidOperationException("Unexpected Outcome variant");
        }

        public Outcome<C> FlatMap<A, B, C>(Outcome<B> value, Func<B, Outcome<C>> f)
        {
            if (value is Outcome<B>.Failure failure)
            {
                return new Outcome<C>.Failure(failure.Error);
            }
            if (value is Outcome<B>.Success success)
            {
                return f(success.Value);
            }
            throw new InvalidOperationException("Unexpected Outcome variant");
        }
    }
}

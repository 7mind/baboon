#nullable enable

using System;

namespace CustomContainers
{
    public abstract record Result<TSuccess, TError>
    {
        private Result() {}

        public sealed record Success(TSuccess Value) : Result<TSuccess, TError>;
        public sealed record Failure(TError Value) : Result<TSuccess, TError>;
    }

    // IBaboonServiceRt implementation for Result<$success, $error> (reversed param order)
    //
    // The generated interface uses renderContainer("L","R") = Result<R,L>, etc.
    // So the Rt methods have these signatures:
    //   Result<R, L> Pure<L, R>(R value)
    //   Result<R, L> Fail<L, R>(L error)
    //   Result<B, C> LeftMap<A, B, C>(Result<B, A> value, Func<A, C> f)
    //   Result<C, A> FlatMap<A, B, C>(Result<B, A> value, Func<B, Result<C, A>> f)
    public class ResultServiceRt : Testpkg.Pkg0.IBaboonServiceRt
    {
        public static readonly ResultServiceRt Instance = new ResultServiceRt();

        public Result<R, L> Pure<L, R>(R value)
        {
            return new Result<R, L>.Success(value);
        }

        public Result<R, L> Fail<L, R>(L error)
        {
            return new Result<R, L>.Failure(error);
        }

        public Result<B, C> LeftMap<A, B, C>(Result<B, A> value, Func<A, C> f)
        {
            if (value is Result<B, A>.Failure failure)
            {
                return new Result<B, C>.Failure(f(failure.Value));
            }
            if (value is Result<B, A>.Success success)
            {
                return new Result<B, C>.Success(success.Value);
            }
            throw new InvalidOperationException("Unexpected Result variant");
        }

        public Result<C, A> FlatMap<A, B, C>(Result<B, A> value, Func<B, Result<C, A>> f)
        {
            if (value is Result<B, A>.Failure failure)
            {
                return new Result<C, A>.Failure(failure.Value);
            }
            if (value is Result<B, A>.Success success)
            {
                return f(success.Value);
            }
            throw new InvalidOperationException("Unexpected Result variant");
        }
    }
}

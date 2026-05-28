#nullable enable

using System;
using System.Collections.Generic;

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
    public sealed record BaboonMethodId(string ServiceName, string MethodName);

    public abstract record BaboonWiringError
    {
        private BaboonWiringError()
        {
        }

        public sealed record NoMatchingMethod(BaboonMethodId Method) : BaboonWiringError;

        public sealed record NoMatchingService(BaboonMethodId Method) : BaboonWiringError;

        public sealed record DuplicateService(string ServiceName) : BaboonWiringError;

        public sealed record DecoderFailed(BaboonMethodId Method, Exception Exception) : BaboonWiringError;

        public sealed record EncoderFailed(BaboonMethodId Method, Exception Exception) : BaboonWiringError;

        public sealed record CallFailed(BaboonMethodId Method, object? DomainError) : BaboonWiringError;
    }

    public class BaboonWiringException : Exception
    {
        public BaboonWiringError Error { get; }

        public BaboonWiringException(BaboonWiringError error) : base(error.ToString())
        {
            Error = error;
        }
    }

    // --- Service muxers ---
    //
    // Cross-domain composable dispatch. A muxer holds a set of services from
    // any model(s) and routes an `(method, data, ctx)` call to the right one
    // by `method.ServiceName`. The R type parameter encodes the return shape
    // so the same class supports both sync (R = string / byte[]) and
    // hypothetical async (R = Task<string> / Task<byte[]>) generated
    // services. The C# wiring generator currently emits sync code only, so
    // the per-service wrapper classes pick R = string / byte[] (or the
    // service-result-container-wrapped variant in errors mode), but the
    // muxer itself stays parametric for forward-compat with potential
    // future Task-based emission.

    public interface IBaboonJsonService<out R>
    {
        string ServiceName { get; }
        R Invoke(BaboonMethodId method, string data, BaboonCodecContext ctx);
    }

    public interface IBaboonUebaService<out R>
    {
        string ServiceName { get; }
        R Invoke(BaboonMethodId method, byte[] data, BaboonCodecContext ctx);
    }

    public sealed class JsonMuxer<R>
    {
        private readonly Dictionary<string, IBaboonJsonService<R>> _table = new Dictionary<string, IBaboonJsonService<R>>();

        public JsonMuxer(params IBaboonJsonService<R>[] services)
        {
            foreach (var s in services)
            {
                Register(s);
            }
        }

        public void Register(IBaboonJsonService<R> service)
        {
            if (_table.ContainsKey(service.ServiceName))
            {
                throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.ServiceName));
            }
            _table[service.ServiceName] = service;
        }

        public R Invoke(BaboonMethodId method, string data, BaboonCodecContext ctx)
        {
            if (!_table.TryGetValue(method.ServiceName, out var service))
            {
                throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
            }
            return service.Invoke(method, data, ctx);
        }

        public IReadOnlyCollection<string> ServiceNames()
        {
            return _table.Keys;
        }
    }

    public sealed class UebaMuxer<R>
    {
        private readonly Dictionary<string, IBaboonUebaService<R>> _table = new Dictionary<string, IBaboonUebaService<R>>();

        public UebaMuxer(params IBaboonUebaService<R>[] services)
        {
            foreach (var s in services)
            {
                Register(s);
            }
        }

        public void Register(IBaboonUebaService<R> service)
        {
            if (_table.ContainsKey(service.ServiceName))
            {
                throw new BaboonWiringException(new BaboonWiringError.DuplicateService(service.ServiceName));
            }
            _table[service.ServiceName] = service;
        }

        public R Invoke(BaboonMethodId method, byte[] data, BaboonCodecContext ctx)
        {
            if (!_table.TryGetValue(method.ServiceName, out var service))
            {
                throw new BaboonWiringException(new BaboonWiringError.NoMatchingService(method));
            }
            return service.Invoke(method, data, ctx);
        }

        public IReadOnlyCollection<string> ServiceNames()
        {
            return _table.Keys;
        }
    }
}

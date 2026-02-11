#nullable enable

using System;

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
}

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
// ReSharper disable ArrangeNamespaceBody
// ReSharper disable UnusedType.Global
// ReSharper disable InconsistentNaming
// ReSharper disable ClassCanBeSealed.Global

namespace Baboon.Runtime.Shared
{
    public abstract class BaboonCodecException : Exception
    {
        protected BaboonCodecException(string message) : base(message)
        {
        }

        protected BaboonCodecException(string message, Exception? cause) : base(message, cause)
        {
        }

        public sealed class EncoderFailure : BaboonCodecException
        {
            public EncoderFailure(string message) : base(message)
            {
            }

            public EncoderFailure(string message, Exception? cause) : base(message, cause)
            {
            }
        }

        public sealed class DecoderFailure : BaboonCodecException
        {
            public DecoderFailure(string message) : base(message)
            {
            }

            public DecoderFailure(string message, Exception? cause) : base(message, cause)
            {
            }
        }

        public sealed class ConverterFailure : BaboonCodecException
        {
            public ConverterFailure(string message) : base(message)
            {
            }

            public ConverterFailure(string message, Exception? cause) : base(message, cause)
            {
            }
        }

        public sealed class CodecNotFound : BaboonCodecException
        {
            public CodecNotFound(string message) : base(message)
            {
            }
        }

        public sealed class ConversionNotFound : BaboonCodecException
        {
            public ConversionNotFound(string message) : base(message)
            {
            }
        }
    }
}

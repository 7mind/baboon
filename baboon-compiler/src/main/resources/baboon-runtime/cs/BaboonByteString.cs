#nullable enable

using System;
using System.IO;
using System.Linq;
using System.Text;

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
    public sealed class ByteString : IComparable<ByteString>, IEquatable<ByteString>
    {
        private readonly byte[] _bytes;

        // Properties
        public int Length => _bytes.Length;
        public byte this[int index] => _bytes[index];

        // Constructors
        public ByteString(byte[] bytes)
        {
            _bytes = bytes ?? throw new ArgumentNullException(nameof(bytes));
        }

        public ByteString(string text, Encoding? encoding = null)
        {
            encoding ??= Encoding.UTF8;
            _bytes = encoding.GetBytes(text);
        }

        public ByteString(int capacity)
        {
            _bytes = new byte[capacity];
        }

        // Static factory methods
        public static ByteString Empty => new ByteString(Array.Empty<byte>());

        public static ByteString FromHex(string hex)
        {
            return Parse(hex);
        }

        // Concatenation
        public ByteString Concat(ByteString other)
        {
            if (other is null)
            {
                throw new ArgumentNullException(nameof(other));
            }

            var result = new byte[_bytes.Length + other._bytes.Length];
            Array.Copy(_bytes, 0, result, 0, _bytes.Length);
            Array.Copy(other._bytes, 0, result, _bytes.Length, other._bytes.Length);
            return new ByteString(result);
        }

        public ByteString Concat(params ByteString[] others)
        {
            if (others == null)
            {
                throw new ArgumentNullException(nameof(others));
            }

            var totalLength = _bytes.Length + others.Sum(bs => bs?.Length ?? 0);
            var result = new byte[totalLength];

            var offset = 0;
            Array.Copy(_bytes, 0, result, offset, _bytes.Length);
            offset += _bytes.Length;

            foreach (var other in others)
            {
                if (other == null!) continue;
                Array.Copy(other._bytes, 0, result, offset, other._bytes.Length);
                offset += other._bytes.Length;
            }

            return new ByteString(result);
        }

        // Operator overloads for concatenation
        public static ByteString operator +(ByteString left, ByteString right)
        {
            if (left == null!) return right;
            if (right == null!) return left;
            return left.Concat(right);
        }

        // Comparison
        public int CompareTo(ByteString? other)
        {
            if (other is null) return 1;

            var minLength = Math.Min(_bytes.Length, other._bytes.Length);
            for (var i = 0; i < minLength; i++)
            {
                var comparison = _bytes[i].CompareTo(other._bytes[i]);
                if (comparison != 0) return comparison;
            }

            return _bytes.Length.CompareTo(other._bytes.Length);
        }

        // Equality
        public bool Equals(ByteString? other)
        {
            if (other is null) return false;
            if (_bytes.Length != other._bytes.Length) return false;

            for (var i = 0; i < _bytes.Length; i++)
            {
                if (_bytes[i] != other._bytes[i]) return false;
            }

            return true;
        }

        public override bool Equals(object? obj)
        {
            return obj is ByteString bs && Equals(bs);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hash = 17;
                foreach (var b in _bytes) hash = hash * 31 + b;
                return hash;
            }
        }

        // Comparison operators
        public static bool operator ==(ByteString? left, ByteString? right)
        {
            if (ReferenceEquals(left, right)) return true;
            if (left is null || right is null) return false;
            return left.Equals(right);
        }

        public static bool operator !=(ByteString left, ByteString right)
        {
            return !(left == right);
        }

        public static bool operator <(ByteString? left, ByteString? right)
        {
            if (left is null) return right is not null;
            return left.CompareTo(right) < 0;
        }

        public static bool operator >(ByteString? left, ByteString? right)
        {
            if (left is null) return right is not null;
            return left.CompareTo(right) > 0;
        }

        public static bool operator <=(ByteString? left, ByteString? right)
        {
            if (left is null) return true;
            return left.CompareTo(right) <= 0;
        }

        public static bool operator >=(ByteString? left, ByteString? right)
        {
            if (left is null) return right is null;
            return left.CompareTo(right) >= 0;
        }

        // Utility methods
        public byte[] ToArray()
        {
            var copy = new byte[_bytes.Length];
            Array.Copy(_bytes, copy, _bytes.Length);
            return copy;
        }

        // Direct access to underlying bytes - USE WITH CAUTION!
        // Modifying the returned array will break immutability
        public byte[] UnderlyingUnsafe()
        {
            return _bytes;
        }

        public string ToString(Encoding encoding)
        {
            return encoding.GetString(_bytes);
        }

        public override string ToString()
        {
            return ToString(Encoding.UTF8);
        }

        public string ToHexString()
        {
            return BitConverter.ToString(_bytes).Replace("-", "");
        }

        // Hex-encode the byte string content
        public string Encode()
        {
            if (_bytes.Length == 0) return string.Empty;
            var sb = new StringBuilder(_bytes.Length * 2);
            foreach (var b in _bytes) sb.Append(b.ToString("X2"));
            return sb.ToString();
        }

        // Static method to parse hex-encoded string into ByteString.
        // Separators (ASCII space, \t, \n, \v, \f, \r, ':' and '-') are ignored anywhere;
        // everything else must be an ASCII hex digit.
        public static ByteString Parse(string hexString)
        {
            if (hexString == null)
            {
                throw new ArgumentNullException(nameof(hexString));
            }

            var nibbles = new byte[hexString.Length];
            var count = 0;
            for (var i = 0; i < hexString.Length; i++)
            {
                var c = hexString[i];
                switch (c)
                {
                    case ' ':
                    case '\t':
                    case '\n':
                    case '\v':
                    case '\f':
                    case '\r':
                    case ':':
                    case '-':
                        continue;
                    case >= '0' and <= '9':
                        nibbles[count++] = (byte) (c - '0');
                        break;
                    case >= 'a' and <= 'f':
                        nibbles[count++] = (byte) (c - 'a' + 10);
                        break;
                    case >= 'A' and <= 'F':
                        nibbles[count++] = (byte) (c - 'A' + 10);
                        break;
                    default:
                        throw new FormatException($"Invalid hex character '{c}' (U+{(int) c:X4}) at position {i}");
                }
            }

            if (count % 2 != 0)
            {
                throw new FormatException($"Invalid hex string: odd number of hex digits ({count})");
            }

            var bytes = new byte[count / 2];
            for (var i = 0; i < bytes.Length; i++)
            {
                bytes[i] = (byte) ((nibbles[i * 2] << 4) | nibbles[i * 2 + 1]);
            }

            return new ByteString(bytes);
        }

        public static void WriteBytes(ByteString dt, BinaryWriter writer)
        {
            writer.Write((uint) dt._bytes.Length);
            writer.Write(dt._bytes);
        }

        public static ByteString ReadBytes(BinaryReader reader)
        {
            var length = reader.ReadUInt32();
            var bytes = reader.ReadBytes((int) length);
            return new ByteString(bytes);
        }

        public ByteString Substring(int startIndex, int length)
        {
            if (startIndex < 0 || startIndex >= _bytes.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(startIndex));
            }

            if (length < 0 || startIndex + length > _bytes.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(length));
            }

            var result = new byte[length];
            Array.Copy(_bytes, startIndex, result, 0, length);
            return new ByteString(result);
        }

        public bool StartsWith(ByteString? other)
        {
            if (other is null || other.Length > Length) return false;
            for (var i = 0; i < other.Length; i++)
            {
                if (_bytes[i] != other._bytes[i]) return false;
            }

            return true;
        }

        public bool EndsWith(ByteString? other)
        {
            if (other is null || other.Length > Length) return false;

            int offset = Length - other.Length;
            for (int i = 0; i < other.Length; i++)
            {
                if (_bytes[offset + i] != other._bytes[i]) return false;
            }

            return true;
        }
    }
}
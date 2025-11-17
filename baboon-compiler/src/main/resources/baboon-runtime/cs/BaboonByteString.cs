#nullable enable

using System.Collections.Generic;
using System.IO;
using System.Linq;
using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Text;
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
    public class ByteString : IComparable<ByteString>, IEquatable<ByteString>
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

        public ByteString(string text, Encoding encoding = null)
        {
            encoding = encoding ?? Encoding.UTF8;
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
            hex = hex.Replace(" ", "").Replace("-", "");
            if (hex.Length % 2 != 0)
                throw new ArgumentException("Hex string must have even length");

            byte[] bytes = new byte[hex.Length / 2];
            for (int i = 0; i < bytes.Length; i++)
            {
                bytes[i] = Convert.ToByte(hex.Substring(i * 2, 2), 16);
            }

            return new ByteString(bytes);
        }

        // Concatenation
        public ByteString Concat(ByteString other)
        {
            if (other == null) throw new ArgumentNullException(nameof(other));

            byte[] result = new byte[_bytes.Length + other._bytes.Length];
            Array.Copy(_bytes, 0, result, 0, _bytes.Length);
            Array.Copy(other._bytes, 0, result, _bytes.Length, other._bytes.Length);
            return new ByteString(result);
        }

        public ByteString Concat(params ByteString[] others)
        {
            if (others == null) throw new ArgumentNullException(nameof(others));

            int totalLength = _bytes.Length + others.Sum(bs => bs?.Length ?? 0);
            byte[] result = new byte[totalLength];

            int offset = 0;
            Array.Copy(_bytes, 0, result, offset, _bytes.Length);
            offset += _bytes.Length;

            foreach (var other in others)
            {
                if (other != null)
                {
                    Array.Copy(other._bytes, 0, result, offset, other._bytes.Length);
                    offset += other._bytes.Length;
                }
            }

            return new ByteString(result);
        }

        // Operator overloads for concatenation
        public static ByteString operator +(ByteString left, ByteString right)
        {
            if (left == null) return right;
            if (right == null) return left;
            return left.Concat(right);
        }

        // Comparison
        public int CompareTo(ByteString other)
        {
            if (other == null) return 1;

            int minLength = Math.Min(_bytes.Length, other._bytes.Length);
            for (int i = 0; i < minLength; i++)
            {
                int comparison = _bytes[i].CompareTo(other._bytes[i]);
                if (comparison != 0) return comparison;
            }

            return _bytes.Length.CompareTo(other._bytes.Length);
        }

        // Equality
        public bool Equals(ByteString other)
        {
            if (other == null) return false;
            if (_bytes.Length != other._bytes.Length) return false;

            for (int i = 0; i < _bytes.Length; i++)
            {
                if (_bytes[i] != other._bytes[i]) return false;
            }

            return true;
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as ByteString);
        }

        public override int GetHashCode()
        {
            unchecked
            {
                int hash = 17;
                foreach (byte b in _bytes)
                {
                    hash = hash * 31 + b;
                }

                return hash;
            }
        }

        // Comparison operators
        public static bool operator ==(ByteString left, ByteString right)
        {
            if (ReferenceEquals(left, right)) return true;
            if (left is null || right is null) return false;
            return left.Equals(right);
        }

        public static bool operator !=(ByteString left, ByteString right)
        {
            return !(left == right);
        }

        public static bool operator <(ByteString left, ByteString right)
        {
            if (left is null) return right is not null;
            return left.CompareTo(right) < 0;
        }

        public static bool operator >(ByteString left, ByteString right)
        {
            if (left is null) return false;
            return left.CompareTo(right) > 0;
        }

        public static bool operator <=(ByteString left, ByteString right)
        {
            if (left is null) return true;
            return left.CompareTo(right) <= 0;
        }

        public static bool operator >=(ByteString left, ByteString right)
        {
            if (left is null) return right is null;
            return left.CompareTo(right) >= 0;
        }

        // Utility methods
        public byte[] ToArray()
        {
            byte[] copy = new byte[_bytes.Length];
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

            StringBuilder sb = new StringBuilder(_bytes.Length * 2);
            foreach (byte b in _bytes)
            {
                sb.Append(b.ToString("X2"));
            }

            return sb.ToString();
        }

        // Static method to parse hex-encoded string into ByteString
        public static ByteString Parse(string hexString)
        {
            if (hexString == null)
                throw new ArgumentNullException(nameof(hexString));

            // Remove common separators and whitespace
            hexString = hexString.Replace(" ", "")
                .Replace("-", "")
                .Replace(":", "")
                .Trim();

            if (hexString.Length == 0)
                return ByteString.Empty;

            if (hexString.Length % 2 != 0)
                throw new FormatException(
                    $"Invalid hex string length: {hexString.Length}. Hex string must have even length.");

            byte[] bytes = new byte[hexString.Length / 2];
            for (int i = 0; i < bytes.Length; i++)
            {
                string byteString = hexString.Substring(i * 2, 2);
                if (!byte.TryParse(byteString, System.Globalization.NumberStyles.HexNumber, null, out bytes[i]))
                {
                    throw new FormatException($"Invalid hex characters at position {i * 2}: '{byteString}'");
                }
            }

            return new ByteString(bytes);
        }

        public static void WriteBytes(ByteString dt, BinaryWriter writer)
        {
            writer.Write((uint)dt._bytes.Length);
            writer.Write(dt._bytes);
        }

        public static ByteString ReadBytes(BinaryReader reader)
        {
            uint length = reader.ReadUInt32();
            byte[] bytes = reader.ReadBytes((int)length);
            return new ByteString(bytes);
        }

        public ByteString Substring(int startIndex, int length)
        {
            if (startIndex < 0 || startIndex >= _bytes.Length)
                throw new ArgumentOutOfRangeException(nameof(startIndex));
            if (length < 0 || startIndex + length > _bytes.Length)
                throw new ArgumentOutOfRangeException(nameof(length));

            byte[] result = new byte[length];
            Array.Copy(_bytes, startIndex, result, 0, length);
            return new ByteString(result);
        }

        public bool StartsWith(ByteString other)
        {
            if (other == null || other.Length > Length) return false;

            for (int i = 0; i < other.Length; i++)
            {
                if (_bytes[i] != other._bytes[i]) return false;
            }

            return true;
        }

        public bool EndsWith(ByteString other)
        {
            if (other == null || other.Length > Length) return false;

            int offset = Length - other.Length;
            for (int i = 0; i < other.Length; i++)
            {
                if (_bytes[offset + i] != other._bytes[i]) return false;
            }

            return true;
        }
    }
}
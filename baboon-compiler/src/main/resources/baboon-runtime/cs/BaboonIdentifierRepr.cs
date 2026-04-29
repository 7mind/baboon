#nullable enable

using System;
using System.Globalization;
using System.Text;
using System.Text.RegularExpressions;

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
    /// <summary>
    /// Runtime helpers for the `id` toString / parseRepr machinery defined in
    /// docs/spec/identifier-repr.md. The C# backend (PR-57a) uses these helpers
    /// from emitted code; conformance to the spec is the contract.
    /// </summary>
    public static class BaboonIdentifierRepr
    {
        // Defensive: numeric Char constant rather than a quoted-character literal.
        // Mirrors Scala helper.
        private const char BS  = (char)92; // ASCII backslash
        private const char HSH = '#';
        private const char COL = ':';
        private const char OBR = '{';
        private const char CBR = '}';

        // Identifier repr mandates 24-char tsu (always UTC `Z`) and 29-char tso
        // (always `±HH:MM` offset, never `Z` shorthand). These dedicated formats
        // enforce the widths the schema-directed parser depends on (spec §3 / §5.4).
        private const string TsuPattern = "yyyy-MM-ddTHH:mm:ss.fffZ";
        private const string TsoPattern = "yyyy-MM-ddTHH:mm:ss.fffzzz";

        private static readonly Regex LowerHexEven = new Regex("^[0-9a-f]*$", RegexOptions.Compiled);

        /// <summary>Uid lowercase canonical-form regex per spec §5.4. Used by emitted
        /// parseRepr decoders to reject mixed-case forms before delegating to Guid.Parse.</summary>
        public static readonly Regex UidLowerRegex =
            new Regex("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$", RegexOptions.Compiled);

        /// <summary>Backslash-escape the 5 metacharacters per spec §4.2.</summary>
        public static string EscapeStr(string s)
        {
            var sb = new StringBuilder(s.Length);
            for (var i = 0; i < s.Length; i++)
            {
                var c = s[i];
                if (c == BS || c == HSH || c == COL || c == OBR || c == CBR)
                {
                    sb.Append(BS);
                    sb.Append(c);
                }
                else
                {
                    sb.Append(c);
                }
            }
            return sb.ToString();
        }

        /// <summary>Lowercase hex, no separators, per spec §3 / §4.4.</summary>
        public static string BytesToHex(ByteString bs)
        {
            var arr = bs.UnderlyingUnsafe();
            var sb  = new StringBuilder(arr.Length * 2);
            for (var i = 0; i < arr.Length; i++)
            {
                var b = arr[i] & 0xFF;
                sb.Append(ToLowerHexDigit((b >> 4) & 0xF));
                sb.Append(ToLowerHexDigit(b & 0xF));
            }
            return sb.ToString();
        }

        private static char ToLowerHexDigit(int v)
        {
            return (char)(v < 10 ? ('0' + v) : ('a' + (v - 10)));
        }

        /// <summary>Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
        /// exactly 24 characters. Normalises the input to UTC.</summary>
        public static string TsuToString(DateTime dt)
        {
            // ToUniversalTime is a no-op on Kind=Utc; for Local/Unspecified we coerce to UTC.
            var utc = dt.Kind == DateTimeKind.Utc ? dt : dt.ToUniversalTime();
            return utc.ToString(TsuPattern, CultureInfo.InvariantCulture);
        }

        /// <summary>Render a `tsu` from a DateTimeOffset by extracting the UTC instant.</summary>
        public static string TsuToString(DateTimeOffset dto)
        {
            return dto.UtcDateTime.ToString(TsuPattern, CultureInfo.InvariantCulture);
        }

        /// <summary>Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
        /// milliseconds, exactly 29 characters. Never emits `Z` shorthand.</summary>
        public static string TsoToString(DateTimeOffset dto)
        {
            return dto.ToString(TsoPattern, CultureInfo.InvariantCulture);
        }

        /// <summary>Render an unsigned i64 as decimal.</summary>
        public static string U64ToString(ulong v)
        {
            return v.ToString(CultureInfo.InvariantCulture);
        }

        /// <summary>Render a `bit` per spec §3 — exact lowercase ASCII.</summary>
        public static string BitToString(bool b)
        {
            return b ? "true" : "false";
        }

        public static Either<string, DateTime> ParseTsuRepr(string s)
        {
            if (s.Length != 24)
            {
                return Either.Left<string, DateTime>("tsu repr must be 24 chars, got " + s.Length);
            }
            if (s[23] != 'Z')
            {
                return Either.Left<string, DateTime>("tsu repr must end with 'Z', got: " + s);
            }
            try
            {
                var parsed = DateTime.ParseExact(
                    s,
                    TsuPattern,
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.AssumeUniversal | DateTimeStyles.AdjustToUniversal
                );
                return Either.Right<string, DateTime>(parsed);
            }
            catch (FormatException)
            {
                return Either.Left<string, DateTime>("could not parse tsu: " + s);
            }
        }

        public static Either<string, DateTimeOffset> ParseTsoRepr(string s)
        {
            if (s.Length != 29)
            {
                return Either.Left<string, DateTimeOffset>("tso repr must be 29 chars, got " + s.Length);
            }
            try
            {
                var parsed = DateTimeOffset.ParseExact(
                    s,
                    TsoPattern,
                    CultureInfo.InvariantCulture,
                    DateTimeStyles.None
                );
                return Either.Right<string, DateTimeOffset>(parsed);
            }
            catch (FormatException)
            {
                return Either.Left<string, DateTimeOffset>("could not parse tso: " + s);
            }
        }

        /// <summary>Decode `bytes` from lowercase hex. Empty string is legal (empty bytes).</summary>
        public static Either<string, ByteString> ParseBytesHex(string s)
        {
            if (s.Length == 0)
            {
                return Either.Right<string, ByteString>(new ByteString(Array.Empty<byte>()));
            }
            if ((s.Length & 1) != 0)
            {
                return Either.Left<string, ByteString>("odd-length hex sequence: " + s);
            }
            // Spec mandates lowercase; reject uppercase to keep exactly one canonical form.
            if (!LowerHexEven.IsMatch(s))
            {
                return Either.Left<string, ByteString>("non-lowercase or non-hex character in: " + s);
            }
            var outArr = new byte[s.Length / 2];
            for (var i = 0; i < s.Length; i += 2)
            {
                var hi = HexDigit(s[i]);
                var lo = HexDigit(s[i + 1]);
                outArr[i / 2] = (byte)((hi << 4) | lo);
            }
            return Either.Right<string, ByteString>(new ByteString(outArr));
        }

        private static int HexDigit(char c)
        {
            if (c >= '0' && c <= '9') return c - '0';
            if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
            return -1;
        }

        public static Either<string, bool> ParseBit(string s)
        {
            if (s == "true")  return Either.Right<string, bool>(true);
            if (s == "false") return Either.Right<string, bool>(false);
            return Either.Left<string, bool>("expected 'true' or 'false' but found '" + s + "'");
        }

        /// <summary>Cursor-based parser for parseRepr decoders. Schema-directed; the
        /// caller (the emitted &lt;TypeName&gt;Codec.ParseRepr) drives the field
        /// sequence per declared type and order.</summary>
        public sealed class Cursor
        {
            private readonly string _source;
            private int _pos;

            public Cursor(string source)
            {
                _source = source;
                _pos    = 0;
            }

            public int Position => _pos;
            public bool AtEnd   => _pos >= _source.Length;
            public char Peek    => _source[_pos];

            public Either<string, Unit> Expect(char c)
            {
                if (AtEnd) return Either.Left<string, Unit>("expected '" + c + "' at " + _pos + " but reached end of input");
                if (Peek != c) return Either.Left<string, Unit>("expected '" + c + "' at " + _pos + " but found '" + Peek + "'");
                _pos += 1;
                return Either.Right<string, Unit>(Unit.Default);
            }

            public Either<string, Unit> ExpectLiteral(string lit)
            {
                if (_pos + lit.Length > _source.Length)
                {
                    return Either.Left<string, Unit>("expected literal '" + lit + "' at " + _pos + " but reached end of input");
                }
                for (var i = 0; i < lit.Length; i++)
                {
                    if (_source[_pos + i] != lit[i])
                    {
                        return Either.Left<string, Unit>("expected literal '" + lit + "' at " + _pos);
                    }
                }
                _pos += lit.Length;
                return Either.Right<string, Unit>(Unit.Default);
            }

            /// <summary>Read until the next bare metachar in `:#{}`. Backslash escapes are
            /// NOT processed here — see ReadStrField for that. Used for primitive
            /// consumption (numbers, uuids, hex bytes).</summary>
            public string ReadUntilStructural()
            {
                var start = _pos;
                while (_pos < _source.Length)
                {
                    var c = _source[_pos];
                    if (c == COL || c == HSH || c == OBR || c == CBR)
                    {
                        return _source.Substring(start, _pos - start);
                    }
                    _pos += 1;
                }
                return _source.Substring(start, _pos - start);
            }

            /// <summary>Consume exactly n characters as a fixed-width lexeme. Used for
            /// tsu/tso (which contain `:` characters inside the lexeme).</summary>
            public Either<string, string> ReadFixed(int n)
            {
                if (_pos + n > _source.Length)
                {
                    return Either.Left<string, string>("expected " + n + " chars at " + _pos + " but only " + (_source.Length - _pos) + " remain");
                }
                var s = _source.Substring(_pos, n);
                _pos += n;
                return Either.Right<string, string>(s);
            }

            /// <summary>Read a `str` field value with backslash-unescaping per spec §5.5.</summary>
            public Either<string, string> ReadStrField()
            {
                var sb = new StringBuilder();
                while (_pos < _source.Length)
                {
                    var c = _source[_pos];
                    if (c == COL || c == HSH || c == OBR || c == CBR)
                    {
                        return Either.Right<string, string>(sb.ToString());
                    }
                    if (c == BS)
                    {
                        if (_pos + 1 >= _source.Length)
                        {
                            return Either.Left<string, string>("trailing backslash at " + _pos);
                        }
                        var nxt = _source[_pos + 1];
                        if (nxt == BS || nxt == HSH || nxt == COL || nxt == OBR || nxt == CBR)
                        {
                            sb.Append(nxt);
                            _pos += 2;
                        }
                        else
                        {
                            return Either.Left<string, string>("invalid escape at " + _pos);
                        }
                    }
                    else
                    {
                        sb.Append(c);
                        _pos += 1;
                    }
                }
                return Either.Right<string, string>(sb.ToString());
            }
        }

        /// <summary>Validate header of an identifier-repr: &lt;simpleName&gt;:&lt;version&gt;#.</summary>
        public static Either<string, Unit> ParseHeader(Cursor cursor, string expectedSimpleName, string expectedVersion)
        {
            var nameLit = cursor.ReadUntilStructural();
            if (nameLit != expectedSimpleName)
            {
                return Either.Left<string, Unit>("expected name '" + expectedSimpleName + "' but found '" + nameLit + "'");
            }
            var afterName = cursor.Expect(':');
            if (afterName is Either<string, Unit>.Left ln) return Either.Left<string, Unit>(ln.Value);
            var verLit = cursor.ReadUntilStructural();
            if (verLit != expectedVersion)
            {
                return Either.Left<string, Unit>("expected version '" + expectedVersion + "' but found '" + verLit + "'");
            }
            return cursor.Expect('#');
        }

        /// <summary>Validate field-name segment: &lt;expectedFieldName&gt;:.</summary>
        public static Either<string, Unit> ParseFieldName(Cursor cursor, string expectedFieldName)
        {
            var name = cursor.ReadUntilStructural();
            if (name != expectedFieldName)
            {
                return Either.Left<string, Unit>("expected field name '" + expectedFieldName + "' but found '" + name + "'");
            }
            return cursor.Expect(':');
        }
    }
}

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


    public static class BaboonTools
    {
        public static decimal ReadDecimalLenient(JToken token)
        {
            if (token.Type == JTokenType.String)
            {
                return decimal.Parse(token.Value<string>()!, System.Globalization.CultureInfo.InvariantCulture);
            }
            return token.Value<decimal>();
        }

        public static T? ReadNullableValueType<T>(bool ifNot, Func<T> thenReturn) where T : struct
        {
            if (ifNot) return null;
            return thenReturn();
        }

        public static T? ReadNullableValueType<T>(JToken? token, Func<JToken, T> readValue) where T : struct
        {
            if (token == null || token.Type == JTokenType.Null) return null;
            return readValue(token);
        }

        public static T? ReadNullableReferentialType<T>(JToken? token, Func<JToken, T> readValue) where T : class
        {
            if (token == null || token.Type == JTokenType.Null) return null;
            return readValue(token);
        }

        public static int OptionHashcode<T>(T? value, Func<T, int> mk)
        {
            return value == null ? 0 : mk(value);
        }

        public static int SeqHashcode<T>(IEnumerable<T> value, Func<T, int> mk)
        {
            return value.Aggregate(0x1EAFDEAD, (current, item) => current ^ mk(item));
        }

        public static int SetHashcode<T>(ISet<T> value, Func<T, int> mk)
        {
            return value.Select(item => mk(item)).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, item) => current ^ item);
        }

        public static int MapHashcode<TKey, TValue>(IEnumerable<KeyValuePair<TKey, TValue>> value, Func<TKey, int> mkk, Func<TValue, int> mkv)
        {
            return (value.Select(item0 => HashCode.Combine(mkk(item0.Key), mkv(item0.Value))).OrderBy(c => c).Aggregate(0x1EAFDEAD, (current, item0) => current ^ item0));
        }

        public static bool MapEquals<TKey, TValue>(IReadOnlyDictionary<TKey, TValue> left, IReadOnlyDictionary<TKey, TValue> right, Func<TValue, TValue, bool> cmp)
        {
            return (left.Count == right.Count &&
                    left.Keys.All(right.ContainsKey) &&
                    left.Keys.All(key => cmp(left[key], right[key])));
        }

        public static bool MapEquals<TKey, TValue>(IDictionary<TKey, TValue> left, IDictionary<TKey, TValue> right, Func<TValue, TValue, bool> cmp)
        {
            return (left.Count == right.Count &&
                    left.Keys.All(right.ContainsKey) &&
                    left.Keys.All(key => cmp(left[key], right[key])));
        }

        public static bool OptionEquals<T>(T? left, T? right, Func<T, T, bool> cmp)
        {
            return Equals(left, right) || (left != null && right != null && cmp(left, right));
        }

        public static bool SeqEquals<T>(IList<T> left, IList<T> right, Func<T, T, bool> cmp)
        {
            return left.SequenceEqual(right) || (left.Count == right.Count && (left.Zip(right, (r, l) => (r, l)).All(t => cmp(t.Item1, t.Item2))));
        }

        public static bool SeqEquals<T>(IReadOnlyList<T> left, IReadOnlyList<T> right, Func<T, T, bool> cmp)
        {
            return left.SequenceEqual(right) || (left.Count == right.Count && (left.Zip(right, (r, l) => (r, l)).All(t => cmp(t.Item1, t.Item2))));
        }

        public static Dictionary<TKey, TValue> ReadDict<TKey, TValue>(BinaryReader wire, Func<BinaryReader, TKey> kd, Func<BinaryReader, TValue> vd) where TKey : notnull
        {
            return Enumerable.Range(0, wire.ReadInt32())
                .Select(_ => new KeyValuePair<TKey, TValue>(kd(wire), vd(wire)))
                .BbnToDictionary();
        }

        public static List<T> ReadList<T>(BinaryReader wire, Func<BinaryReader, T> d)
        {
            return Enumerable.Range(0, wire.ReadInt32())
                .Select(_ => d(wire))
                .BbnToList();
        }

        public static ImmutableHashSet<T> ReadSet<T>(BinaryReader wire, Func<BinaryReader, T> d)
        {
            return Enumerable.Range(0, wire.ReadInt32())
                .Select(_ => d(wire))
                .ToImmutableHashSet();
        }

        public static List<T> ReadJsonList<T>(JToken? token, Func<JToken, T> d)
        {
            return token!.Value<JArray>()!.Select(e => d(e)).BbnToList();
        }

        public static ImmutableHashSet<T> ReadJsonSet<T>(JToken? token, Func<JToken, T> d)
        {
            return token!.Value<JArray>()!.Select(e => d(e)).ToImmutableHashSet();
        }

        public static Dictionary<TKey, TValue> ReadJsonDict<TKey, TValue>(JToken? token, Func<string, TKey> dk, Func<JToken, TValue> dv) where TKey : notnull
        {
            return token!.Value<JObject>()!.Properties().Select(kv =>
                    new KeyValuePair<TKey, TValue>(dk(kv.Name), dv(kv.Value)))
                .BbnToDictionary();
        }

        public static JToken WriteOptionVal<T>(T? value, Func<T, JToken> e) where T : struct
        {
            return !value.HasValue ? JValue.CreateNull() : e(value.Value);
        }

        public static JToken WriteOptionRef<T>(T? value, Func<T, JToken> e)
        {
            return value == null ? JValue.CreateNull() : e(value);
        }

        public static JToken WriteSeq<T>(IEnumerable<T> value, Func<T, JToken> enc)
        {
            return new JArray(value.Select(e => enc(e)));
        }

        public static JToken WriteMap<TKey, TValue>(IEnumerable<KeyValuePair<TKey, TValue>> value, Func<KeyValuePair<TKey, TValue>, JProperty> enc)
        {
            // Sort by typed key's ToString form before emit. Matches the determinism
            // contract Scala's ScJsonCodecGenerator now provides (sortBy(_._1.toString)).
            // BAB-C04: insulates against user-supplied collections (HashMap, etc.) whose
            // iteration order depends on hash codes / runtime state.
            return new JObject(value.OrderBy(e => e.Key?.ToString() ?? string.Empty, StringComparer.Ordinal).Select(enc));
        }
    }
    
    public static class BaboonBinTools
    {
        // Wire-compatible with Scala's BaboonBinTools.writeString (ULEB128-prefixed UTF-8).
        // .NET's BinaryWriter.Write(string) uses a 7-bit-encoded length prefix (= ULEB128) followed
        // by UTF-8 bytes. BinaryReader.ReadString() is symmetric. Provided here for explicit
        // call sites that want to emphasise the wire-format guarantee, and for parity with
        // Scala/Kotlin sibling runtimes.
        public static void WriteString(BinaryWriter writer, string s)
        {
            writer.Write(s);
        }

        public static string ReadString(BinaryReader reader)
        {
            return reader.ReadString();
        }
    }

    public static class BaboonTestTools
    {
        public static void WriteBinaryFile(string filePath, byte[] data)
        {
            var directoryPath = Path.GetDirectoryName(filePath);
            if (!string.IsNullOrEmpty(directoryPath) && !Directory.Exists(directoryPath))
            {
                Directory.CreateDirectory(directoryPath);
            }

            File.WriteAllBytes(filePath, data);
        }
    }    
}
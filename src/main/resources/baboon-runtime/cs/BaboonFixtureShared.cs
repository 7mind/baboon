using System.Text;
using System.Collections.Generic;
using System.Collections.Immutable;
using System;
using System.Linq;

using Baboon.Time;
// ReSharper disable CheckNamespace
// ReSharper disable ArrangeNamespaceBody

namespace Baboon.Fixture {
    // RandomValuesGenerator
    public static class BaboonFixture
    {
        private static readonly Random Rnd = new();
        private const string Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

        public static bool NextBoolean()
        {
            return Rnd.Next(0, 2) == 1;
        }

        public static sbyte NextSByte()
        {
            return (sbyte)Rnd.Next(sbyte.MinValue, sbyte.MaxValue);
        }

        public static short NextInt16()
        {
            return (short)Rnd.Next(short.MinValue, short.MaxValue);
        }

        public static int NextInt32()
        {
            return Rnd.Next(int.MinValue, int.MaxValue);
        }

        public static int NextInt32(int max)
        {
            return Rnd.Next(0, max);
        }

        public static long NextInt64()
        {
            return Rnd.NextInt64(long.MinValue, long.MaxValue);
        }

        public static byte NextByte()
        {
            return (byte)Rnd.Next(0, byte.MaxValue);
        }

        public static ushort NextUInt16()
        {
            return (ushort)Rnd.Next(0, ushort.MaxValue);
        }

        public static uint NextUInt32()
        {
            return (uint)Rnd.Next(0, int.MaxValue);
        }

        public static ulong NextUInt64()
        {
            return (ulong)Rnd.Next(0, int.MaxValue);
        }

        public static float NextSingle()
        {
            return Rnd.NextSingle();
        }

        public static double NextDouble()
        {
            return Rnd.NextDouble();
        }

        public static decimal NextDecimal()
        {
            return (decimal)Rnd.NextDouble();
        }

        public static string NextString()
        {
            var length = Rnd.Next(0, 21);

            var stringBuilder = new StringBuilder(length);

            for (var i = 0; i < length; i++)
            {
                var randomChar = Chars[Rnd.Next(Chars.Length)];
                stringBuilder.Append(randomChar);
            }

            return stringBuilder.ToString();
        }

        public static RpDateTime NextRpDateTime()
        {
            var minTicks = DateTime.MinValue.Ticks;
            var maxTicks = DateTime.MaxValue.Ticks;

            var randomTicks = (long)(Rnd.NextDouble() * (maxTicks - minTicks)) + minTicks;

            return new RpDateTime(new DateTime(randomTicks));
        }

        public static Guid NextGuid()
        {
            return Guid.NewGuid();
        }

        public static T NextRandomEnum<T>() where T : Enum
        {
            var values = Enum.GetValues(typeof(T));
            return (T)values.GetValue(Rnd.Next(values.Length))!;
        }

        public static ImmutableList<T> FillList<T>(int count, Func<T> action)
        {
            return Enumerable.Range(0, count).Select(_ => action.Invoke()).ToImmutableList();
        }

        public static ImmutableHashSet<T> FillSet<T>(int count, Func<T> action)
        {
            return Enumerable.Range(0, count).Select(_ => action.Invoke()).ToImmutableHashSet();
        }

        public static ImmutableDictionary<TK, TV> FillDict<TK, TV>(int count, Func<KeyValuePair<TK, TV>> action) where TK: notnull
        {
            var entries= Enumerable.Range(0, count).Select(_ => action.Invoke()).ToList();

            var map = new Dictionary<TK, TV>(entries.Count);
            entries.ForEach(pair => map.TryAdd(pair.Key, pair.Value));

            return map.ToImmutableDictionary();
        }
    }
}
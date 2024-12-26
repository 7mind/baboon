using System.Text;
using Baboon.Time;
using System.Collections.Generic;
using System.Collections.Immutable;
using System;

namespace Baboon.Fixture {
    // RandomValuesGenerator
    public static class BaboonFixture
    {
        private static readonly Random Rnd = new Random();
        private const String Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    
        public static Boolean NextBoolean()
        {
            return Rnd.Next(0, 2) == 1;
        }
    
        public static sbyte NextSByte()
        {
            return (sbyte)Rnd.Next(sbyte.MinValue, sbyte.MaxValue);
        }
    
        public static Int16 NextInt16()
        {
            return (Int16)Rnd.Next(Int16.MinValue, Int16.MaxValue);
        }
    
        public static Int32 NextInt32()
        {
            return Rnd.Next(Int32.MinValue, Int32.MaxValue);
        }

        public static Int32 NextInt32(int max)
        {
            return Rnd.Next(0, max);
        }

        public static Int64 NextInt64()
        {
            return Rnd.NextInt64(Int64.MinValue, Int64.MaxValue);
        }
    
        public static byte NextByte()
        {
            return (byte)Rnd.Next(0, byte.MaxValue);
        }
    
        public static UInt16 NextUInt16()
        {
            return (UInt16)Rnd.Next(0, UInt16.MaxValue);
        }
    
        public static UInt32 NextUInt32()
        {
            return (UInt32)Rnd.Next(0, Int32.MaxValue);
        }
    
        public static UInt64 NextUInt64()
        {
            return (UInt64)Rnd.Next(0, Int32.MaxValue);
        }
    
        public static Single NextSingle()
        {
            return Rnd.NextSingle();
        }
    
        public static Double NextDouble()
        {
            return Rnd.NextDouble();
        }
    
        public static Decimal NextDecimal()
        {
            return (Decimal)Rnd.NextDouble();
        }
    
        public static String NextString()
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
    
            var randomTicks = (Int64)(Rnd.NextDouble() * (maxTicks - minTicks)) + minTicks;
    
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
            return Enumerable.Range(0, count).Select(i => action.Invoke()).ToImmutableList();
        }

        public static ImmutableHashSet<T> FillSet<T>(int count, Func<T> action)
        {
            return Enumerable.Range(0, count).Select(i => action.Invoke()).ToImmutableHashSet();
        }

        public static ImmutableDictionary<K, V> FillDict<K, V>(int count, Func<KeyValuePair<K, V>> action)
        {
            var entries= Enumerable.Range(0, count).Select(i => action.Invoke()).ToList();

            var map = new Dictionary<K, V>(entries.Count);
            entries.ForEach(pair => map.TryAdd(pair.Key, pair.Value));

            return map.ToImmutableDictionary();
        }
    }
}
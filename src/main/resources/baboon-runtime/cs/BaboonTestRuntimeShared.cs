#nullable enable

#pragma warning disable 612,618

using System.Text;
using Baboon.Time;
using System.Collections.Generic;
using System.Linq;
using System.Collections.Immutable;
using System;

namespace Baboon.Test.Runtime.Shared {
    // RandomValuesGenerator
    public static class RVG
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
    
        public static ImmutableDictionary<TK, TV> CreateImmutableDictionary<TK, TV>(List<KeyValuePair<TK, TV>> values) where TK : notnull
        {
            var map = new Dictionary<TK, TV>(values.Count);
            values.ForEach(pair => map.TryAdd(pair.Key, pair.Value));
    
            return map.ToImmutableDictionary();
        }
    }
}
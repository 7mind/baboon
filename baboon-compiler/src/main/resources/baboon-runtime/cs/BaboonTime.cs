using System;
using System.Globalization;
using System.IO;
using Newtonsoft.Json;

// ReSharper disable UnusedMember.Global
// ReSharper disable UseCollectionExpression
// ReSharper disable ConvertIfStatementToSwitchStatement
// ReSharper disable RedundantExplicitArrayCreation
// ReSharper disable CheckNamespace
// ReSharper disable ArrangeNamespaceBody
// ReSharper disable MemberCanBePrivate.Global

namespace Baboon.Time
{
    /** Reduced to milliseconds precision DateTime */
    [JsonConverter(typeof(JsonConverter))]
    public readonly struct RpDateTime : IComparable, IComparable<RpDateTime>, IEquatable<RpDateTime>
    {
        public readonly DateTimeOffset DateTimeOffset;

        public readonly DateTimeKind Kind;

        public DateTime DateTime => Kind switch
        {
            DateTimeKind.Utc => DateTimeOffset.UtcDateTime,
            DateTimeKind.Local => DateTimeOffset.LocalDateTime,
            DateTimeKind.Unspecified => DateTimeOffset.DateTime,
            _ => throw new ArgumentOutOfRangeException()
        };

        public long Ticks => DateTimeOffset.Ticks;

        public RpDateTime(DateTimeOffset dateTimeOffset)
        {
            DateTimeOffset = dateTimeOffset.TruncateToMillis();
            Kind = DetectKind(dateTimeOffset);
        }

        public RpDateTime(DateTimeOffset dateTimeOffset, DateTimeKind kind)
        {
            DateTimeOffset = dateTimeOffset.TruncateToMillis();
            Kind = kind;
        }

        public RpDateTime(int year, int month, int day)
        {
            DateTimeOffset = new DateTime(year, month, day).ToDateTimeOffset();
            Kind = DateTimeKind.Local;
        }

        public RpDateTime(int year, int month, int day, int hour, int minute, int second)
        {
            DateTimeOffset = new DateTime(year, month, day, hour, minute, second).ToDateTimeOffset();
            Kind = DateTimeKind.Local;
        }

        public RpDateTime(int year, int month, int day, int hour, int minute, int second, int millisecond, DateTimeKind kind)
        {
            DateTimeOffset = new DateTime(year, month, day, hour, minute, second, millisecond, kind).ToDateTimeOffset();
            Kind = kind;
        }

        public RpDateTime(long ticks, TimeSpan offset, DateTimeKind kind)
        {
            DateTimeOffset = new DateTimeOffset(AdjustTicksOverflow(BaboonDateTimeFormats.TruncateToMillis(ticks)), offset);
            Kind = kind;
        }

        public RpDateTime(long ticks, TimeSpan offset)
        {
            DateTimeOffset = new DateTimeOffset(AdjustTicksOverflow(BaboonDateTimeFormats.TruncateToMillis(ticks)), offset);
            Kind = DetectKind(DateTimeOffset);
        }

        public RpDateTime(long ticks, DateTimeKind kind)
        {
            DateTimeOffset = new DateTime(AdjustTicksOverflow(BaboonDateTimeFormats.TruncateToMillis(ticks)), kind).ToDateTimeOffset();
            Kind = kind;
        }

        public RpDateTime(DateTime dateTime)
        {
            DateTimeOffset = dateTime.ToDateTimeOffset().TruncateToMillis();
            Kind = dateTime.Kind;
        }

        public override int GetHashCode()
        {
            return DateTimeOffset.GetHashCode();
        }

        public override bool Equals(object? obj)
        {
            if (obj is not RpDateTime other) return false;
            return other.DateTimeOffset == DateTimeOffset;
        }

        public bool Equals(RpDateTime other)
        {
            return other.DateTimeOffset == DateTimeOffset;
        }

        public override string ToString() => BaboonDateTimeFormats.ToString(this);
        public string ToString(string format) => DateTimeOffset.ToString(format);

        public TimeSpan Offset => DateTimeOffset.Offset;
        public TimeSpan GetUtcOffset() => DateTimeOffset.Offset;

        public RpDateTime ToUniversalTime(bool ignoreDaylightSavingTime = false)
        {
            if (!ignoreDaylightSavingTime)
            {
                // convert to UTC time with system converter
                return new RpDateTime(DateTimeOffset.ToUniversalTime(), DateTimeKind.Utc);
            }

            if (DateTimeOffset.Offset.Ticks == 0)
            {
                return new RpDateTime(DateTimeOffset, DateTimeKind.Utc);
            }

            // compute local from utc
            var localTicks = AdjustTicksOverflow(DateTimeOffset.Ticks - DateTimeOffset.Offset.Ticks);

            return new RpDateTime(new DateTimeOffset(localTicks, TimeSpan.Zero), DateTimeKind.Utc);
        }

        public RpDateTime ToLocalTime(bool ignoreDaylightSavingTime = false)
        {
            if (!ignoreDaylightSavingTime)
            {
                // convert to local time with system converter
                return new RpDateTime(DateTimeOffset.ToLocalTime(), DateTimeKind.Local);
            }

            // using base utc offset
            var utcOffset = TimeZoneInfo.Local.BaseUtcOffset;

            // already local
            if (Offset.Ticks == utcOffset.Ticks)
            {
                return new RpDateTime(DateTimeOffset, DateTimeKind.Local);
            }

            // compute local from utc
            var localTicks = AdjustTicksOverflow(DateTimeOffset.Ticks - DateTimeOffset.Offset.Ticks + utcOffset.Ticks);

            return new RpDateTime(new DateTimeOffset(localTicks, utcOffset), DateTimeKind.Local);
        }

        /**
         * Forcefully convert this DateTime to a local time.
         */
        public RpDateTime ForceLocalTime(bool ignoreDaylightSavingTime = false)
        {
            return ignoreDaylightSavingTime ?
                new RpDateTime(Ticks, TimeZoneInfo.Local.BaseUtcOffset, DateTimeKind.Local) :
                new RpDateTime(new DateTime(Ticks, DateTimeKind.Local));
        }

        public RpDateTime LocalDate => ToLocalTime().TruncateToDays();
        public RpDateTime Date => this.TruncateToDays();

        public TimeSpan Subtract(RpDateTime right) => DateTimeOffset - right.DateTimeOffset;
        public RpDateTime Subtract(TimeSpan span) => new(DateTimeOffset.Subtract(span), Kind);
        public RpDateTime Add(TimeSpan value) => new(DateTimeOffset.Add(value), Kind);
        public RpDateTime AddTicks(long value) => new(DateTimeOffset.AddTicks(value), Kind);
        public RpDateTime AddMilliseconds(double value) => new(DateTimeOffset.AddMilliseconds(value), Kind);
        public RpDateTime AddSeconds(double value) => new(DateTimeOffset.AddSeconds(value), Kind);
        public RpDateTime AddMinutes(double value) => new(DateTimeOffset.AddMinutes(value), Kind);
        public RpDateTime AddHours(double value) => new(DateTimeOffset.AddHours(value), Kind);
        public RpDateTime AddDays(double value) => new(DateTimeOffset.AddDays(value), Kind);
        public RpDateTime AddMonths(int value) => new(DateTimeOffset.AddMonths(value), Kind);
        public RpDateTime AddYears(int value) => new(DateTimeOffset.AddYears(value), Kind);

        public int DiffInFullHours(RpDateTime other) => (int) (this - other).TotalHours;
        public int DiffInFullDays(RpDateTime other) => (int) (this - other).TotalDays;
        public int DiffInFullWeeks(RpDateTime other) => DiffInFullDays(other) / 7;
        public int DiffInFullMonths(RpDateTime other)
        {
            var thisDt = ToUniversalTime();
            var otherDt = other.ToUniversalTime();
            return thisDt.Date == otherDt.Date ? 0 : (thisDt.Year - otherDt.Year) * 12 + thisDt.Month - otherDt.Month + thisDt.GetMonthsDiffByDays(otherDt);
        }
        public int DiffInFullYears(RpDateTime other) => DiffInFullMonths(other) / 12;

        public static RpDateTime Now => new(DateTimeOffset.Now, DateTimeKind.Local);
        public static RpDateTime UtcNow => new(DateTimeOffset.UtcNow, DateTimeKind.Utc);
        public static RpDateTime Epoch => new(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
        public static RpDateTime MinValue => new(DateTimeOffset.MinValue);
        public static RpDateTime MaxValue => new(DateTimeOffset.MaxValue);

        public int Year => DateTime.Year;
        public int Month => DateTime.Month;
        public int Day => DateTime.Day;
        public int DayOfYear => DateTime.DayOfYear;
        public int Hour => DateTime.Hour;
        public int Minute => DateTime.Minute;
        public int Second => DateTime.Second;

        public TimeSpan TimeOfDay => DateTime.TimeOfDay;
        public DayOfWeek DayOfWeek => DateTime.DayOfWeek;

        public int CompareTo(object? obj)
        {
            if (obj == null) return 1; // same as in DateTimeOffset.CompareTo
            if (obj is RpDateTime dt) return DateTimeOffset.CompareTo(dt.DateTimeOffset);
            throw new ArgumentException("Argument is not RpDateTime.");
        }

        public int CompareTo(RpDateTime other)
        {
            return DateTimeOffset.CompareTo(other.DateTimeOffset);
        }

        public static RpDateTime operator -(RpDateTime left, TimeSpan delta)
        {
            return new RpDateTime(left.DateTimeOffset - delta, left.Kind);
        }

        public static RpDateTime operator +(RpDateTime left, TimeSpan delta)
        {
            return new RpDateTime(left.DateTimeOffset + delta, left.Kind);
        }

        public static bool operator ==(RpDateTime left, RpDateTime right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(RpDateTime left, RpDateTime right)
        {
            return !(left == right);
        }

        public static TimeSpan operator -(RpDateTime left, RpDateTime right)
        {
            return left.DateTimeOffset - right.DateTimeOffset;
        }

        public static bool operator >(RpDateTime left, RpDateTime right)
        {
            return left.DateTimeOffset > right.DateTimeOffset;
        }

        public static bool operator <(RpDateTime left, RpDateTime right)
        {
            return left.DateTimeOffset < right.DateTimeOffset;
        }

        public static bool operator <=(RpDateTime left, RpDateTime right)
        {
            return left.DateTimeOffset <= right.DateTimeOffset;
        }

        public static bool operator >=(RpDateTime left, RpDateTime right)
        {
            return left.DateTimeOffset >= right.DateTimeOffset;
        }

        public static implicit operator RpDateTime(DateTime dt) => new(dt);
        public static implicit operator DateTime(RpDateTime dt) => dt.DateTime;
        public static implicit operator DateTimeOffset(RpDateTime dt) => dt.DateTimeOffset;

        public static RpDateTime Parse(string dt) => BaboonDateTimeFormats.FromString(dt);
        public static bool TryParse(string dt, out RpDateTime value) => BaboonDateTimeFormats.TryFromString(dt, out value);

        private int GetMonthsDiffByDays(RpDateTime other)
        {
            var thisDt = ToUniversalTime();
            var otherDt = other.ToUniversalTime();

            var thisDaysInMonth = DateTime.DaysInMonth(thisDt.Year, thisDt.Month);
            var otherDaysInMonth = DateTime.DaysInMonth(otherDt.Year, otherDt.Month);

            var thisDay = thisDaysInMonth < otherDaysInMonth ? thisDt.Day : thisDaysInMonth == thisDt.Day ? otherDaysInMonth : thisDt.Day;
            var otherDay = otherDaysInMonth < thisDaysInMonth ? otherDt.Day : otherDaysInMonth == otherDt.Day ? thisDaysInMonth : otherDt.Day;

            if (thisDt < other)
            {
                if (thisDay > otherDay) return 1;
                if (thisDay == otherDay && thisDt.TimeOfDay > otherDt.TimeOfDay) return 1;
            }
            else
            {
                if (thisDay < otherDay) return -1;
                if (thisDay == otherDay && thisDt.TimeOfDay < otherDt.TimeOfDay) return -1;
            }

            return 0;
        }

        private static long AdjustTicksOverflow(long ticks)
        {
            // adjust ticks count, ignoring overflow
            if (ticks < DateTime.MinValue.Ticks)
            {
                return DateTime.MinValue.Ticks;
            }

            if (ticks > DateTime.MaxValue.Ticks)
            {
                return DateTime.MaxValue.Ticks;
            }

            return ticks;
        }

        private static DateTimeKind DetectKind(DateTimeOffset dateTimeOffset)
        {
            if (dateTimeOffset.Offset.Ticks == 0)
            {
                return DateTimeKind.Utc;
            }

            if (dateTimeOffset.Offset == TimeZoneInfo.Local.GetUtcOffset(dateTimeOffset))
            {
                return DateTimeKind.Local;
            }

            return DateTimeKind.Unspecified;
        }

        private sealed class JsonConverter : JsonConverter<RpDateTime>
        {
            public override void WriteJson(JsonWriter writer, RpDateTime value, JsonSerializer serializer)
            {
                writer.WriteValue(BaboonDateTimeFormats.ToString(value));
            }

            public override RpDateTime ReadJson(JsonReader reader, Type objectType, RpDateTime existingValue, bool hasExistingValue, JsonSerializer serializer)
            {
                return BaboonDateTimeFormats.FromString((string) reader.Value!);
            }
        }
    }

    public static class BaboonDateTimeFormats
    {
        public static readonly string TslDefault = "yyyy-MM-ddTHH:mm:ss.fff";

        public static readonly string[] Tsl = new string[]
        {
            "yyyy-MM-ddTHH:mm:ss",
            "yyyy-MM-ddTHH:mm:ss.f",
            "yyyy-MM-ddTHH:mm:ss.ff",
            "yyyy-MM-ddTHH:mm:ss.fff",
            "yyyy-MM-ddTHH:mm:ss.ffff",
            "yyyy-MM-ddTHH:mm:ss.fffff",
            "yyyy-MM-ddTHH:mm:ss.ffffff",
            "yyyy-MM-ddTHH:mm:ss.fffffff",
            "yyyy-MM-ddTHH:mm:ss.ffffffff",
            "yyyy-MM-ddTHH:mm:ss.fffffffff"
        };

        public const string TszDefault = "yyyy-MM-ddTHH:mm:ss.fffzzz";

        public static readonly string[] Tsz = new string[]
        {
            "yyyy-MM-ddTHH:mm:ssZ",
            "yyyy-MM-ddTHH:mm:ss.fZ",
            "yyyy-MM-ddTHH:mm:ss.ffZ",
            "yyyy-MM-ddTHH:mm:ss.fffZ",
            "yyyy-MM-ddTHH:mm:ss.ffffZ",
            "yyyy-MM-ddTHH:mm:ss.fffffZ",
            "yyyy-MM-ddTHH:mm:ss.ffffffZ",
            "yyyy-MM-ddTHH:mm:ss.fffffffZ",
            "yyyy-MM-ddTHH:mm:ss.ffffffffZ",
            "yyyy-MM-ddTHH:mm:ss.fffffffffZ",
            "yyyy-MM-ddTHH:mm:sszzz",
            "yyyy-MM-ddTHH:mm:ss.fzzz",
            "yyyy-MM-ddTHH:mm:ss.ffzzz",
            "yyyy-MM-ddTHH:mm:ss.fffzzz",
            "yyyy-MM-ddTHH:mm:ss.ffffzzz",
            "yyyy-MM-ddTHH:mm:ss.fffffzzz",
            "yyyy-MM-ddTHH:mm:ss.ffffffzzz",
            "yyyy-MM-ddTHH:mm:ss.fffffffzzz",
            "yyyy-MM-ddTHH:mm:ss.ffffffffzzz",
            "yyyy-MM-ddTHH:mm:ss.fffffffffzzz"
        };

        public const string TsuDefault = "yyyy-MM-ddTHH:mm:ss.fffZ";
        public static readonly string[] Tsu = Tsz;

        public static string ToString(DateTime dt)
        {
            var format = dt.Kind == DateTimeKind.Utc ? TsuDefault : TszDefault;
            return dt.ToString(format, CultureInfo.InvariantCulture);
        }

        public static string ToString(RpDateTime dt)
        {
            var format = dt.DateTimeOffset.Offset.Ticks == 0 && dt.Kind == DateTimeKind.Utc ? TsuDefault : TszDefault;
            return dt.DateTimeOffset.ToString(format, CultureInfo.InvariantCulture);
        }

        public static RpDateTime FromString(string dt)
        {
            var dateTimeOffset = DateTimeOffset.ParseExact(dt, Tsz, CultureInfo.InvariantCulture, DateTimeStyles.None);
            return new RpDateTime(dateTimeOffset);
        }

        public static bool TryFromString(string dt, out RpDateTime value)
        {
            if (!DateTimeOffset.TryParseExact(dt, Tsz, CultureInfo.InvariantCulture, DateTimeStyles.None, out var dateTimeOffset))
            {
                value = default;
                return false;
            }

            value = new RpDateTime(dateTimeOffset);
            return true;
        }

        public static void EncodeToBin(RpDateTime dt, BinaryWriter writer)
        {
            // store not ticks, but milliseconds component for easier compatibility
            writer.Write(dt.DateTimeOffset.Ticks / TimeSpan.TicksPerMillisecond);
            writer.Write(dt.DateTimeOffset.Offset.Ticks / TimeSpan.TicksPerMillisecond);
            byte kindByte = dt.Kind switch
            {
                DateTimeKind.Unspecified => 0,
                DateTimeKind.Utc => 1,
                DateTimeKind.Local => 2,
                _ => throw new ArgumentOutOfRangeException(message: $"Unknown DateTimeKind: {Enum.GetName(dt.Kind.GetType(), dt.Kind)}.", null)
            };
            writer.Write(kindByte);
        }

        public static RpDateTime DecodeFromBin(BinaryReader reader)
        {
            // store not ticks, but milliseconds component for easier compatibility
            var ticks = reader.ReadInt64() * TimeSpan.TicksPerMillisecond;
            var offset = new TimeSpan(reader.ReadInt64() * TimeSpan.TicksPerMillisecond);
            var kind = reader.ReadByte() switch
            {
                0 => DateTimeKind.Unspecified,
                1 => DateTimeKind.Utc,
                2 => DateTimeKind.Local,
                var other => throw new ArgumentOutOfRangeException(message: $"Unknown DateTimeKind: {other}.", null)
            };
            return new RpDateTime(ticks, offset, kind);
        }

        public static DateTimeOffset ToDateTimeOffset(this DateTime dateTime)
        {
            if (dateTime.Kind == DateTimeKind.Utc)
            {
                // It's UTC DateTime, we can create DateTimeOffset with zero offset
                return new DateTimeOffset(dateTime.Ticks, TimeSpan.Zero);
            }

            // Local and Unspecified are both treated as Local
            var offset = TimeZoneInfo.Local.GetUtcOffset(dateTime);
            var utcTicks = dateTime.Ticks - offset.Ticks;

            // Adjust ticks to be sure we are in-range
            if (utcTicks <= DateTime.MinValue.Ticks) return DateTimeOffset.MinValue;
            if (utcTicks >= DateTime.MaxValue.Ticks) return DateTimeOffset.MaxValue;

            // Create DateTimeOffset with local time zone offset (with DST aplied)
            return new DateTimeOffset(dateTime.Ticks, offset);
        }

        private static long TruncateTo(long ticks, long unitTicks) => ticks - ticks % unitTicks;
        public static long TruncateToMillis(long ticks) => TruncateTo(ticks, TimeSpan.TicksPerMillisecond);
        public static long TruncateToSeconds(long ticks) => TruncateTo(ticks, TimeSpan.TicksPerSecond);
        public static long TruncateToMinutes(long ticks) => TruncateTo(ticks, TimeSpan.TicksPerMinute);
        public static long TruncateToHours(long ticks) => TruncateTo(ticks, TimeSpan.TicksPerHour);
        public static long TruncateToDays(long ticks) => TruncateTo(ticks, TimeSpan.TicksPerDay);

        public static DateTime TruncateToMillis(this DateTime dt) => new DateTime(TruncateToMillis(dt.Ticks), dt.Kind);
        public static DateTime TruncateToSeconds(this DateTime dt) => new DateTime(TruncateToSeconds(dt.Ticks), dt.Kind);
        public static DateTime TruncateToMinutes(this DateTime dt) => new DateTime(TruncateToMinutes(dt.Ticks), dt.Kind);
        public static DateTime TruncateToHours(this DateTime dt) => new DateTime(TruncateToHours(dt.Ticks), dt.Kind);
        public static DateTime TruncateToDays(this DateTime dt) => new DateTime(TruncateToDays(dt.Ticks), dt.Kind);

        public static DateTimeOffset TruncateToMillis(this DateTimeOffset dt) => new DateTimeOffset(TruncateToMillis(dt.Ticks), dt.Offset);
        public static DateTimeOffset TruncateToSeconds(this DateTimeOffset dt) => new DateTimeOffset(TruncateToSeconds(dt.Ticks), dt.Offset);
        public static DateTimeOffset TruncateToMinutes(this DateTimeOffset dt) => new DateTimeOffset(TruncateToMinutes(dt.Ticks), dt.Offset);
        public static DateTimeOffset TruncateToHours(this DateTimeOffset dt) => new DateTimeOffset(TruncateToHours(dt.Ticks), dt.Offset);
        public static DateTimeOffset TruncateToDays(this DateTimeOffset dt) => new DateTimeOffset(TruncateToDays(dt.Ticks), dt.Offset);

        public static RpDateTime TruncateToMillis(this RpDateTime dt) => new RpDateTime(dt.DateTimeOffset.TruncateToMillis(), dt.Kind);
        public static RpDateTime TruncateToSeconds(this RpDateTime dt) => new RpDateTime(dt.DateTimeOffset.TruncateToSeconds(), dt.Kind);
        public static RpDateTime TruncateToMinutes(this RpDateTime dt) => new RpDateTime(dt.DateTimeOffset.TruncateToMinutes(), dt.Kind);
        public static RpDateTime TruncateToHours(this RpDateTime dt) => new RpDateTime(dt.DateTimeOffset.TruncateToHours(), dt.Kind);
        public static RpDateTime TruncateToDays(this RpDateTime dt) => new RpDateTime(dt.DateTimeOffset.TruncateToDays(), dt.Kind);
    }
}
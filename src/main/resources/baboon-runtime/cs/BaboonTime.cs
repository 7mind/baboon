using System;
using System.Globalization;
using System.Linq;

using Newtonsoft.Json;

namespace Baboon.Time
{
    /** Reduced to milliseconds precision DateTime */
    [JsonConverter(typeof(JsonConverter))]
    public readonly struct RpDateTime : IComparable, IComparable<RpDateTime>, IEquatable<RpDateTime>
    {
        public readonly DateTimeOffset DateTimeOffset;

        public readonly DateTimeKind Kind;
        public DateTime DateTime => DateTime.SpecifyKind(DateTimeOffset.UtcDateTime, Kind);
        public long Ticks => DateTime.Ticks;

        public RpDateTime(DateTimeOffset dateTimeOffset)
        {
            DateTimeOffset = BaboonDateTimeFormats.TruncateToMilliseconds(dateTimeOffset);
            Kind = dateTimeOffset.Offset == TimeSpan.Zero ? DateTimeKind.Utc :
                dateTimeOffset.Offset == TimeZoneInfo.Local.BaseUtcOffset ? DateTimeKind.Local : DateTimeKind.Unspecified;
        }

        public RpDateTime(DateTimeOffset dateTimeOffset, DateTimeKind kind)
        {
            DateTimeOffset = BaboonDateTimeFormats.TruncateToMilliseconds(dateTimeOffset);
            Kind = kind;
        }

        public RpDateTime(int year, int month, int day)
        {
            DateTimeOffset = new DateTimeOffset(new DateTime(year, month, day));
            Kind = DateTimeKind.Local;
        }

        public RpDateTime(int year, int month, int day, int hour, int minute, int second)
        {
            DateTimeOffset = new DateTimeOffset(new DateTime(year, month, day, hour, minute, second));
            Kind = DateTimeKind.Local;
        }

        public RpDateTime(int year, int month, int day, int hour, int minute, int second, int millisecond, DateTimeKind kind)
        {
            DateTimeOffset = new DateTimeOffset(new DateTime(year, month, day, hour, minute, second, millisecond, kind));
            Kind = kind;
        }

        public RpDateTime(long ticks, DateTimeKind kind)
        {
            DateTimeOffset = new DateTimeOffset(BaboonDateTimeFormats.TruncateToMilliseconds(ticks, kind));
            Kind = kind;
        }

        public RpDateTime(DateTime dateTime)
        {
            DateTimeOffset = new DateTimeOffset(BaboonDateTimeFormats.TruncateToMilliseconds(dateTime));
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

        public RpDateTime ToUniversalTime()
        {
            if (Offset == TimeSpan.Zero)
            {
                return new RpDateTime(DateTimeOffset, DateTimeKind.Utc);
            }

            // compute local from utc
            var localTicks = DateTimeOffset.Ticks - DateTimeOffset.Offset.Ticks;

            // adjust ticks count, ignoring overflow
            if (localTicks < DateTime.MinValue.Ticks) localTicks = DateTime.MinValue.Ticks;
            if (localTicks > DateTime.MaxValue.Ticks) localTicks = DateTime.MaxValue.Ticks;

            return new RpDateTime(new DateTimeOffset(localTicks, TimeSpan.Zero), DateTimeKind.Utc);
        }

        public RpDateTime ToLocalTime()
        {
            // using base utc offset to ignore DST
            var utcOffset = TimeZoneInfo.Local.BaseUtcOffset;

            // already local
            if (Offset.Ticks == utcOffset.Ticks)
            {
                return new RpDateTime(DateTimeOffset, DateTimeKind.Local);
            }

            // compute local from utc
            var localTicks = DateTimeOffset.Ticks - DateTimeOffset.Offset.Ticks + utcOffset.Ticks;

            // adjust ticks count, ignoring overflow
            if (localTicks < DateTime.MinValue.Ticks) localTicks = DateTime.MinValue.Ticks;
            if (localTicks > DateTime.MaxValue.Ticks) localTicks = DateTime.MaxValue.Ticks;

            return new RpDateTime(new DateTimeOffset(localTicks, utcOffset), DateTimeKind.Local);
        }

        public RpDateTime LocalDate => new RpDateTime(BaboonDateTimeFormats.TruncateToDays(ToLocalTime().DateTimeOffset), DateTimeKind.Local);
        public RpDateTime Date => new RpDateTime(BaboonDateTimeFormats.TruncateToDays(DateTimeOffset), Kind);

        public TimeSpan Subtract(RpDateTime right) => DateTimeOffset - right.DateTimeOffset;
        public RpDateTime Subtract(TimeSpan span) => new RpDateTime(DateTimeOffset.Subtract(span), Kind);
        public RpDateTime Add(TimeSpan value) => new RpDateTime(DateTimeOffset.Add(value), Kind);
        public RpDateTime AddTicks(long value) => new RpDateTime(DateTimeOffset.AddTicks(value), Kind);
        public RpDateTime AddMilliseconds(double value) => new RpDateTime(DateTimeOffset.AddMilliseconds(value), Kind);
        public RpDateTime AddSeconds(double value) => new RpDateTime(DateTimeOffset.AddSeconds(value), Kind);
        public RpDateTime AddMinutes(double value) => new RpDateTime(DateTimeOffset.AddMinutes(value), Kind);
        public RpDateTime AddHours(double value) => new RpDateTime(DateTimeOffset.AddHours(value), Kind);
        public RpDateTime AddDays(double value) => new RpDateTime(DateTimeOffset.AddDays(value), Kind);
        public RpDateTime AddMonths(int value) => new RpDateTime(DateTimeOffset.AddMonths(value), Kind);
        public RpDateTime AddYears(int value) => new RpDateTime(DateTimeOffset.AddYears(value), Kind);

        public int DiffInFullHours(RpDateTime other) => (int)(this - other).TotalHours;
        public int DiffInFullDays(RpDateTime other) => (int)(this - other).TotalDays;
        public int DiffInFullWeeks(RpDateTime other) => DiffInFullDays(other) / 7;
        public int DiffInFullMonths(RpDateTime other) => Date == other.Date ? 0 : (Year - other.Year) * 12 + Month - other.Month + GetMonthsDiffByDays(other);
        public int DiffInFullYears(RpDateTime other) => DiffInFullMonths(other) / 12;

        public static RpDateTime Now => new RpDateTime(DateTime.Now);
        public static RpDateTime UtcNow => new RpDateTime(DateTime.UtcNow);
        public static RpDateTime Epoch => new RpDateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeKind.Utc);
        public static RpDateTime MinValue => new RpDateTime(DateTime.MinValue);
        public static RpDateTime MaxValue => new RpDateTime(DateTime.MaxValue);

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

        public static RpDateTime Parse(String dt) => BaboonDateTimeFormats.FromString(dt);

        private int GetMonthsDiffByDays(RpDateTime other)
        {
            var thisDaysInMonth = DateTime.DaysInMonth(Year, Month);
            var otherDaysInMonth = DateTime.DaysInMonth(other.Year, other.Month);

            var thisDay = thisDaysInMonth < otherDaysInMonth ? Day : thisDaysInMonth == Day ? otherDaysInMonth : Day;
            var otherDay = otherDaysInMonth < thisDaysInMonth ? other.Day : otherDaysInMonth == other.Day ? thisDaysInMonth : other.Day;

            if (this < other)
            {
                if (thisDay > otherDay) return 1;
                if (thisDay == otherDay && TimeOfDay > other.TimeOfDay) return 1;
            }
            else
            {
                if (thisDay < otherDay) return -1;
                if (thisDay == otherDay && TimeOfDay < other.TimeOfDay) return -1;
            }

            return 0;
        }

        private sealed class JsonConverter : JsonConverter<RpDateTime>
        {
            public override void WriteJson(JsonWriter writer, RpDateTime value, JsonSerializer serializer)
            {
                writer.WriteValue(BaboonDateTimeFormats.ToString(value));
            }

            public override RpDateTime ReadJson(JsonReader reader, Type objectType, RpDateTime existingValue, bool hasExistingValue, JsonSerializer serializer)
            {
                return BaboonDateTimeFormats.FromString((string)reader.Value!);
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

        public static readonly string TszDefault = "yyyy-MM-ddTHH:mm:ss.fffzzz";

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

        public static readonly string TsuDefault = "yyyy-MM-ddTHH:mm:ss.fffZ";
        public static readonly string[] Tsu = Tsz;

        public static string ToString(DateTime dt)
        {
            return dt.ToString(dt.Kind == DateTimeKind.Utc ? TsuDefault : TszDefault, CultureInfo.InvariantCulture);
        }

        public static string ToString(RpDateTime dt)
        {
            if (dt.DateTimeOffset.Offset == TimeSpan.Zero && dt.Kind == DateTimeKind.Utc)
            {
                return dt.DateTimeOffset.ToString(TsuDefault, CultureInfo.InvariantCulture);
            }

            return dt.DateTimeOffset.ToString(TszDefault, CultureInfo.InvariantCulture);
        }

        public static RpDateTime FromString(string dt)
        {
            var dateTimeOffset = DateTimeOffset.ParseExact(dt, Tsz, CultureInfo.InvariantCulture, DateTimeStyles.None);
            return new RpDateTime(dateTimeOffset);
        }

        public static DateTime TruncateToMilliseconds(long ticks, DateTimeKind kind)
        {
            return new DateTime(ticks - ticks % TimeSpan.TicksPerMillisecond, kind);
        }

        public static DateTime TruncateToMilliseconds(DateTime dateTime)
        {
            return TruncateToMilliseconds(dateTime.Ticks, dateTime.Kind);
        }

        public static DateTimeOffset TruncateToMilliseconds(DateTimeOffset dateTimeOffset)
        {
            return new DateTimeOffset(dateTimeOffset.Ticks - dateTimeOffset.Ticks % TimeSpan.TicksPerMillisecond, dateTimeOffset.Offset);
        }

        public static DateTimeOffset TruncateToDays(DateTimeOffset dateTimeOffset)
        {
            return new DateTimeOffset(dateTimeOffset.Ticks - dateTimeOffset.Ticks % TimeSpan.TicksPerDay, dateTimeOffset.Offset);
        }
    }
}
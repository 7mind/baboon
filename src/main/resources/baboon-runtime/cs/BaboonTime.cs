using System.Globalization;
using Newtonsoft.Json;
using System;

namespace Baboon.Time {
    /** Reduced to milliseconds precision DateTime */
    [JsonConverter(typeof(JsonConverter))]
    public readonly struct RpDateTime : IComparable, IComparable<RpDateTime>, IEquatable<RpDateTime>
    {
        public readonly DateTime DateTime;
    
        public RpDateTime(int year, int month, int day, int hour, int minute, int second, int millisecond, DateTimeKind kind)
        {
            DateTime = new DateTime(year, month, day, hour, minute, second, millisecond, kind);
        }
    
        public RpDateTime(long ticks, DateTimeKind kind)
        {
            DateTime = BaboonDateTimeFormats.TruncateToMilliseconds(ticks, kind);
        }
    
        public RpDateTime(DateTime dateTime)
        {
            DateTime = BaboonDateTimeFormats.TruncateToMilliseconds(dateTime);
        }
    
        public override int GetHashCode()
        {
            return DateTime.GetHashCode();
        }
    
        public override bool Equals(object? obj)
        {
            if (obj is not RpDateTime other) return false;
            return other.DateTime.ToUniversalTime() == DateTime.ToUniversalTime();
        }
    
        public bool Equals(RpDateTime other)
        {
            if (other == null!) return false;
            return other.DateTime.ToUniversalTime() == DateTime.ToUniversalTime();
        }
    
        public override string ToString() => BaboonDateTimeFormats.ToString(this);
        public string ToString(string format) => DateTime.ToString(format);
    
        public long Ticks => DateTime.Ticks;
        public DateTimeKind Kind => DateTime.Kind;
        public RpDateTime ToUniversalTime() => new RpDateTime(DateTime.ToUniversalTime());
        public RpDateTime ToLocalTime() => new RpDateTime(DateTime.ToLocalTime());
        public RpDateTime LocalDate => new RpDateTime(DateTime.ToLocalTime().Date);
        public RpDateTime Date => new RpDateTime(DateTime.Date);
        public TimeSpan GetUtcOffset() => TimeZoneInfo.Local.GetUtcOffset(DateTime);
        public TimeSpan Subtract(RpDateTime right) => DateTime.ToUniversalTime().Subtract(right.DateTime.ToUniversalTime());
        public RpDateTime Subtract(TimeSpan span) => new RpDateTime(DateTime.Subtract(span));
        public RpDateTime Add(TimeSpan value) => new RpDateTime(DateTime.Add(value));
        public RpDateTime AddTicks(long value) => new RpDateTime(DateTime.AddTicks(value));
        public RpDateTime AddMilliseconds(double value) => new RpDateTime(DateTime.AddMilliseconds(value));
        public RpDateTime AddSeconds(double value) => new RpDateTime(DateTime.AddSeconds(value));
        public RpDateTime AddMinutes(double value) => new RpDateTime(DateTime.AddMinutes(value));
        public RpDateTime AddHours(double value) => new RpDateTime(DateTime.AddHours(value));
        public RpDateTime AddDays(double value) => new RpDateTime(DateTime.AddDays(value));
        public RpDateTime AddMonths(int value) => new RpDateTime(DateTime.AddMonths(value));
        public RpDateTime AddYears(int value) => new RpDateTime(DateTime.AddYears(value));
    
        public int DiffInFullHours(RpDateTime other) => (int) (this - other).TotalHours;
        public int DiffInFullDays(RpDateTime other) => (int) (this - other).TotalDays;
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
    
        public TimeSpan TimeOfDay => DateTime.TimeOfDay;
        public DayOfWeek DayOfWeek => DateTime.DayOfWeek;
    
        public int CompareTo(object obj)
        {
            if (obj is RpDateTime dt) return DateTime.CompareTo(dt.DateTime);
            throw new ArgumentException("Argument is not RpDateTime.");
        }
    
        public int CompareTo(RpDateTime other)
        {
            return other == null! ? DateTime.CompareTo(null) : DateTime.CompareTo(other.DateTime);
        }
    
        public static RpDateTime operator -(RpDateTime left, TimeSpan delta)
        {
            return new RpDateTime(left.DateTime - delta);
        }
    
        public static RpDateTime operator +(RpDateTime left, TimeSpan delta)
        {
            return new RpDateTime(left.DateTime + delta);
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
            return left.DateTime.ToUniversalTime() - right.DateTime.ToUniversalTime();
        }
    
        public static bool operator >(RpDateTime left, RpDateTime right)
        {
            return left.DateTime.ToUniversalTime() > right.DateTime.ToUniversalTime();
        }
    
        public static bool operator <(RpDateTime left, RpDateTime right)
        {
            return left.DateTime.ToUniversalTime() < right.DateTime.ToUniversalTime();
        }
    
        public static bool operator <=(RpDateTime left, RpDateTime right)
        {
            return left.DateTime.ToUniversalTime() <= right.DateTime.ToUniversalTime();
        }
    
        public static bool operator >=(RpDateTime left, RpDateTime right)
        {
            return left.DateTime.ToUniversalTime() >= right.DateTime.ToUniversalTime();
        }
    
        public static implicit operator RpDateTime(DateTime dt) => new(dt);
        public static implicit operator DateTime(RpDateTime dt) => dt.DateTime;
    
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
                return BaboonDateTimeFormats.FromString((string) reader.Value!);
            }
        }
    }
    
    public static class BaboonDateTimeFormats
    {
        public static readonly String TslDefault = "yyyy-MM-ddTHH:mm:ss.fff";
    
        public static readonly String[] Tsl = new string[]
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
    
        public static readonly String TszDefault = "yyyy-MM-ddTHH:mm:ss.fffzzz";
    
        public static readonly String[] Tsz = new string[]
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
    
        public static readonly String TsuDefault = "yyyy-MM-ddTHH:mm:ss.fffZ";
        public static readonly String[] Tsu = Tsz;
    
        public static String ToString(DateTime dt)
        {
            return dt.ToString(dt.Kind == DateTimeKind.Utc ? TsuDefault : TszDefault, CultureInfo.InvariantCulture);
        }
    
        public static String ToString(RpDateTime dt)
        {
            return dt.DateTime.ToString(dt.DateTime.Kind == DateTimeKind.Utc ? TsuDefault : TszDefault, CultureInfo.InvariantCulture);
        }
    
        public static RpDateTime FromString(String dt)
        {
            return DateTime.ParseExact(dt, Tsz, CultureInfo.InvariantCulture, DateTimeStyles.None);
        }
    
        public static DateTime TruncateToMilliseconds(long ticks, DateTimeKind kind)
        {
            return new DateTime(ticks - (ticks % TimeSpan.TicksPerMillisecond), kind);
        }
    
        public static DateTime TruncateToMilliseconds(DateTime dateTime)
        {
            return TruncateToMilliseconds(dateTime.Ticks, dateTime.Kind);
        }
    }
}
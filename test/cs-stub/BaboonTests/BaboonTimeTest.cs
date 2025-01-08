using System;
using System.Globalization;
using System.IO;
using Baboon.Fixture;
using NUnit.Framework;

namespace Baboon.Time
{
    [TestFixture]
    public class RpDateTimeTest
    {
        [Test]
        public void DstTest()
        {
            const string testStringTime = "2020-09-30T23:39:22.925+02:00";
            var dateTime = RpDateTime.Parse(testStringTime);

            TestContext.Out.WriteLine("Base:");
            TestContext.Out.WriteLine(dateTime);
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To local:");
            TestContext.Out.WriteLine(dateTime.ToLocalTime());
            TestContext.Out.WriteLine(dateTime.ToUniversalTime().ToLocalTime());
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To UTC:");
            TestContext.Out.WriteLine(dateTime.ToLocalTime().ToUniversalTime());
            TestContext.Out.WriteLine(dateTime.ToUniversalTime());
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To local date:");
            TestContext.Out.WriteLine(dateTime.ToUniversalTime().LocalDate.LocalDate.LocalDate);
            TestContext.Out.WriteLine(dateTime.LocalDate.LocalDate.LocalDate);
            TestContext.Out.WriteLine(dateTime.LocalDate.LocalDate.LocalDate.ToUniversalTime());
            TestContext.Out.WriteLine(dateTime.LocalDate.LocalDate.LocalDate.ToLocalTime());
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To UTC date:");
            TestContext.Out.WriteLine(dateTime.ToUniversalTime().Date.Date.Date);
            TestContext.Out.WriteLine(dateTime.Date.Date.Date.ToUniversalTime().Date);
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To date:");
            TestContext.Out.WriteLine(dateTime.Date.Date.Date);
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To date, to local:");
            TestContext.Out.WriteLine(dateTime.Date.ToLocalTime());
            TestContext.Out.WriteLine(BaboonDateTimeFormats.ToString(dateTime.DateTimeOffset.TruncateToDays().ToLocalTime().DateTime));
            TestContext.Out.WriteLine(BaboonDateTimeFormats.ToString(DateTime.SpecifyKind(dateTime.DateTimeOffset.Date, DateTimeKind.Local).ToLocalTime()));
            TestContext.Out.WriteLine(BaboonDateTimeFormats.ToString(dateTime.DateTime.Date));
            TestContext.Out.WriteLine(BaboonDateTimeFormats.ToString(dateTime.DateTime.Date.ToLocalTime()));
            TestContext.Out.WriteLine();

            TestContext.Out.WriteLine("To local, to date:");
            TestContext.Out.WriteLine(dateTime.ToLocalTime().Date);
            TestContext.Out.WriteLine(dateTime.ToLocalTime().Date.ToLocalTime());
            TestContext.Out.WriteLine(new RpDateTime(dateTime.ToLocalTime().Date.ToLocalTime()));
            TestContext.Out.WriteLine();
        }

        [Test]
        public void BinCodecTest()
        {
            for (var i = 0; i < 10000; i++)
            {
                var dateTime = BaboonFixture.NextRpDateTime();

                var simpleDecoded = EncodeDecode(dateTime);
                Assert.That(dateTime, Is.EqualTo(simpleDecoded));

                var utcDecoded = EncodeDecode(dateTime.ToLocalTime());
                Assert.That(dateTime, Is.EqualTo(utcDecoded));

                var utcDstIgnoreDecoded = EncodeDecode(utcDecoded.ToLocalTime(ignoreDaylightSavingTime: true));
                Assert.That(dateTime, Is.EqualTo(utcDstIgnoreDecoded));

                var localDecoded = EncodeDecode(dateTime.ToLocalTime());
                Assert.That(dateTime, Is.EqualTo(localDecoded));

                var localDstIgnoreDecoded = EncodeDecode(localDecoded.ToLocalTime(ignoreDaylightSavingTime: true));
                Assert.That(dateTime, Is.EqualTo(localDstIgnoreDecoded));
            }

            return;

            RpDateTime EncodeDecode(RpDateTime dateTime)
            {
                byte[] encoded;
                using (var memoryStream = new MemoryStream())
                {
                    using var writer = new BinaryWriter(memoryStream);
                    BaboonDateTimeFormats.EncodeToBin(dateTime, writer);
                    encoded = memoryStream.ToArray();
                }

                RpDateTime decoded;
                using (var memoryStream = new MemoryStream(encoded))
                {
                    using var reader = new BinaryReader(memoryStream);
                    decoded = BaboonDateTimeFormats.DecodeFromBin(reader);
                }

                return decoded;
            }
        }

        [Test]
        public void OffsetTest()
        {
            const string testStringTime = "2024-07-08T00:00:00.000-07:00";

            var rpDateTime = RpDateTime.Parse(testStringTime);
            TestContext.Out.WriteLine("RpDateTime:");
            TestContext.Out.WriteLine(rpDateTime);
            TestContext.Out.WriteLine(rpDateTime.ForceLocalTime());
            TestContext.Out.WriteLine(rpDateTime.ForceLocalTime(ignoreDaylightSavingTime: true));
            TestContext.Out.WriteLine(new RpDateTime(rpDateTime.Ticks, DateTimeKind.Local));
            TestContext.Out.WriteLine();

            var dateTimeOffset = DateTimeOffset.ParseExact(testStringTime, BaboonDateTimeFormats.Tsz, CultureInfo.InvariantCulture, DateTimeStyles.None);
            TestContext.Out.WriteLine("DateTimeOffset:");
            TestContext.Out.WriteLine(new RpDateTime(dateTimeOffset));
            TestContext.Out.WriteLine(new RpDateTime(dateTimeOffset.DateTime.Ticks, DateTimeKind.Local));
            TestContext.Out.WriteLine();

            Assert.That(rpDateTime, Is.EqualTo(new RpDateTime(dateTimeOffset)));
            Assert.That(new RpDateTime(rpDateTime.Ticks, DateTimeKind.Local), Is.EqualTo(new RpDateTime(dateTimeOffset.DateTime.Ticks, DateTimeKind.Local)));
        }
    }
}
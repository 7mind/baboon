using Baboon.Time;
using NUnit.Framework;

namespace ConversionsTest;

[TestFixture]
public class RpDateTimeTest
{
    [Test]
    public void DstTest()
    {
        const string testStringTime = "1983-09-30T23:39:22.925+02:00";
        var dateTime = RpDateTime.Parse(testStringTime);

        // base
        TestContext.Out.WriteLine(dateTime);

        // local
        TestContext.Out.WriteLine(dateTime.ToLocalTime());
        TestContext.Out.WriteLine(dateTime.ToUniversalTime().ToLocalTime());

        // utc
        TestContext.Out.WriteLine(dateTime.ToLocalTime().ToUniversalTime());
        TestContext.Out.WriteLine(dateTime.ToUniversalTime());

        // local dates 
        TestContext.Out.WriteLine(dateTime.ToUniversalTime().LocalDate.LocalDate.LocalDate);
        TestContext.Out.WriteLine(dateTime.LocalDate.LocalDate.LocalDate);

        // utc dates 
        TestContext.Out.WriteLine(dateTime.ToUniversalTime().Date.Date.Date);
        TestContext.Out.WriteLine(dateTime.Date.Date.Date.ToUniversalTime());
    }
}
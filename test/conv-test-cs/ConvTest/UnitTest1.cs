using Convtest.Testpkg;

namespace ConvTest
{
    public class RequiredConversionsImpl : RequiredConversions
    {
        
    }
    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Test_Adt_Auto_Upgrade()
        {
            var a1 = new Convtest.Testpkg.v1_0_0.Adt0.B1("val1");

            var conv = new BaboonConversions(new RequiredConversionsImpl());
            
            var a1u1 = conv.Convert(a1).To<Adt0.B1>();
            Assert.That(a1.F == ((Adt0.B1)a1u1).F);
            
            var a1u2 = conv.Convert<Convtest.Testpkg.v1_0_0.Adt0>(a1).To<Adt0>();
            Assert.That(a1u1 == a1u2);
            
        }
    }
}
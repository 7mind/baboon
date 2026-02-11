#nullable enable

using System;
using System.IO;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    public class MockI1 : Testpkg.Pkg0.I1.I1
    {
        public Testpkg.Pkg0.I1.testCall.Out testCall(Testpkg.Pkg0.I1.testCall.In arg)
        {
            return new Testpkg.Pkg0.I1.testCall.Out(42);
        }

        public Testpkg.Pkg0.T7_Empty testCall2(Testpkg.Pkg0.T7_Empty arg)
        {
            return new Testpkg.Pkg0.T7_Empty();
        }
    }

    public class ThrowingI1 : Testpkg.Pkg0.I1.I1
    {
        public Testpkg.Pkg0.I1.testCall.Out testCall(Testpkg.Pkg0.I1.testCall.In arg)
        {
            throw new InvalidOperationException("service error");
        }

        public Testpkg.Pkg0.T7_Empty testCall2(Testpkg.Pkg0.T7_Empty arg)
        {
            throw new InvalidOperationException("service error");
        }
    }

    public class MockI2 : Testpkg.Pkg0.I2.I2
    {
        public Testpkg.Pkg0.I2.noErrCall.Out noErrCall(Testpkg.Pkg0.I2.noErrCall.In arg)
        {
            return new Testpkg.Pkg0.I2.noErrCall.Out("result_" + arg.Value);
        }
    }

    [TestFixture]
    public class WiringTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Default;

        // ==================== I1 JSON Tests ====================

        [Test]
        public void I1_InvokeJson_testCall_Success()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "testCall");

            // Encode input as JSON
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            // Invoke wiring
            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _ctx);

            // Decode output
            var outputToken = JToken.Parse(result);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeJson_testCall2_Success()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "testCall2");

            var inputJson = Testpkg.Pkg0.T7_Empty_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.T7_Empty())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _ctx);

            var outputToken = JToken.Parse(result);
            var decoded = Testpkg.Pkg0.T7_Empty_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded, Is.Not.Null);
        }

        [Test]
        public void I1_InvokeJson_NoMatchingMethod()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "nonexistent");

            var ex = Assert.Throws<BaboonWiringException>(() =>
                Testpkg.Pkg0.I1Wiring.InvokeJson(method, "{}", impl, _ctx));

            Assert.That(ex!.Error, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeJson_DecoderFailure()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "testCall");

            Assert.Catch<Exception>(() =>
                Testpkg.Pkg0.I1Wiring.InvokeJson(method, "not valid json!!", impl, _ctx));
        }

        [Test]
        public void I1_InvokeJson_ServiceThrows()
        {
            var impl = new ThrowingI1();
            var method = new BaboonMethodId("I1", "testCall");

            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var ex = Assert.Throws<InvalidOperationException>(() =>
                Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _ctx));

            Assert.That(ex!.Message, Is.EqualTo("service error"));
        }

        // ==================== I1 UEBA Tests ====================

        [Test]
        public void I1_InvokeUeba_testCall_Success()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "testCall");

            // Encode input
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(_ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            // Invoke wiring
            var resultBytes = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _ctx);

            // Decode output
            using var outputMs = new MemoryStream(resultBytes);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeUeba_testCall2_Success()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "testCall2");

            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.T7_Empty_UEBACodec.Instance.Encode(_ctx, inputWriter, new Testpkg.Pkg0.T7_Empty());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var resultBytes = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _ctx);

            using var outputMs = new MemoryStream(resultBytes);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.T7_Empty_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded, Is.Not.Null);
        }

        [Test]
        public void I1_InvokeUeba_NoMatchingMethod()
        {
            var impl = new MockI1();
            var method = new BaboonMethodId("I1", "nonexistent");

            var ex = Assert.Throws<BaboonWiringException>(() =>
                Testpkg.Pkg0.I1Wiring.InvokeUeba(method, Array.Empty<byte>(), impl, _ctx));

            Assert.That(ex!.Error, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeUeba_ServiceThrows()
        {
            var impl = new ThrowingI1();
            var method = new BaboonMethodId("I1", "testCall");

            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(_ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var ex = Assert.Throws<InvalidOperationException>(() =>
                Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _ctx));

            Assert.That(ex!.Message, Is.EqualTo("service error"));
        }

        // ==================== I2 Tests (no err type) ====================

        [Test]
        public void I2_InvokeJson_noErrCall_Success()
        {
            var impl = new MockI2();
            var method = new BaboonMethodId("I2", "noErrCall");

            var inputJson = Testpkg.Pkg0.I2.noErrCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I2.noErrCall.In(123))
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I2Wiring.InvokeJson(method, inputJson, impl, _ctx);

            var outputToken = JToken.Parse(result);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.Result, Is.EqualTo("result_123"));
        }

        [Test]
        public void I2_InvokeUeba_noErrCall_Success()
        {
            var impl = new MockI2();
            var method = new BaboonMethodId("I2", "noErrCall");

            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I2.noErrCall.In_UEBACodec.Instance.Encode(_ctx, inputWriter, new Testpkg.Pkg0.I2.noErrCall.In(456));
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var resultBytes = Testpkg.Pkg0.I2Wiring.InvokeUeba(method, inputBytes, impl, _ctx);

            using var outputMs = new MemoryStream(resultBytes);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.Result, Is.EqualTo("result_456"));
        }
    }
}

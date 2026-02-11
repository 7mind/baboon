#nullable enable

using System;
using System.IO;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    public class MockI1Either : Testpkg.Pkg0.I1.I1
    {
        public Either<Testpkg.Pkg0.I1.testCall.Err, Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            return new Either<Testpkg.Pkg0.I1.testCall.Err, Testpkg.Pkg0.I1.testCall.Out>.Right(
                new Testpkg.Pkg0.I1.testCall.Out(42));
        }

        public Either<Testpkg.Pkg0.T7_Empty, Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            return new Either<Testpkg.Pkg0.T7_Empty, Testpkg.Pkg0.T7_Empty>.Right(
                new Testpkg.Pkg0.T7_Empty());
        }
    }

    public class FailingI1Either : Testpkg.Pkg0.I1.I1
    {
        public Either<Testpkg.Pkg0.I1.testCall.Err, Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            return new Either<Testpkg.Pkg0.I1.testCall.Err, Testpkg.Pkg0.I1.testCall.Out>.Left(
                new Testpkg.Pkg0.I1.testCall.Err("domain error"));
        }

        public Either<Testpkg.Pkg0.T7_Empty, Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            return new Either<Testpkg.Pkg0.T7_Empty, Testpkg.Pkg0.T7_Empty>.Left(
                new Testpkg.Pkg0.T7_Empty());
        }
    }

    public class ThrowingI1Either : Testpkg.Pkg0.I1.I1
    {
        public Either<Testpkg.Pkg0.I1.testCall.Err, Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            throw new InvalidOperationException("service error");
        }

        public Either<Testpkg.Pkg0.T7_Empty, Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            throw new InvalidOperationException("service error");
        }
    }

    public class MockI2Either : Testpkg.Pkg0.I2.I2
    {
        public Testpkg.Pkg0.I2.noErrCall.Out noErrCall(Testpkg.Pkg0.I2.noErrCall.In arg)
        {
            return new Testpkg.Pkg0.I2.noErrCall.Out("result_" + arg.Value);
        }
    }

    [TestFixture]
    public class EitherWiringTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Default;
        private readonly Testpkg.Pkg0.BaboonServiceRtDefault _rt =
            Testpkg.Pkg0.BaboonServiceRtDefault.Instance;

        // ==================== I1 JSON Tests ====================

        [Test]
        public void I1_InvokeJson_testCall_Success()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Right>());
            var right = (Either<BaboonWiringError, string>.Right)result;
            var outputToken = JToken.Parse(right.Value);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeJson_testCall2_Success()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "testCall2");
            var inputJson = Testpkg.Pkg0.T7_Empty_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.T7_Empty())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Right>());
        }

        [Test]
        public void I1_InvokeJson_DomainError()
        {
            var impl = new FailingI1Either();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Left>());
            var left = (Either<BaboonWiringError, string>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        [Test]
        public void I1_InvokeJson_NoMatchingMethod()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "nonexistent");

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, "{}", impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Left>());
            var left = (Either<BaboonWiringError, string>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeJson_DecoderFailure()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "testCall");

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, "not valid json!!", impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Left>());
            var left = (Either<BaboonWiringError, string>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.DecoderFailed>());
        }

        [Test]
        public void I1_InvokeJson_ServiceThrows()
        {
            var impl = new ThrowingI1Either();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Left>());
            var left = (Either<BaboonWiringError, string>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        // ==================== I1 UEBA Tests ====================

        [Test]
        public void I1_InvokeUeba_testCall_Success()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "testCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, byte[]>.Right>());
            var right = (Either<BaboonWiringError, byte[]>.Right)result;
            using var outputMs = new MemoryStream(right.Value);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeUeba_NoMatchingMethod()
        {
            var impl = new MockI1Either();
            var method = new BaboonMethodId("I1", "nonexistent");

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(
                method, Array.Empty<byte>(), impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, byte[]>.Left>());
            var left = (Either<BaboonWiringError, byte[]>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeUeba_ServiceThrows()
        {
            var impl = new ThrowingI1Either();
            var method = new BaboonMethodId("I1", "testCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, byte[]>.Left>());
            var left = (Either<BaboonWiringError, byte[]>.Left)result;
            Assert.That(left.Value, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        // ==================== I2 Tests (no err type) ====================

        [Test]
        public void I2_InvokeJson_noErrCall_Success()
        {
            var impl = new MockI2Either();
            var method = new BaboonMethodId("I2", "noErrCall");
            var inputJson = Testpkg.Pkg0.I2.noErrCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I2.noErrCall.In(123))
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I2Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, string>.Right>());
            var right = (Either<BaboonWiringError, string>.Right)result;
            var outputToken = JToken.Parse(right.Value);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.Result, Is.EqualTo("result_123"));
        }

        [Test]
        public void I2_InvokeUeba_noErrCall_Success()
        {
            var impl = new MockI2Either();
            var method = new BaboonMethodId("I2", "noErrCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I2.noErrCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I2.noErrCall.In(456));
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I2Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Either<BaboonWiringError, byte[]>.Right>());
            var right = (Either<BaboonWiringError, byte[]>.Right)result;
            using var outputMs = new MemoryStream(right.Value);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.Result, Is.EqualTo("result_456"));
        }
    }
}

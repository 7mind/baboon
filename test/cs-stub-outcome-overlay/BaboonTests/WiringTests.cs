#nullable enable

using System;
using System.IO;
using Baboon.Runtime.Shared;
using CustomContainers;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    // Service method return types with Outcome<$success>:
    //   I1.testCall returns Outcome<Out> (err type is phantom)
    //   I1.testCall2 returns Outcome<T7_Empty>
    //   I2.noErrCall returns plain Out (no err type)

    public class MockI1Outcome : Testpkg.Pkg0.I1.I1
    {
        public Outcome<Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            return new Outcome<Testpkg.Pkg0.I1.testCall.Out>.Success(
                new Testpkg.Pkg0.I1.testCall.Out(42));
        }

        public Outcome<Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            return new Outcome<Testpkg.Pkg0.T7_Empty>.Success(
                new Testpkg.Pkg0.T7_Empty());
        }
    }

    public class FailingI1Outcome : Testpkg.Pkg0.I1.I1
    {
        public Outcome<Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            return new Outcome<Testpkg.Pkg0.I1.testCall.Out>.Failure(
                new Testpkg.Pkg0.I1.testCall.Err("domain error"));
        }

        public Outcome<Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            return new Outcome<Testpkg.Pkg0.T7_Empty>.Failure(
                new Testpkg.Pkg0.T7_Empty());
        }
    }

    public class ThrowingI1Outcome : Testpkg.Pkg0.I1.I1
    {
        public Outcome<Testpkg.Pkg0.I1.testCall.Out> testCall(
            Testpkg.Pkg0.I1.testCall.In arg)
        {
            throw new InvalidOperationException("service error");
        }

        public Outcome<Testpkg.Pkg0.T7_Empty> testCall2(
            Testpkg.Pkg0.T7_Empty arg)
        {
            throw new InvalidOperationException("service error");
        }
    }

    public class MockI2Outcome : Testpkg.Pkg0.I2.I2
    {
        public Testpkg.Pkg0.I2.noErrCall.Out noErrCall(Testpkg.Pkg0.I2.noErrCall.In arg)
        {
            return new Testpkg.Pkg0.I2.noErrCall.Out("result_" + arg.Value);
        }
    }

    [TestFixture]
    public class OutcomeWiringTests
    {
        private readonly BaboonCodecContext _ctx = BaboonCodecContext.Default;
        private readonly OutcomeServiceRt _rt = OutcomeServiceRt.Instance;

        // Wiring return type: Outcome<string> for JSON, Outcome<byte[]> for UEBA
        // Errors are tracked as object inside Outcome.Failure

        // ==================== I1 JSON Tests ====================

        [Test]
        public void I1_InvokeJson_testCall_Success()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Success>());
            var success = (Outcome<string>.Success)result;
            var outputToken = JToken.Parse(success.Value);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeJson_testCall2_Success()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "testCall2");
            var inputJson = Testpkg.Pkg0.T7_Empty_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.T7_Empty())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Success>());
        }

        [Test]
        public void I1_InvokeJson_DomainError()
        {
            var impl = new FailingI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Failure>());
            var failure = (Outcome<string>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        [Test]
        public void I1_InvokeJson_NoMatchingMethod()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "nonexistent");

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, "{}", impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Failure>());
            var failure = (Outcome<string>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeJson_DecoderFailure()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, "not valid json!!", impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Failure>());
            var failure = (Outcome<string>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.DecoderFailed>());
        }

        [Test]
        public void I1_InvokeJson_ServiceThrows()
        {
            var impl = new ThrowingI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");
            var inputJson = Testpkg.Pkg0.I1.testCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I1.testCall.In())
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I1Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Failure>());
            var failure = (Outcome<string>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        // ==================== I1 UEBA Tests ====================

        [Test]
        public void I1_InvokeUeba_testCall_Success()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<byte[]>.Success>());
            var success = (Outcome<byte[]>.Success)result;
            using var outputMs = new MemoryStream(success.Value);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I1.testCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.I00, Is.EqualTo(42));
        }

        [Test]
        public void I1_InvokeUeba_NoMatchingMethod()
        {
            var impl = new MockI1Outcome();
            var method = new BaboonMethodId("I1", "nonexistent");

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(
                method, Array.Empty<byte>(), impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<byte[]>.Failure>());
            var failure = (Outcome<byte[]>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.NoMatchingMethod>());
        }

        [Test]
        public void I1_InvokeUeba_ServiceThrows()
        {
            var impl = new ThrowingI1Outcome();
            var method = new BaboonMethodId("I1", "testCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I1.testCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I1.testCall.In());
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I1Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<byte[]>.Failure>());
            var failure = (Outcome<byte[]>.Failure)result;
            Assert.That(failure.Error, Is.InstanceOf<BaboonWiringError.CallFailed>());
        }

        // ==================== I2 Tests (no err type) ====================

        [Test]
        public void I2_InvokeJson_noErrCall_Success()
        {
            var impl = new MockI2Outcome();
            var method = new BaboonMethodId("I2", "noErrCall");
            var inputJson = Testpkg.Pkg0.I2.noErrCall.In_JsonCodec.Instance
                .Encode(_ctx, new Testpkg.Pkg0.I2.noErrCall.In(123))
                .ToString(Newtonsoft.Json.Formatting.None);

            var result = Testpkg.Pkg0.I2Wiring.InvokeJson(method, inputJson, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<string>.Success>());
            var success = (Outcome<string>.Success)result;
            var outputToken = JToken.Parse(success.Value);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_JsonCodec.Instance.Decode(_ctx, outputToken);
            Assert.That(decoded.Result, Is.EqualTo("result_123"));
        }

        [Test]
        public void I2_InvokeUeba_noErrCall_Success()
        {
            var impl = new MockI2Outcome();
            var method = new BaboonMethodId("I2", "noErrCall");
            using var inputMs = new MemoryStream();
            using var inputWriter = new BinaryWriter(inputMs);
            Testpkg.Pkg0.I2.noErrCall.In_UEBACodec.Instance.Encode(
                _ctx, inputWriter, new Testpkg.Pkg0.I2.noErrCall.In(456));
            inputWriter.Flush();
            var inputBytes = inputMs.ToArray();

            var result = Testpkg.Pkg0.I2Wiring.InvokeUeba(method, inputBytes, impl, _rt, _ctx);

            Assert.That(result, Is.InstanceOf<Outcome<byte[]>.Success>());
            var success = (Outcome<byte[]>.Success)result;
            using var outputMs = new MemoryStream(success.Value);
            using var outputReader = new BinaryReader(outputMs);
            var decoded = Testpkg.Pkg0.I2.noErrCall.Out_UEBACodec.Instance.Decode(_ctx, outputReader);
            Assert.That(decoded.Result, Is.EqualTo("result_456"));
        }
    }
}

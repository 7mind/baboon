import unittest
import json
from io import BytesIO

from BaboonDefinitions.Generated.baboon_service_wiring import (
    BaboonMethodId, NoMatchingMethod, CallFailed, DecoderFailed,
)
from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataInputStream, LEDataOutputStream
from BaboonDefinitions.Generated.testpkg.pkg0.I1 import I1
from BaboonDefinitions.Generated.testpkg.pkg0.I2 import I2
from BaboonDefinitions.Generated.testpkg.pkg0.I1_Wiring import invoke_json_I1, invoke_ueba_I1
from BaboonDefinitions.Generated.testpkg.pkg0.I2_Wiring import invoke_json_I2, invoke_ueba_I2
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.In import In as I1_In, In_UEBACodec as I1_In_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.Out import Out as I1_Out, Out_UEBACodec as I1_Out_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.Err import Err as I1_Err
from BaboonDefinitions.Generated.testpkg.pkg0.T7_Empty import T7_Empty, T7_Empty_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i2.noerrcall.In import In as I2_In, In_UEBACodec as I2_In_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i2.noerrcall.Out import Out as I2_Out, Out_UEBACodec as I2_Out_UEBACodec
from BaboonTests.custom_containers import OutcomeSuccess, OutcomeFailure, OutcomeServiceRt


class MockI1(I1):
    def testCall(self, arg):
        return OutcomeSuccess(I1_Out(i00=42))

    def testCall2(self, arg):
        return OutcomeSuccess(T7_Empty())


class FailingI1(I1):
    def testCall(self, arg):
        return OutcomeFailure(I1_Err(msg="domain error"))

    def testCall2(self, arg):
        return OutcomeFailure(T7_Empty())


class ThrowingI1(I1):
    def testCall(self, arg):
        raise RuntimeError("service error")

    def testCall2(self, arg):
        raise RuntimeError("service error")


class MockI2(I2):
    def noErrCall(self, arg):
        return I2_Out(result="result_" + str(arg.value))


class OutcomeWiringTests(unittest.TestCase):

    ctx = BaboonCodecContext.default()
    rt = OutcomeServiceRt()

    # ==================== I1 JSON Tests ====================

    def test_i1_json_testCall_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeSuccess)
        decoded = json.loads(result.value)
        self.assertEqual(decoded["i00"], 42)

    def test_i1_json_testCall2_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall2")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeSuccess)

    def test_i1_json_domain_error(self):
        impl = FailingI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, CallFailed)

    def test_i1_json_no_matching_method(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "nonexistent")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, NoMatchingMethod)

    def test_i1_json_decoder_failed(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "not valid json!!", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, DecoderFailed)

    def test_i1_json_service_throws(self):
        impl = ThrowingI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, CallFailed)

    # ==================== I1 UEBA Tests ====================

    def test_i1_ueba_testCall_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        I1_In_UEBACodec.instance().encode(self.ctx, writer, I1_In())
        input_bytes = buf.getvalue()
        result = invoke_ueba_I1(method, input_bytes, impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeSuccess)
        reader = LEDataInputStream(BytesIO(result.value))
        decoded = I1_Out_UEBACodec.instance().decode(self.ctx, reader)
        self.assertEqual(decoded.i00, 42)

    def test_i1_ueba_no_matching_method(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "nonexistent")
        result = invoke_ueba_I1(method, b'', impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, NoMatchingMethod)

    def test_i1_ueba_service_throws(self):
        impl = ThrowingI1()
        method = BaboonMethodId("I1", "testCall")
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        I1_In_UEBACodec.instance().encode(self.ctx, writer, I1_In())
        input_bytes = buf.getvalue()
        result = invoke_ueba_I1(method, input_bytes, impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeFailure)
        self.assertIsInstance(result.error, CallFailed)

    # ==================== I2 JSON Tests ====================

    def test_i2_json_success(self):
        impl = MockI2()
        method = BaboonMethodId("I2", "noErrCall")
        result = invoke_json_I2(method, '{"value": 123}', impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeSuccess)
        decoded = json.loads(result.value)
        self.assertEqual(decoded["result"], "result_123")

    # ==================== I2 UEBA Tests ====================

    def test_i2_ueba_success(self):
        impl = MockI2()
        method = BaboonMethodId("I2", "noErrCall")
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        I2_In_UEBACodec.instance().encode(self.ctx, writer, I2_In(value=456))
        input_bytes = buf.getvalue()
        result = invoke_ueba_I2(method, input_bytes, impl, self.rt, self.ctx)
        self.assertIsInstance(result, OutcomeSuccess)
        reader = LEDataInputStream(BytesIO(result.value))
        decoded = I2_Out_UEBACodec.instance().decode(self.ctx, reader)
        self.assertEqual(decoded.result, "result_456")


if __name__ == "__main__":
    unittest.main()

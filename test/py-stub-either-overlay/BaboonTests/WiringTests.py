import unittest
import json
from io import BytesIO

from BaboonDefinitions.Generated.baboon_service_wiring import (
    BaboonMethodId, BaboonLeft, BaboonRight,
    NoMatchingMethod, CallFailed, DecoderFailed,
    JsonMuxer, UebaMuxer, BaboonWiringException, NoMatchingService, DuplicateService,
)
from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataInputStream, LEDataOutputStream
from BaboonDefinitions.Generated.testpkg.pkg0.BaboonServiceRt import BaboonServiceRtDefault
from BaboonDefinitions.Generated.testpkg.pkg0.I1 import I1
from BaboonDefinitions.Generated.testpkg.pkg0.I2 import I2
from BaboonDefinitions.Generated.testpkg.pkg0.I1_Wiring import invoke_json_I1, invoke_ueba_I1, I1_JsonService, I1_UebaService
from BaboonDefinitions.Generated.testpkg.pkg0.I2_Wiring import invoke_json_I2, invoke_ueba_I2, I2_JsonService, I2_UebaService
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.In import In as I1_In, In_UEBACodec as I1_In_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.Out import Out as I1_Out, Out_UEBACodec as I1_Out_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i1.testcall.Err import Err as I1_Err
from BaboonDefinitions.Generated.testpkg.pkg0.T7_Empty import T7_Empty, T7_Empty_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i2.noerrcall.In import In as I2_In, In_UEBACodec as I2_In_UEBACodec
from BaboonDefinitions.Generated.testpkg.pkg0.i2.noerrcall.Out import Out as I2_Out, Out_UEBACodec as I2_Out_UEBACodec


class MockI1(I1):
    def testCall(self, arg):
        return BaboonRight(I1_Out(i00=42))

    def testCall2(self, arg):
        return BaboonRight(T7_Empty())


class FailingI1(I1):
    def testCall(self, arg):
        return BaboonLeft(I1_Err(msg="domain error"))

    def testCall2(self, arg):
        return BaboonLeft(T7_Empty())


class ThrowingI1(I1):
    def testCall(self, arg):
        raise RuntimeError("service error")

    def testCall2(self, arg):
        raise RuntimeError("service error")


class MockI2(I2):
    def noErrCall(self, arg):
        return I2_Out(result="result_" + str(arg.value))


class EitherWiringTests(unittest.TestCase):

    ctx = BaboonCodecContext.default()
    rt = BaboonServiceRtDefault()

    # ==================== I1 JSON Tests ====================

    def test_i1_json_testCall_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonRight)
        decoded = json.loads(result.value)
        self.assertEqual(decoded["i00"], 42)

    def test_i1_json_testCall2_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall2")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonRight)

    def test_i1_json_domain_error(self):
        impl = FailingI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, CallFailed)

    def test_i1_json_no_matching_method(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "nonexistent")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, NoMatchingMethod)

    def test_i1_json_decoder_failed(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "not valid json!!", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, DecoderFailed)

    def test_i1_json_service_throws(self):
        impl = ThrowingI1()
        method = BaboonMethodId("I1", "testCall")
        result = invoke_json_I1(method, "{}", impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, CallFailed)

    # ==================== I1 UEBA Tests ====================

    def test_i1_ueba_testCall_success(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "testCall")
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        I1_In_UEBACodec.instance().encode(self.ctx, writer, I1_In())
        input_bytes = buf.getvalue()
        result = invoke_ueba_I1(method, input_bytes, impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonRight)
        reader = LEDataInputStream(BytesIO(result.value))
        decoded = I1_Out_UEBACodec.instance().decode(self.ctx, reader)
        self.assertEqual(decoded.i00, 42)

    def test_i1_ueba_no_matching_method(self):
        impl = MockI1()
        method = BaboonMethodId("I1", "nonexistent")
        result = invoke_ueba_I1(method, b'', impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, NoMatchingMethod)

    def test_i1_ueba_service_throws(self):
        impl = ThrowingI1()
        method = BaboonMethodId("I1", "testCall")
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        I1_In_UEBACodec.instance().encode(self.ctx, writer, I1_In())
        input_bytes = buf.getvalue()
        result = invoke_ueba_I1(method, input_bytes, impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, CallFailed)

    # ==================== I2 JSON Tests ====================

    def test_i2_json_success(self):
        impl = MockI2()
        method = BaboonMethodId("I2", "noErrCall")
        result = invoke_json_I2(method, '{"value": 123}', impl, self.rt, self.ctx)
        self.assertIsInstance(result, BaboonRight)
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
        self.assertIsInstance(result, BaboonRight)
        reader = LEDataInputStream(BytesIO(result.value))
        decoded = I2_Out_UEBACodec.instance().decode(self.ctx, reader)
        self.assertEqual(decoded.result, "result_456")


# ==================== Cross-domain Muxer ====================
# A single muxer composes the I1 (errors mode) and I2 (no-err mode)
# services and routes each call by method.service_name.

class MuxerWiringTests(unittest.TestCase):

    ctx = BaboonCodecContext.default()
    rt = BaboonServiceRtDefault()

    def _json_muxer(self):
        return JsonMuxer(
            I1_JsonService(MockI1(), self.rt),
            I2_JsonService(MockI2(), self.rt),
        )

    def _ueba_muxer(self):
        return UebaMuxer(
            I1_UebaService(MockI1(), self.rt),
            I2_UebaService(MockI2(), self.rt),
        )

    def test_json_muxer_routes_to_i1(self):
        method = BaboonMethodId("I1", "testCall")
        result = self._json_muxer().invoke(method, "{}", self.ctx)
        self.assertIsInstance(result, BaboonRight)
        self.assertEqual(json.loads(result.value)["i00"], 42)

    def test_json_muxer_routes_to_i2(self):
        method = BaboonMethodId("I2", "noErrCall")
        result = self._json_muxer().invoke(method, '{"value": 123}', self.ctx)
        self.assertIsInstance(result, BaboonRight)
        self.assertEqual(json.loads(result.value)["result"], "result_123")

    def test_json_muxer_no_matching_service(self):
        method = BaboonMethodId("Nonexistent", "x")
        with self.assertRaises(BaboonWiringException) as cm:
            self._json_muxer().invoke(method, "{}", self.ctx)
        self.assertIsInstance(cm.exception.error, NoMatchingService)

    def test_json_muxer_duplicate_service(self):
        with self.assertRaises(BaboonWiringException) as cm:
            JsonMuxer(
                I1_JsonService(MockI1(), self.rt),
                I1_JsonService(MockI1(), self.rt),
            )
        self.assertIsInstance(cm.exception.error, DuplicateService)

    def test_ueba_muxer_routes_to_i1(self):
        method = BaboonMethodId("I1", "testCall")
        buf = BytesIO()
        I1_In_UEBACodec.instance().encode(self.ctx, LEDataOutputStream(buf), I1_In())
        result = self._ueba_muxer().invoke(method, buf.getvalue(), self.ctx)
        self.assertIsInstance(result, BaboonRight)
        decoded = I1_Out_UEBACodec.instance().decode(self.ctx, LEDataInputStream(BytesIO(result.value)))
        self.assertEqual(decoded.i00, 42)

    def test_ueba_muxer_routes_to_i2(self):
        method = BaboonMethodId("I2", "noErrCall")
        buf = BytesIO()
        I2_In_UEBACodec.instance().encode(self.ctx, LEDataOutputStream(buf), I2_In(value=456))
        result = self._ueba_muxer().invoke(method, buf.getvalue(), self.ctx)
        self.assertIsInstance(result, BaboonRight)
        decoded = I2_Out_UEBACodec.instance().decode(self.ctx, LEDataInputStream(BytesIO(result.value)))
        self.assertEqual(decoded.result, "result_456")

    def test_ueba_muxer_no_matching_service(self):
        method = BaboonMethodId("Nonexistent", "x")
        with self.assertRaises(BaboonWiringException) as cm:
            self._ueba_muxer().invoke(method, b"", self.ctx)
        self.assertIsInstance(cm.exception.error, NoMatchingService)


if __name__ == "__main__":
    unittest.main()

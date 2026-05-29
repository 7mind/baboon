// Service-wiring + cross-domain muxer tests (no-errors service-result mode).
// Generated into this stub only by the `test-gen-kt-wiring` mdl action
// (rsync + codegen with --service-result-no-errors=true). Exercises the
// per-domain dispatchers (I1Wiring/I2Wiring) and the cross-domain
// JsonMuxer/UebaMuxer that routes by method.serviceName.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonMethodId
import baboon.runtime.shared.BaboonWiringError
import baboon.runtime.shared.BaboonWiringException
import baboon.runtime.shared.JsonMuxer
import baboon.runtime.shared.LEDataInputStream
import baboon.runtime.shared.LEDataOutputStream
import baboon.runtime.shared.UebaMuxer
import kotlinx.serialization.json.Json
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import testpkg.pkg0.I1
import testpkg.pkg0.I1JsonService
import testpkg.pkg0.I1UebaService
import testpkg.pkg0.I1Wiring
import testpkg.pkg0.I2
import testpkg.pkg0.I2JsonService
import testpkg.pkg0.I2UebaService
import testpkg.pkg0.I2Wiring
import testpkg.pkg0.T7_Empty
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

private class MockI1 : I1 {
    override fun testCall(arg: testpkg.pkg0.i1.testcall.In): testpkg.pkg0.i1.testcall.Out =
        testpkg.pkg0.i1.testcall.Out(i00 = 42)

    override fun testCall2(arg: T7_Empty): T7_Empty = T7_Empty()
}

private class MockI2 : I2 {
    override fun noErrCall(arg: testpkg.pkg0.i2.noerrcall.In): testpkg.pkg0.i2.noerrcall.Out =
        testpkg.pkg0.i2.noerrcall.Out(result = "result_" + arg.value.toString())
}

class WiringTests {
    private val ctx = BaboonCodecContext.Default

    private fun ueba(encode: (LEDataOutputStream) -> Unit): ByteArray {
        val buf = ByteArrayOutputStream()
        encode(LEDataOutputStream(buf))
        return buf.toByteArray()
    }

    // ==================== Per-domain dispatch ====================

    @Test
    fun i1JsonDispatchSuccess() {
        val method = BaboonMethodId("I1", "testCall")
        val input = testpkg.pkg0.i1.testcall.In_JsonCodec.encode(ctx, testpkg.pkg0.i1.testcall.In()).toString()
        val result = I1Wiring.invokeJson(method, input, MockI1(), ctx)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(result))
        assertEquals(42, decoded.i00)
    }

    @Test
    fun i1JsonDispatchNoMatchingMethod() {
        val method = BaboonMethodId("I1", "nonexistent")
        val ex = assertThrows(BaboonWiringException::class.java) {
            I1Wiring.invokeJson(method, "{}", MockI1(), ctx)
        }
        assertTrue(ex.error is BaboonWiringError.NoMatchingMethod)
    }

    @Test
    fun i2JsonDispatchSuccess() {
        val method = BaboonMethodId("I2", "noErrCall")
        val input = testpkg.pkg0.i2.noerrcall.In_JsonCodec.encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).toString()
        val result = I2Wiring.invokeJson(method, input, MockI2(), ctx)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(result))
        assertEquals("result_123", decoded.result)
    }

    // ==================== Cross-domain Muxer ====================
    // A single muxer composes I1 and I2 and routes by method.serviceName.

    private fun newJsonMuxer() = JsonMuxer<String>(I1JsonService(MockI1()), I2JsonService(MockI2()))
    private fun newUebaMuxer() = UebaMuxer<ByteArray>(I1UebaService(MockI1()), I2UebaService(MockI2()))

    @Test
    fun jsonMuxerRoutesToI1() {
        val method = BaboonMethodId("I1", "testCall")
        val input = testpkg.pkg0.i1.testcall.In_JsonCodec.encode(ctx, testpkg.pkg0.i1.testcall.In()).toString()
        val result = newJsonMuxer().invoke(method, input, ctx)
        val decoded = testpkg.pkg0.i1.testcall.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(result))
        assertEquals(42, decoded.i00)
    }

    @Test
    fun jsonMuxerRoutesToI2() {
        val method = BaboonMethodId("I2", "noErrCall")
        val input = testpkg.pkg0.i2.noerrcall.In_JsonCodec.encode(ctx, testpkg.pkg0.i2.noerrcall.In(value = 123)).toString()
        val result = newJsonMuxer().invoke(method, input, ctx)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_JsonCodec.decode(ctx, Json.parseToJsonElement(result))
        assertEquals("result_123", decoded.result)
    }

    @Test
    fun jsonMuxerNoMatchingService() {
        val method = BaboonMethodId("Nonexistent", "x")
        val ex = assertThrows(BaboonWiringException::class.java) { newJsonMuxer().invoke(method, "{}", ctx) }
        assertTrue(ex.error is BaboonWiringError.NoMatchingService)
    }

    @Test
    fun jsonMuxerDuplicateService() {
        val ex = assertThrows(BaboonWiringException::class.java) {
            JsonMuxer<String>(I1JsonService(MockI1()), I1JsonService(MockI1()))
        }
        assertTrue(ex.error is BaboonWiringError.DuplicateService)
    }

    @Test
    fun uebaMuxerRoutesToI1() {
        val method = BaboonMethodId("I1", "testCall")
        val input = ueba { testpkg.pkg0.i1.testcall.In_UEBACodec.instance.encode(ctx, it, testpkg.pkg0.i1.testcall.In()) }
        val result = newUebaMuxer().invoke(method, input, ctx)
        val decoded = testpkg.pkg0.i1.testcall.Out_UEBACodec.instance.decode(ctx, LEDataInputStream(ByteArrayInputStream(result)))
        assertEquals(42, decoded.i00)
    }

    @Test
    fun uebaMuxerRoutesToI2() {
        val method = BaboonMethodId("I2", "noErrCall")
        val input = ueba { testpkg.pkg0.i2.noerrcall.In_UEBACodec.instance.encode(ctx, it, testpkg.pkg0.i2.noerrcall.In(value = 456)) }
        val result = newUebaMuxer().invoke(method, input, ctx)
        val decoded = testpkg.pkg0.i2.noerrcall.Out_UEBACodec.instance.decode(ctx, LEDataInputStream(ByteArrayInputStream(result)))
        assertEquals("result_456", decoded.result)
    }

    @Test
    fun uebaMuxerNoMatchingService() {
        val method = BaboonMethodId("Nonexistent", "x")
        val ex = assertThrows(BaboonWiringException::class.java) { newUebaMuxer().invoke(method, ByteArray(0), ctx) }
        assertTrue(ex.error is BaboonWiringError.NoMatchingService)
    }
}

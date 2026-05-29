// Service-wiring + cross-domain muxer tests (no-errors service-result mode).
// Generated into this stub only by the `test-gen-jv-wiring` mdl action
// (rsync + codegen with --service-result-no-errors=true). Exercises the
// per-domain dispatchers (I1Wiring/I2Wiring) and the cross-domain
// JsonMuxer/UebaMuxer that routes by method.serviceName().
package runtime;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonMethodId;
import baboon.runtime.shared.BaboonWiringError;
import baboon.runtime.shared.BaboonWiringException;
import baboon.runtime.shared.JsonMuxer;
import baboon.runtime.shared.LEDataInputStream;
import baboon.runtime.shared.LEDataOutputStream;
import baboon.runtime.shared.UebaMuxer;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class WiringTests {
    private final BaboonCodecContext ctx = BaboonCodecContext.Compact;
    private final ObjectMapper mapper = new ObjectMapper();

    static final class MockI1 implements testpkg.pkg0.I1 {
        public testpkg.pkg0.i1.testcall.Out testCall(testpkg.pkg0.i1.testcall.In arg) {
            return new testpkg.pkg0.i1.testcall.Out(42);
        }
        public testpkg.pkg0.T7_Empty testCall2(testpkg.pkg0.T7_Empty arg) {
            return new testpkg.pkg0.T7_Empty();
        }
    }

    static final class MockI2 implements testpkg.pkg0.I2 {
        public testpkg.pkg0.i2.noerrcall.Out noErrCall(testpkg.pkg0.i2.noerrcall.In arg) {
            return new testpkg.pkg0.i2.noerrcall.Out("result_" + arg.value());
        }
    }

    private String encodeI1In() {
        return testpkg.pkg0.i1.testcall.i1_testcall_In_JsonCodec.INSTANCE
                .encode(ctx, new testpkg.pkg0.i1.testcall.In()).toString();
    }

    private String encodeI2In() {
        return testpkg.pkg0.i2.noerrcall.i2_noerrcall_In_JsonCodec.INSTANCE
                .encode(ctx, new testpkg.pkg0.i2.noerrcall.In(123)).toString();
    }

    private byte[] ueba(IoConsumer<LEDataOutputStream> enc) throws Exception {
        var oms = new ByteArrayOutputStream();
        var bw = new LEDataOutputStream(oms);
        enc.accept(bw);
        bw.flush();
        return oms.toByteArray();
    }

    @FunctionalInterface
    private interface IoConsumer<T> { void accept(T t) throws Exception; }

    // ==================== Per-domain dispatch ====================

    @Test
    public void i1JsonDispatchSuccess() throws Exception {
        var method = new BaboonMethodId("I1", "testCall");
        var result = testpkg.pkg0.I1Wiring.invokeJson(method, encodeI1In(), new MockI1(), ctx);
        var decoded = testpkg.pkg0.i1.testcall.i1_testcall_Out_JsonCodec.INSTANCE.decode(ctx, mapper.readTree(result));
        assertEquals(42, decoded.i00());
    }

    @Test
    public void i1JsonDispatchNoMatchingMethod() {
        var method = new BaboonMethodId("I1", "nonexistent");
        var ex = assertThrows(BaboonWiringException.class,
                () -> testpkg.pkg0.I1Wiring.invokeJson(method, "{}", new MockI1(), ctx));
        assertInstanceOf(BaboonWiringError.NoMatchingMethod.class, ex.getError());
    }

    @Test
    public void i2JsonDispatchSuccess() throws Exception {
        var method = new BaboonMethodId("I2", "noErrCall");
        var result = testpkg.pkg0.I2Wiring.invokeJson(method, encodeI2In(), new MockI2(), ctx);
        var decoded = testpkg.pkg0.i2.noerrcall.i2_noerrcall_Out_JsonCodec.INSTANCE.decode(ctx, mapper.readTree(result));
        assertEquals("result_123", decoded.result());
    }

    // ==================== Cross-domain Muxer ====================
    // A single muxer composes I1 and I2 and routes by method.serviceName().

    private JsonMuxer<String> newJsonMuxer() {
        return new JsonMuxer<>(
                new testpkg.pkg0.I1Wiring.JsonService(new MockI1()),
                new testpkg.pkg0.I2Wiring.JsonService(new MockI2()));
    }

    private UebaMuxer<byte[]> newUebaMuxer() {
        return new UebaMuxer<>(
                new testpkg.pkg0.I1Wiring.UebaService(new MockI1()),
                new testpkg.pkg0.I2Wiring.UebaService(new MockI2()));
    }

    @Test
    public void jsonMuxerRoutesToI1() throws Exception {
        var method = new BaboonMethodId("I1", "testCall");
        var result = newJsonMuxer().invoke(method, encodeI1In(), ctx);
        var decoded = testpkg.pkg0.i1.testcall.i1_testcall_Out_JsonCodec.INSTANCE.decode(ctx, mapper.readTree(result));
        assertEquals(42, decoded.i00());
    }

    @Test
    public void jsonMuxerRoutesToI2() throws Exception {
        var method = new BaboonMethodId("I2", "noErrCall");
        var result = newJsonMuxer().invoke(method, encodeI2In(), ctx);
        var decoded = testpkg.pkg0.i2.noerrcall.i2_noerrcall_Out_JsonCodec.INSTANCE.decode(ctx, mapper.readTree(result));
        assertEquals("result_123", decoded.result());
    }

    @Test
    public void jsonMuxerNoMatchingService() {
        var method = new BaboonMethodId("Nonexistent", "x");
        var ex = assertThrows(BaboonWiringException.class, () -> newJsonMuxer().invoke(method, "{}", ctx));
        assertInstanceOf(BaboonWiringError.NoMatchingService.class, ex.getError());
    }

    @Test
    public void jsonMuxerDuplicateService() {
        var ex = assertThrows(BaboonWiringException.class, () -> new JsonMuxer<>(
                new testpkg.pkg0.I1Wiring.JsonService(new MockI1()),
                new testpkg.pkg0.I1Wiring.JsonService(new MockI1())));
        assertInstanceOf(BaboonWiringError.DuplicateService.class, ex.getError());
    }

    @Test
    public void uebaMuxerRoutesToI1() throws Exception {
        var method = new BaboonMethodId("I1", "testCall");
        var input = ueba(bw -> testpkg.pkg0.i1.testcall.i1_testcall_In_UEBACodec.INSTANCE.encode(ctx, bw, new testpkg.pkg0.i1.testcall.In()));
        var result = newUebaMuxer().invoke(method, input, ctx);
        var decoded = testpkg.pkg0.i1.testcall.i1_testcall_Out_UEBACodec.INSTANCE.decode(ctx, new LEDataInputStream(new ByteArrayInputStream(result)));
        assertEquals(42, decoded.i00());
    }

    @Test
    public void uebaMuxerRoutesToI2() throws Exception {
        var method = new BaboonMethodId("I2", "noErrCall");
        var input = ueba(bw -> testpkg.pkg0.i2.noerrcall.i2_noerrcall_In_UEBACodec.INSTANCE.encode(ctx, bw, new testpkg.pkg0.i2.noerrcall.In(456)));
        var result = newUebaMuxer().invoke(method, input, ctx);
        var decoded = testpkg.pkg0.i2.noerrcall.i2_noerrcall_Out_UEBACodec.INSTANCE.decode(ctx, new LEDataInputStream(new ByteArrayInputStream(result)));
        assertEquals("result_456", decoded.result());
    }

    @Test
    public void uebaMuxerNoMatchingService() {
        var method = new BaboonMethodId("Nonexistent", "x");
        var ex = assertThrows(BaboonWiringException.class, () -> newUebaMuxer().invoke(method, new byte[0], ctx));
        assertInstanceOf(BaboonWiringError.NoMatchingService.class, ex.getError());
    }
}

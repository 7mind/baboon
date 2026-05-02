// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
// impl (last-wins). Kotlin already used a `@Volatile var` mutable singleton
// pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/kt-stub/.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import my.ok.m19.foreign.FStr_KeyCodec
import my.ok.m19.foreign.FStr_KeyCodecHost
import my.ok.m19.foreign.Holder
import my.ok.m19.foreign.Holder_JsonCodec
import my.ok.m19.foreign.ItemKey
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class KeyCodecHostLastWinsTest {

    private val ctx = BaboonCodecContext.Compact

    private class PrefixCodec(private val tag: String) : FStr_KeyCodec {
        override fun encodeKey(value: String): String = "$tag:$value"
        override fun decodeKey(s: String): String {
            val pfx = "$tag:"
            return if (s.startsWith(pfx)) s.substring(pfx.length) else s
        }
    }

    @Test
    fun registerBAfterRegisterAObservesB() {
        val original = Holder(m = linkedMapOf(ItemKey("k") to "v"))

        FStr_KeyCodecHost.register(PrefixCodec("A"))
        val encodedA = Holder_JsonCodec.encode(ctx, original).toString()
        assertTrue(encodedA.contains("A:k"),
            "expected A: prefix in encoded wire form, got $encodedA")

        FStr_KeyCodecHost.register(PrefixCodec("B"))
        val encodedB = Holder_JsonCodec.encode(ctx, original).toString()
        assertTrue(encodedB.contains("B:k"),
            "PR-26.2 last-wins regression: expected B: prefix after re-register, got $encodedB")
        assertFalse(encodedB.contains("A:k"),
            "PR-26.2 last-wins regression: A: prefix still present after B re-register, got $encodedB")

        // Restore identity-encoding default for any subsequent tests in this JVM.
        FStr_KeyCodecHost.register(object : FStr_KeyCodec {
            override fun encodeKey(value: String): String = value
            override fun decodeKey(s: String): String = s
        })
    }
}

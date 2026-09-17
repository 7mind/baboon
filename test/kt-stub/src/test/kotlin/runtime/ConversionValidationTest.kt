package runtime

import baboon.runtime.shared.*
import kotlin.test.*

class ConversionValidationTest {
    private open class Value(override val baboonTypeIdentifier: String) : BaboonGenerated {
        override val baboonDomainVersion = "1.0.0"
        override val baboonDomainIdentifier = "test"
        override val baboonSameInVersions = listOf("1.0.0")
        override val baboonForwardReadable = emptyMap<String, String>()
        override val baboonMinReaderVersions = emptyMap<String, String>()
    }
    private class Branch(id: String, override val baboonAdtTypeIdentifier: String) : Value(id), BaboonAdtMemberMeta {
        override val baboonAdtType: Class<*> = Value::class.java
    }
    private val conversions = object : AbstractBaboonConversions() {
        override val versionsFrom = listOf("1.0.0")
        override val versionTo = "2.0.0"
    }
    private open class Legacy(override val typeId: String, private val result: Value) :
        AbstractConversion<Value, Value>(Value::class.java, Value::class.java) {
        var calls = 0
        override val versionFrom = "1.0.0"
        override val versionTo = "2.0.0"
        override fun <C> doConvert(context: C?, conversions: AbstractBaboonConversions, from: Value): Value {
            calls++
            return result
        }
    }
    private class Renamed(source: String, private val target: String, result: Value) : Legacy(source, result) {
        var targetReads = 0
        override val targetTypeId: String get() { targetReads++; return target }
    }

    @Test fun legacyDefaultRetainsSameIdentifierValidation() {
        val good = Legacy("same", Value("same"))
        assertEquals("same", good.convert<Unit>(null, conversions, Value("same")).baboonTypeIdentifier)
        val wrongSource = Legacy("same", Value("same"))
        assertFailsWith<IllegalArgumentException> { wrongSource.convert<Unit>(null, conversions, Value("wrong")) }
        assertEquals(0, wrongSource.calls)
        val wrongResult = Legacy("same", Value("wrong"))
        assertFailsWith<IllegalArgumentException> { wrongResult.convert<Unit>(null, conversions, Value("same")) }
        assertEquals(1, wrongResult.calls)
    }

    @Test fun renamedResultUsesTargetIdentifierWithoutEagerHook() {
        val conversion = Renamed("old", "new", Value("new"))
        assertEquals(0, conversion.targetReads)
        assertFailsWith<IllegalArgumentException> { conversion.convert<Unit>(null, conversions, Value("wrong")) }
        assertEquals(0, conversion.calls)
        assertEquals(0, conversion.targetReads)
        assertEquals("new", conversion.convert<Unit>(null, conversions, Value("old")).baboonTypeIdentifier)
        assertEquals(1, conversion.calls)
        assertTrue(conversion.targetReads > 0)
    }

    @Test fun renamedResultRejectsSourceOrUnrelatedIdentifier() {
        for (id in listOf("old", "unrelated")) {
            val conversion = Renamed("old", "new", Value(id))
            assertFailsWith<IllegalArgumentException> { conversion.convert<Unit>(null, conversions, Value("old")) }
            assertEquals(1, conversion.calls)
        }
    }

    @Test fun adtValidationAcceptsParentOrExactBranchAndRejectsNeither() {
        val parent = Renamed("oldParent", "newParent", Branch("newBranch", "newParent"))
        assertTrue(parent.convert<Unit>(null, conversions, Branch("oldBranch", "oldParent")) is Branch)
        val exact = Renamed("oldBranch", "newBranch", Branch("newBranch", "otherParent"))
        assertTrue(exact.convert<Unit>(null, conversions, Branch("oldBranch", "otherParent")) is Branch)
        val wrong = Renamed("oldParent", "newParent", Branch("wrongBranch", "wrongParent"))
        assertFailsWith<IllegalArgumentException> { wrong.convert<Unit>(null, conversions, Branch("oldBranch", "oldParent")) }
    }

    @Test fun foreignValuesDoNotReadMetadataHooks() {
        val conversion = object : AbstractConversion<String, String>(String::class.java, String::class.java) {
            override val typeId: String get() = error("source metadata must not be read")
            override val targetTypeId: String get() = error("target metadata must not be read")
            override val versionFrom = "1.0.0"
            override val versionTo = "2.0.0"
            override fun <C> doConvert(context: C?, conversions: AbstractBaboonConversions, from: String) = from.uppercase()
        }
        assertEquals("VALUE", conversion.convert<Unit>(null, conversions, "value"))
    }
}

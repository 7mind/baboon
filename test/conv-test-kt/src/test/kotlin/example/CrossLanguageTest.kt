package example

import convtest.testpkg.AllBasicTypes
import convtest.testpkg.AllBasicTypes_JsonCodec
import convtest.testpkg.AllBasicTypes_UEBACodec
import convtest.testpkg.AnyShowcase
import convtest.testpkg.AnyShowcase_JsonCodec
import convtest.testpkg.AnyShowcase_UEBACodec
import convtest.testpkg.InnerPayload
import convtest.testpkg.InnerPayload_JsonCodec
import convtest.testpkg.InnerPayload_UEBACodec
import baboon.runtime.shared.AnyOpaque
import baboon.runtime.shared.AnyOpaqueJson
import baboon.runtime.shared.AnyOpaqueUeba
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.LEDataInputStream
import kotlinx.serialization.json.Json
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileInputStream
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class CrossLanguageTest {

    private val baseDir = File("../../target/compat-test").absoluteFile.normalize()
    private val ctx = BaboonCodecContext.Default

    private fun readJsonFile(source: String): AllBasicTypes {
        val file = File(baseDir, "$source-json/all-basic-types.json")
        val jsonStr = file.readText(Charsets.UTF_8)
        val json = Json.parseToJsonElement(jsonStr)
        return AllBasicTypes_JsonCodec.decode(ctx, json)
    }

    private fun readUebaFile(source: String): AllBasicTypes {
        val file = File(baseDir, "$source-ueba/all-basic-types.ueba")
        val fis = FileInputStream(file)
        try {
            val reader = LEDataInputStream(fis)
            return AllBasicTypes_UEBACodec.decode(ctx, reader)
        } finally {
            fis.close()
        }
    }

    private fun assertBasicFields(data: AllBasicTypes, label: String) {
        println("Successfully decoded $label: ${data.vstr}")
        assertEquals("Hello, Baboon!", data.vstr)
        assertEquals(123456, data.vi32)
        assertTrue(data.vbit)
    }

    @Test
    fun `Kotlin JSON should read Kotlin-generated JSON`() {
        assertBasicFields(readJsonFile("kotlin"), "Kotlin JSON")
    }

    @Test
    fun `Kotlin JSON should read Scala-generated JSON`() {
        assertBasicFields(readJsonFile("scala"), "Scala JSON")
    }

    @Test
    fun `Kotlin JSON should read CS-generated JSON`() {
        assertBasicFields(readJsonFile("cs"), "C# JSON")
    }

    @Test
    fun `Kotlin JSON should read Rust-generated JSON`() {
        assertBasicFields(readJsonFile("rust"), "Rust JSON")
    }

    @Test
    fun `Kotlin JSON should read Python-generated JSON`() {
        assertBasicFields(readJsonFile("python"), "Python JSON")
    }

    @Test
    fun `Kotlin JSON should read TypeScript-generated JSON`() {
        assertBasicFields(readJsonFile("typescript"), "TypeScript JSON")
    }

    @Test
    fun `Kotlin UEBA should read Kotlin-generated UEBA`() {
        assertBasicFields(readUebaFile("kotlin"), "Kotlin UEBA")
    }

    @Test
    fun `Kotlin UEBA should read Scala-generated UEBA`() {
        assertBasicFields(readUebaFile("scala"), "Scala UEBA")
    }

    @Test
    fun `Kotlin UEBA should read CS-generated UEBA`() {
        assertBasicFields(readUebaFile("cs"), "C# UEBA")
    }

    @Test
    fun `Kotlin UEBA should read Rust-generated UEBA`() {
        assertBasicFields(readUebaFile("rust"), "Rust UEBA")
    }

    @Test
    fun `Kotlin UEBA should read Python-generated UEBA`() {
        assertBasicFields(readUebaFile("python"), "Python UEBA")
    }

    @Test
    fun `Kotlin UEBA should read TypeScript-generated UEBA`() {
        assertBasicFields(readUebaFile("typescript"), "TypeScript UEBA")
    }

    @Test
    fun `Cross-language JSON should produce equivalent data`() {
        val kotlinData = readJsonFile("kotlin")
        val scalaData = readJsonFile("scala")
        val csData = readJsonFile("cs")
        val rustData = readJsonFile("rust")
        val tsData = readJsonFile("typescript")

        assertEquals(kotlinData, scalaData, "Kotlin and Scala JSON data should be equal")
        assertEquals(kotlinData, csData, "Kotlin and C# JSON data should be equal")
        assertEquals(kotlinData, rustData, "Kotlin and Rust JSON data should be equal")
        assertEquals(kotlinData, tsData, "Kotlin and TypeScript JSON data should be equal")
    }

    @Test
    fun `Kotlin JSON should read Java-generated JSON`() {
        assertBasicFields(readJsonFile("java"), "Java JSON")
    }

    @Test
    fun `Kotlin JSON should read Dart-generated JSON`() {
        assertBasicFields(readJsonFile("dart"), "Dart JSON")
    }

    @Test
    fun `Kotlin UEBA should read Java-generated UEBA`() {
        assertBasicFields(readUebaFile("java"), "Java UEBA")
    }

    @Test
    fun `Kotlin UEBA should read Dart-generated UEBA`() {
        assertBasicFields(readUebaFile("dart"), "Dart UEBA")
    }

    @Test
    fun `Cross-language UEBA should produce equivalent data`() {
        val kotlinData = readUebaFile("kotlin")
        val scalaData = readUebaFile("scala")
        val csData = readUebaFile("cs")
        val rustData = readUebaFile("rust")
        val pythonData = readUebaFile("python")
        val tsData = readUebaFile("typescript")

        assertEquals(kotlinData, scalaData, "Kotlin and Scala UEBA data should be equal")
        assertEquals(kotlinData, csData, "Kotlin and C# UEBA data should be equal")
        assertEquals(kotlinData, rustData, "Kotlin and Rust UEBA data should be equal")
        assertEquals(kotlinData, pythonData, "Kotlin and Python UEBA data should be equal")
        assertEquals(kotlinData, tsData, "Kotlin and TypeScript UEBA data should be equal")
    }

    @Test
    fun `Kotlin JSON should read Swift-generated JSON`() {
        val file = File(baseDir, "swift-json/all-basic-types.json")
        if (!file.exists()) { println("Swift JSON file not found, skipping"); return }
        assertBasicFields(readJsonFile("swift"), "Swift JSON")
    }

    @Test
    fun `Kotlin UEBA should read Swift-generated UEBA`() {
        val file = File(baseDir, "swift-ueba/all-basic-types.ueba")
        if (!file.exists()) { println("Swift UEBA file not found, skipping"); return }
        assertBasicFields(readUebaFile("swift"), "Swift UEBA")
    }

    // -----------------------------------------------------------------------------
    // AnyShowcase cross-language tests (M13 / PR 13.2)
    // -----------------------------------------------------------------------------

    private val expectedAnyPayloads = listOf(
        InnerPayload("variant-A", 1),
        InnerPayload("variant-B", 2),
        InnerPayload("variant-C", 3),
        InnerPayload("variant-D1", 4),
        InnerPayload("variant-D2", 5),
        InnerPayload("variant-D3", 6),
        InnerPayload("opt-any", 7),
        InnerPayload("lst-any-0", 8),
    )

    private fun readAnyShowcaseJson(source: String): AnyShowcase {
        val file = File(baseDir, "$source-json/any-showcase.json")
        val jsonStr = file.readText(Charsets.UTF_8)
        val json = Json.parseToJsonElement(jsonStr)
        return AnyShowcase_JsonCodec.decode(ctx, json)
    }

    private fun readAnyShowcaseUeba(source: String): AnyShowcase {
        val file = File(baseDir, "$source-ueba/any-showcase.ueba")
        val fis = FileInputStream(file)
        try {
            val r = LEDataInputStream(fis)
            return AnyShowcase_UEBACodec.decode(ctx, r)
        } finally {
            fis.close()
        }
    }

    private fun decodeInner(o: AnyOpaque): InnerPayload {
        return when (o) {
            is AnyOpaqueUeba -> {
                val r = LEDataInputStream(ByteArrayInputStream(o.bytes))
                try {
                    InnerPayload_UEBACodec.decode(BaboonCodecContext.Compact, r)
                } finally {
                    r.close()
                }
            }
            is AnyOpaqueJson -> InnerPayload_JsonCodec.decode(BaboonCodecContext.Compact, o.json)
        }
    }

    private fun decodeAllPayloads(v: AnyShowcase): List<InnerPayload> {
        val opt = v.optAny ?: error("optAny was null")
        val lst0 = v.lstAny.firstOrNull() ?: error("lstAny was empty")
        return listOf(
            decodeInner(v.vAnyA),
            decodeInner(v.vAnyB),
            decodeInner(v.vAnyC),
            decodeInner(v.vAnyD1),
            decodeInner(v.vAnyD2),
            decodeInner(v.vAnyD3),
            decodeInner(opt),
            decodeInner(lst0),
        )
    }

    private fun assertAnyShowcase(source: String, fmt: String, v: AnyShowcase) {
        val decoded = decodeAllPayloads(v)
        assertEquals(expectedAnyPayloads.size, decoded.size, "$source $fmt count mismatch")
        for (i in expectedAnyPayloads.indices) {
            assertEquals(expectedAnyPayloads[i], decoded[i], "$source $fmt payload $i mismatch")
        }
    }

    @Test fun anyShowcaseKotlinJson() = assertAnyShowcase("kotlin", "JSON", readAnyShowcaseJson("kotlin"))
    @Test fun anyShowcaseKotlinUeba() = assertAnyShowcase("kotlin", "UEBA", readAnyShowcaseUeba("kotlin"))
    @Test fun anyShowcaseScalaJson()  = assertAnyShowcase("scala",  "JSON", readAnyShowcaseJson("scala"))
    @Test fun anyShowcaseScalaUeba()  = assertAnyShowcase("scala",  "UEBA", readAnyShowcaseUeba("scala"))
    @Test fun anyShowcaseCsJson()     = assertAnyShowcase("cs",     "JSON", readAnyShowcaseJson("cs"))
    @Test fun anyShowcaseCsUeba()     = assertAnyShowcase("cs",     "UEBA", readAnyShowcaseUeba("cs"))
    @Test fun anyShowcaseRustJson()   = assertAnyShowcase("rust",   "JSON", readAnyShowcaseJson("rust"))
    @Test fun anyShowcaseRustUeba()   = assertAnyShowcase("rust",   "UEBA", readAnyShowcaseUeba("rust"))
    @Test fun anyShowcaseJavaJson()   = assertAnyShowcase("java",   "JSON", readAnyShowcaseJson("java"))
    @Test fun anyShowcaseJavaUeba()   = assertAnyShowcase("java",   "UEBA", readAnyShowcaseUeba("java"))

    @Test fun anyShowcasePythonJson() {
        val f = File(baseDir, "python-json/any-showcase.json")
        if (!f.exists()) { println("python any-showcase JSON not found, skipping"); return }
        assertAnyShowcase("python", "JSON", readAnyShowcaseJson("python"))
    }
    @Test fun anyShowcasePythonUeba() {
        val f = File(baseDir, "python-ueba/any-showcase.ueba")
        if (!f.exists()) { println("python any-showcase UEBA not found, skipping"); return }
        assertAnyShowcase("python", "UEBA", readAnyShowcaseUeba("python"))
    }
    @Test fun anyShowcaseTsJson() {
        val f = File(baseDir, "typescript-json/any-showcase.json")
        if (!f.exists()) { println("ts any-showcase JSON not found, skipping"); return }
        assertAnyShowcase("typescript", "JSON", readAnyShowcaseJson("typescript"))
    }
    @Test fun anyShowcaseTsUeba() {
        val f = File(baseDir, "typescript-ueba/any-showcase.ueba")
        if (!f.exists()) { println("ts any-showcase UEBA not found, skipping"); return }
        assertAnyShowcase("typescript", "UEBA", readAnyShowcaseUeba("typescript"))
    }
    @Test fun anyShowcaseDartJson() {
        val f = File(baseDir, "dart-json/any-showcase.json")
        if (!f.exists()) { println("dart any-showcase JSON not found, skipping"); return }
        assertAnyShowcase("dart", "JSON", readAnyShowcaseJson("dart"))
    }
    @Test fun anyShowcaseDartUeba() {
        val f = File(baseDir, "dart-ueba/any-showcase.ueba")
        if (!f.exists()) { println("dart any-showcase UEBA not found, skipping"); return }
        assertAnyShowcase("dart", "UEBA", readAnyShowcaseUeba("dart"))
    }
    @Test fun anyShowcaseSwiftJson() {
        val f = File(baseDir, "swift-json/any-showcase.json")
        if (!f.exists()) { println("swift any-showcase JSON not found, skipping"); return }
        assertAnyShowcase("swift", "JSON", readAnyShowcaseJson("swift"))
    }
    @Test fun anyShowcaseSwiftUeba() {
        val f = File(baseDir, "swift-ueba/any-showcase.ueba")
        if (!f.exists()) { println("swift any-showcase UEBA not found, skipping"); return }
        assertAnyShowcase("swift", "UEBA", readAnyShowcaseUeba("swift"))
    }

    @Test fun anyShowcaseUebaByteIdenticalKotlinScala() {
        val k = File(baseDir, "kotlin-ueba/any-showcase.ueba").readBytes()
        val s = File(baseDir, "scala-ueba/any-showcase.ueba").readBytes()
        assertContentEquals(s, k, "Kotlin and Scala UEBA bytes diverged")
    }

    @Test fun anyShowcaseUebaByteIdenticalKotlinCs() {
        val k = File(baseDir, "kotlin-ueba/any-showcase.ueba").readBytes()
        val cs = File(baseDir, "cs-ueba/any-showcase.ueba").readBytes()
        assertContentEquals(cs, k, "Kotlin and C# UEBA bytes diverged")
    }
}

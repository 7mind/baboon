package example

import convtest.testpkg.AllBasicTypes
import convtest.testpkg.AllBasicTypes_JsonCodec
import convtest.testpkg.AllBasicTypes_UEBACodec
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonBinaryReader
import kotlinx.serialization.json.Json
import java.io.File
import kotlin.test.Test
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
        val bytes = file.readBytes()
        val reader = BaboonBinaryReader(bytes)
        return AllBasicTypes_UEBACodec.decode(ctx, reader)
    }

    private fun assertBasicFields(data: AllBasicTypes, label: String) {
        println("Successfully decoded $label: ${data.vstr}")
        assertEquals("Hello, Baboon!", data.vstr)
        assertEquals(123456, data.vi32)
        assertTrue(data.vbit)
    }

    @Test
    fun `KMP JSON should read Kotlin-KMP-generated JSON`() {
        assertBasicFields(readJsonFile("kotlin-kmp"), "Kotlin-KMP JSON")
    }

    @Test
    fun `KMP JSON should read Scala-generated JSON`() {
        assertBasicFields(readJsonFile("scala"), "Scala JSON")
    }

    @Test
    fun `KMP JSON should read CS-generated JSON`() {
        assertBasicFields(readJsonFile("cs"), "C# JSON")
    }

    @Test
    fun `KMP JSON should read Rust-generated JSON`() {
        assertBasicFields(readJsonFile("rust"), "Rust JSON")
    }

    @Test
    fun `KMP JSON should read Python-generated JSON`() {
        assertBasicFields(readJsonFile("python"), "Python JSON")
    }

    @Test
    fun `KMP JSON should read TypeScript-generated JSON`() {
        assertBasicFields(readJsonFile("typescript"), "TypeScript JSON")
    }

    @Test
    fun `KMP JSON should read Kotlin-JVM-generated JSON`() {
        assertBasicFields(readJsonFile("kotlin"), "Kotlin-JVM JSON")
    }

    @Test
    fun `KMP JSON should read Java-generated JSON`() {
        assertBasicFields(readJsonFile("java"), "Java JSON")
    }

    @Test
    fun `KMP JSON should read Dart-generated JSON`() {
        assertBasicFields(readJsonFile("dart"), "Dart JSON")
    }

    @Test
    fun `KMP UEBA should read Kotlin-KMP-generated UEBA`() {
        assertBasicFields(readUebaFile("kotlin-kmp"), "Kotlin-KMP UEBA")
    }

    @Test
    fun `KMP UEBA should read Scala-generated UEBA`() {
        assertBasicFields(readUebaFile("scala"), "Scala UEBA")
    }

    @Test
    fun `KMP UEBA should read CS-generated UEBA`() {
        assertBasicFields(readUebaFile("cs"), "C# UEBA")
    }

    @Test
    fun `KMP UEBA should read Rust-generated UEBA`() {
        assertBasicFields(readUebaFile("rust"), "Rust UEBA")
    }

    @Test
    fun `KMP UEBA should read Python-generated UEBA`() {
        assertBasicFields(readUebaFile("python"), "Python UEBA")
    }

    @Test
    fun `KMP UEBA should read TypeScript-generated UEBA`() {
        assertBasicFields(readUebaFile("typescript"), "TypeScript UEBA")
    }

    @Test
    fun `KMP UEBA should read Kotlin-JVM-generated UEBA`() {
        assertBasicFields(readUebaFile("kotlin"), "Kotlin-JVM UEBA")
    }

    @Test
    fun `KMP UEBA should read Java-generated UEBA`() {
        assertBasicFields(readUebaFile("java"), "Java UEBA")
    }

    @Test
    fun `KMP UEBA should read Dart-generated UEBA`() {
        assertBasicFields(readUebaFile("dart"), "Dart UEBA")
    }

    @Test
    fun `KMP JSON should read Swift-generated JSON`() {
        val file = File(baseDir, "swift-json/all-basic-types.json")
        if (!file.exists()) { println("Swift JSON file not found, skipping"); return }
        assertBasicFields(readJsonFile("swift"), "Swift JSON")
    }

    @Test
    fun `KMP UEBA should read Swift-generated UEBA`() {
        val file = File(baseDir, "swift-ueba/all-basic-types.ueba")
        if (!file.exists()) { println("Swift UEBA file not found, skipping"); return }
        assertBasicFields(readUebaFile("swift"), "Swift UEBA")
    }

    @Test
    fun `Cross-language JSON should produce equivalent data via KMP`() {
        val kmpData = readJsonFile("kotlin-kmp")
        val scalaData = readJsonFile("scala")
        val csData = readJsonFile("cs")
        val rustData = readJsonFile("rust")
        val tsData = readJsonFile("typescript")

        assertEquals(kmpData, scalaData, "KMP and Scala JSON data should be equal")
        assertEquals(kmpData, csData, "KMP and C# JSON data should be equal")
        assertEquals(kmpData, rustData, "KMP and Rust JSON data should be equal")
        assertEquals(kmpData, tsData, "KMP and TypeScript JSON data should be equal")
    }

    @Test
    fun `Cross-language UEBA should produce equivalent data via KMP`() {
        val kmpData = readUebaFile("kotlin-kmp")
        val scalaData = readUebaFile("scala")
        val csData = readUebaFile("cs")
        val rustData = readUebaFile("rust")
        val pythonData = readUebaFile("python")
        val tsData = readUebaFile("typescript")

        assertEquals(kmpData, scalaData, "KMP and Scala UEBA data should be equal")
        assertEquals(kmpData, csData, "KMP and C# UEBA data should be equal")
        assertEquals(kmpData, rustData, "KMP and Rust UEBA data should be equal")
        assertEquals(kmpData, pythonData, "KMP and Python UEBA data should be equal")
        assertEquals(kmpData, tsData, "KMP and TypeScript UEBA data should be equal")
    }
}

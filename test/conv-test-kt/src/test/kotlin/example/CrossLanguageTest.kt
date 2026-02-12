package example

import convtest.testpkg.AllBasicTypes
import convtest.testpkg.AllBasicTypes_JsonCodec
import convtest.testpkg.AllBasicTypes_UEBACodec
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.LEDataInputStream
import kotlinx.serialization.json.Json
import java.io.File
import java.io.FileInputStream
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
    fun `Kotlin UEBA should read TypeScript-generated UEBA`() {
        assertBasicFields(readUebaFile("typescript"), "TypeScript UEBA")
    }

    @Test
    fun `Cross-language JSON should produce equivalent data`() {
        val kotlinData = readJsonFile("kotlin")
        val scalaData = readJsonFile("scala")
        val csData = readJsonFile("cs")
        val rustData = readJsonFile("rust")

        assertEquals(kotlinData, scalaData, "Kotlin and Scala JSON data should be equal")
        assertEquals(kotlinData, csData, "Kotlin and C# JSON data should be equal")
        assertEquals(kotlinData, rustData, "Kotlin and Rust JSON data should be equal")
    }

    @Test
    fun `Cross-language UEBA should produce equivalent data`() {
        val kotlinData = readUebaFile("kotlin")
        val scalaData = readUebaFile("scala")
        val csData = readUebaFile("cs")
        val rustData = readUebaFile("rust")

        assertEquals(kotlinData, scalaData, "Kotlin and Scala UEBA data should be equal")
        assertEquals(kotlinData, csData, "Kotlin and C# UEBA data should be equal")
        assertEquals(kotlinData, rustData, "Kotlin and Rust UEBA data should be equal")
    }
}

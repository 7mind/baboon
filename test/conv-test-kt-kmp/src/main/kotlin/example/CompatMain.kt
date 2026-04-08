@file:OptIn(ExperimentalUuidApi::class)

package example

import convtest.testpkg.AllBasicTypes
import convtest.testpkg.AllBasicTypes_JsonCodec
import convtest.testpkg.AllBasicTypes_UEBACodec
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.ByteString
import baboon.runtime.shared.BaboonBinaryReader
import baboon.runtime.shared.BaboonBinaryWriter
import baboon.runtime.shared.BaboonDecimal
import baboon.runtime.shared.BaboonOffsetDateTime
import baboon.runtime.shared.BaboonTimeFormats
import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import java.io.File
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    when {
        args.size >= 3 && args[0] == "write" -> {
            val outputDir = args[1]
            val format = args[2]
            File(outputDir).mkdirs()
            val sampleData = createSampleData()
            val ctx = BaboonCodecContext.Default
            when (format) {
                "json" -> writeJson(ctx, sampleData, outputDir)
                "ueba" -> writeUeba(ctx, sampleData, outputDir)
                else -> {
                    System.err.println("Unknown format: $format")
                    exitProcess(1)
                }
            }
        }
        args.size >= 2 && args[0] == "read" -> {
            readAndVerify(args[1])
        }
        else -> {
            runLegacy()
        }
    }
}

private fun runLegacy() {
    val sampleData = createSampleData()
    val baseDir = File("../../target/compat-test").absoluteFile.normalize()
    val kotlinKmpJsonDir = File(baseDir, "kotlin-kmp-json")
    val kotlinKmpUebaDir = File(baseDir, "kotlin-kmp-ueba")

    kotlinKmpJsonDir.mkdirs()
    kotlinKmpUebaDir.mkdirs()

    val ctx = BaboonCodecContext.Default
    writeJson(ctx, sampleData, kotlinKmpJsonDir.absolutePath)
    writeUeba(ctx, sampleData, kotlinKmpUebaDir.absolutePath)

    println("Kotlin KMP serialization complete!")
}

private fun writeJson(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String) {
    val json: JsonElement = AllBasicTypes_JsonCodec.encode(ctx, data)
    val jsonStr = json.toString()
    val jsonFile = File(outputDir, "all-basic-types.json")
    jsonFile.writeText(jsonStr, Charsets.UTF_8)
    println("Written JSON to ${jsonFile.absolutePath}")
}

private fun writeUeba(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String) {
    val uebaWriter = BaboonBinaryWriter()
    AllBasicTypes_UEBACodec.encode(ctx, uebaWriter, data)
    val uebaBytes = uebaWriter.toByteArray()
    val uebaFile = File(outputDir, "all-basic-types.ueba")
    uebaFile.writeBytes(uebaBytes)
    println("Written UEBA to ${uebaFile.absolutePath}")
}

private fun readAndVerify(filePath: String) {
    val ctx = BaboonCodecContext.Default
    val file = File(filePath)
    val data: AllBasicTypes

    try {
        if (filePath.endsWith(".json")) {
            val jsonStr = file.readText(Charsets.UTF_8)
            val jsonElement = Json.parseToJsonElement(jsonStr)
            data = AllBasicTypes_JsonCodec.decode(ctx, jsonElement)
        } else if (filePath.endsWith(".ueba")) {
            val bytes = file.readBytes()
            val uebaReader = BaboonBinaryReader(bytes)
            data = AllBasicTypes_UEBACodec.decode(ctx, uebaReader)
        } else {
            System.err.println("Unknown file extension: $filePath")
            exitProcess(1)
            return
        }
    } catch (e: Exception) {
        System.err.println("Deserialization failed: ${e.message}")
        exitProcess(1)
        return
    }

    if (data.vstr != "Hello, Baboon!") {
        System.err.println("vstr mismatch: expected 'Hello, Baboon!', got '${data.vstr}'")
        exitProcess(1)
    }
    if (data.vi32 != 123456) {
        System.err.println("vi32 mismatch: expected 123456, got ${data.vi32}")
        exitProcess(1)
    }
    if (!data.vbit) {
        System.err.println("vbit mismatch: expected true, got ${data.vbit}")
        exitProcess(1)
    }

    // Roundtrip
    try {
        if (filePath.endsWith(".json")) {
            val reEncoded = AllBasicTypes_JsonCodec.encode(ctx, data)
            val reDecoded = AllBasicTypes_JsonCodec.decode(ctx, reEncoded)
            if (data != reDecoded) {
                System.err.println("JSON roundtrip mismatch")
                exitProcess(1)
            }
        } else {
            val w = BaboonBinaryWriter()
            AllBasicTypes_UEBACodec.encode(ctx, w, data)
            val reBytes = w.toByteArray()
            val r = BaboonBinaryReader(reBytes)
            val reDecoded = AllBasicTypes_UEBACodec.decode(ctx, r)
            if (data != reDecoded) {
                System.err.println("UEBA roundtrip mismatch")
                exitProcess(1)
            }
        }
    } catch (e: Exception) {
        System.err.println("Roundtrip failed: ${e.message}")
        exitProcess(1)
    }

    println("OK")
}

private fun createSampleData(): AllBasicTypes {
    return AllBasicTypes(
        vi8 = 42.toByte(),
        vi16 = 1234.toShort(),
        vi32 = 123456,
        vi64 = 123456789L,
        vu8 = 200.toUByte(),
        vu16 = 50000.toUShort(),
        vu32 = 3000000000u.toUInt(),
        vu64 = 10000000000uL.toULong(),
        vf32 = 3.14159f,
        vf64 = 2.718281828,
        vf128 = BaboonDecimal.fromString("123456789.987654321"),
        vstr = "Hello, Baboon!",
        vbstr = ByteString.of(byteArrayOf(0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73)),
        vuid = Uuid.parse("12345678-1234-5678-1234-567812345678"),
        vbit = true,
        vtsu = Instant.parse("2024-06-15T12:30:45.123Z"),
        vtso = BaboonOffsetDateTime.fromEpochMilliseconds(
            Instant.parse("2024-06-15T12:30:45.987Z").toEpochMilliseconds(),
            7200
        ),
        voptStr = "optional value",
        vlstI32 = listOf(1, 2, 3, 4, 5),
        vsetStr = setOf("apple", "banana", "cherry"),
        vmapStrI32 = mapOf("one" to 1, "two" to 2, "three" to 3),
        voptLst = listOf("nested", "list", "values"),
        vlstOpt = listOf(10, null, 20, 30),
        vmapLst = mapOf("numbers" to listOf(1L, 2L, 3L), "more" to listOf(4L, 5L, 6L)),
    )
}

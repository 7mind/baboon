package example

import convtest.testpkg.AllBasicTypes
import convtest.testpkg.AllBasicTypes_JsonCodec
import convtest.testpkg.AllBasicTypes_UEBACodec
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonByteString
import baboon.runtime.shared.LEDataOutputStream
import baboon.runtime.shared.BaboonTimeFormats
import kotlinx.serialization.json.JsonElement
import java.io.ByteArrayOutputStream
import java.io.File
import java.math.BigDecimal
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID

fun main() {
    val sampleData = createSampleData()

    val baseDir = File("../../target/compat-test").absoluteFile.normalize()
    val kotlinJsonDir = File(baseDir, "kotlin-json")
    val kotlinUebaDir = File(baseDir, "kotlin-ueba")

    kotlinJsonDir.mkdirs()
    kotlinUebaDir.mkdirs()

    val ctx = BaboonCodecContext.Default

    val json: JsonElement = AllBasicTypes_JsonCodec.encode(ctx, sampleData)
    val jsonStr = json.toString()
    val jsonFile = File(kotlinJsonDir, "all-basic-types.json")
    jsonFile.writeText(jsonStr, Charsets.UTF_8)
    println("Written JSON to ${jsonFile.absolutePath}")

    val baos = ByteArrayOutputStream()
    val uebaWriter = LEDataOutputStream(baos)
    try {
        AllBasicTypes_UEBACodec.encode(ctx, uebaWriter, sampleData)
        uebaWriter.flush()
        val uebaBytes = baos.toByteArray()
        val uebaFile = File(kotlinUebaDir, "all-basic-types.ueba")
        uebaFile.writeBytes(uebaBytes)
        println("Written UEBA to ${uebaFile.absolutePath}")
    } finally {
        uebaWriter.close()
    }

    println("Kotlin serialization complete!")
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
        vf128 = BigDecimal("123456789.987654321"),
        vstr = "Hello, Baboon!",
        vbstr = BaboonByteString(byteArrayOf(0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73)),
        vuid = UUID.fromString("12345678-1234-5678-1234-567812345678"),
        vbit = true,
        vtsu = OffsetDateTime.of(2024, 6, 15, 12, 30, 45, 123456789, ZoneOffset.UTC),
        vtso = OffsetDateTime.of(2024, 6, 15, 14, 30, 45, 987654321, ZoneOffset.ofHours(2)),
        voptStr = "optional value",
        vlstI32 = listOf(1, 2, 3, 4, 5),
        vsetStr = setOf("apple", "banana", "cherry"),
        vmapStrI32 = mapOf("one" to 1, "two" to 2, "three" to 3),
        voptLst = listOf("nested", "list", "values"),
        vlstOpt = listOf(10, null, 20, 30),
        vmapLst = mapOf("numbers" to listOf(1L, 2L, 3L), "more" to listOf(4L, 5L, 6L)),
    )
}

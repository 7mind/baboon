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
import convtest.testpkg.BaboonCodecsJson
import convtest.testpkg.BaboonCodecsUeba
import convtest.testpkg.WireEnum
import baboon.runtime.shared.AnyMeta
import baboon.runtime.shared.AnyOpaque
import baboon.runtime.shared.AnyOpaqueJson
import baboon.runtime.shared.AnyOpaqueUeba
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.ByteString
import baboon.runtime.shared.LEDataInputStream
import baboon.runtime.shared.LEDataOutputStream
import baboon.runtime.shared.BaboonTimeFormats
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.math.BigDecimal
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID
import kotlin.system.exitProcess

private const val DOMAIN_ID = "convtest.testpkg"
private const val DOMAIN_VER = "2.0.0"
private const val INNER_TYPE_ID = "convtest.testpkg/:#InnerPayload"

fun main(args: Array<String>) {
    when {
        args.size >= 3 && args[0] == "write" -> {
            val outputDir = args[1]
            val format = args[2]
            File(outputDir).mkdirs()
            val sampleData = createSampleData()
            val sampleAny = createSampleAnyShowcase()
            val ctx = BaboonCodecContext.Default
            val facadeCtx = BaboonCodecContext.withFacade(false, freshFacade())
            when (format) {
                "json" -> {
                    writeJson(ctx, sampleData, outputDir)
                    writeJsonAny(facadeCtx, sampleAny, outputDir)
                }
                "ueba" -> {
                    writeUeba(ctx, sampleData, outputDir)
                    writeUebaAny(facadeCtx, sampleAny, outputDir)
                }
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
    val sampleAny = createSampleAnyShowcase()
    val baseDir = File("../../target/compat-test").absoluteFile.normalize()
    val kotlinJsonDir = File(baseDir, "kotlin-json")
    val kotlinUebaDir = File(baseDir, "kotlin-ueba")

    kotlinJsonDir.mkdirs()
    kotlinUebaDir.mkdirs()

    val ctx = BaboonCodecContext.Default
    val facadeCtx = BaboonCodecContext.withFacade(false, freshFacade())
    writeJson(ctx, sampleData, kotlinJsonDir.absolutePath)
    writeUeba(ctx, sampleData, kotlinUebaDir.absolutePath)
    writeJsonAny(facadeCtx, sampleAny, kotlinJsonDir.absolutePath)
    writeUebaAny(facadeCtx, sampleAny, kotlinUebaDir.absolutePath)

    println("Kotlin serialization complete!")
}

private fun freshFacade(): BaboonCodecsFacade {
    val f = BaboonCodecsFacade()
    f.register(
        BaboonDomainVersion(DOMAIN_ID, DOMAIN_VER),
        { BaboonCodecsJson },
        { BaboonCodecsUeba },
    )
    return f
}

private fun expectedInnerPayloads(): List<InnerPayload> = listOf(
    InnerPayload("variant-A", 1),
    InnerPayload("variant-B", 2),
    InnerPayload("variant-C", 3),
    InnerPayload("variant-D1", 4),
    InnerPayload("variant-D2", 5),
    InnerPayload("variant-D3", 6),
    InnerPayload("opt-any", 7),
    InnerPayload("lst-any-0", 8),
)

private fun uebaBytes(p: InnerPayload): ByteArray {
    val baos = ByteArrayOutputStream()
    val out = LEDataOutputStream(baos)
    try {
        InnerPayload_UEBACodec.encode(BaboonCodecContext.Compact, out, p)
        out.flush()
    } finally {
        out.close()
    }
    return baos.toByteArray()
}

private fun asJson(p: InnerPayload): JsonElement =
    InnerPayload_JsonCodec.encode(BaboonCodecContext.Compact, p)

private fun createSampleAnyShowcase(): AnyShowcase {
    val payloads = expectedInnerPayloads()
    val a = payloads[0]; val b = payloads[1]; val c = payloads[2]
    val d1 = payloads[3]; val d2 = payloads[4]; val d3 = payloads[5]
    val optP = payloads[6]; val lstP = payloads[7]

    val metaA  = AnyMeta(0x07.toByte(), DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID)
    val metaB  = AnyMeta(0x03.toByte(), null, DOMAIN_VER, INNER_TYPE_ID)
    val metaC  = AnyMeta(0x01.toByte(), null, null, INNER_TYPE_ID)
    val metaD1 = AnyMeta(0x06.toByte(), DOMAIN_ID, DOMAIN_VER, null)
    val metaD2 = AnyMeta(0x02.toByte(), null, DOMAIN_VER, null)
    val metaD3 = AnyMeta(0x00.toByte(), null, null, null)
    val metaOpt = AnyMeta(0x07.toByte(), DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID)
    val metaLst = AnyMeta(0x06.toByte(), DOMAIN_ID, DOMAIN_VER, null)

    return AnyShowcase(
        vAnyA  = AnyOpaqueJson(metaA,  asJson(a)),
        vAnyB  = AnyOpaqueJson(metaB,  asJson(b)),
        vAnyC  = AnyOpaqueJson(metaC,  asJson(c)),
        vAnyD1 = AnyOpaqueUeba(metaD1, uebaBytes(d1)),
        vAnyD2 = AnyOpaqueUeba(metaD2, uebaBytes(d2)),
        vAnyD3 = AnyOpaqueUeba(metaD3, uebaBytes(d3)),
        optAny = AnyOpaqueJson(metaOpt, asJson(optP)),
        lstAny = listOf(AnyOpaqueUeba(metaLst, uebaBytes(lstP))),
    )
}

private fun writeJsonAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: String) {
    val json: JsonElement = AnyShowcase_JsonCodec.encode(ctx, data)
    val jsonStr = json.toString()
    val path = File(outputDir, "any-showcase.json")
    path.writeText(jsonStr, Charsets.UTF_8)
    println("Written JSON to ${path.absolutePath}")
}

private fun writeUebaAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: String) {
    val baos = ByteArrayOutputStream()
    val out = LEDataOutputStream(baos)
    try {
        AnyShowcase_UEBACodec.encode(ctx, out, data)
        out.flush()
        val bytes = baos.toByteArray()
        val path = File(outputDir, "any-showcase.ueba")
        path.writeBytes(bytes)
        println("Written UEBA to ${path.absolutePath}")
    } finally {
        out.close()
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
    val opt = v.optAny ?: error("optAny was null; expected non-null")
    val lst0 = v.lstAny.firstOrNull() ?: error("lstAny was empty; expected one element")
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

private fun readAndVerifyAnyShowcase(filePath: String) {
    val ctx = BaboonCodecContext.Default
    val file = File(filePath)
    val data: AnyShowcase = try {
        if (filePath.endsWith(".json")) {
            val jsonStr = file.readText(Charsets.UTF_8)
            val jsonElement = Json.parseToJsonElement(jsonStr)
            AnyShowcase_JsonCodec.decode(ctx, jsonElement)
        } else {
            val bytes = file.readBytes()
            val r = LEDataInputStream(ByteArrayInputStream(bytes))
            try {
                AnyShowcase_UEBACodec.decode(ctx, r)
            } finally {
                r.close()
            }
        }
    } catch (e: Exception) {
        System.err.println("AnyShowcase deserialization failed: ${e.message}")
        exitProcess(1)
        return
    }

    try {
        val expected = expectedInnerPayloads()
        val decoded = decodeAllPayloads(data)
        for (i in expected.indices) {
            if (expected[i] != decoded[i]) {
                System.err.println("AnyShowcase payload $i mismatch: expected ${expected[i]}, got ${decoded[i]}")
                exitProcess(1)
            }
        }
    } catch (e: Exception) {
        System.err.println("AnyShowcase decode failed: ${e.message}")
        exitProcess(1)
    }
    println("OK")
}

private fun writeJson(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String) {
    val json: JsonElement = AllBasicTypes_JsonCodec.encode(ctx, data)
    val jsonStr = json.toString()
    val jsonFile = File(outputDir, "all-basic-types.json")
    jsonFile.writeText(jsonStr, Charsets.UTF_8)
    println("Written JSON to ${jsonFile.absolutePath}")
}

private fun writeUeba(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String) {
    val baos = ByteArrayOutputStream()
    val uebaWriter = LEDataOutputStream(baos)
    try {
        AllBasicTypes_UEBACodec.encode(ctx, uebaWriter, data)
        uebaWriter.flush()
        val uebaBytes = baos.toByteArray()
        val uebaFile = File(outputDir, "all-basic-types.ueba")
        uebaFile.writeBytes(uebaBytes)
        println("Written UEBA to ${uebaFile.absolutePath}")
    } finally {
        uebaWriter.close()
    }
}

private fun readAndVerify(filePath: String) {
    if (filePath.endsWith("any-showcase.json") || filePath.endsWith("any-showcase.ueba")) {
        readAndVerifyAnyShowcase(filePath)
        return
    }
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
            val bais = ByteArrayInputStream(bytes)
            val uebaReader = LEDataInputStream(bais)
            try {
                data = AllBasicTypes_UEBACodec.decode(ctx, uebaReader)
            } finally {
                uebaReader.close()
            }
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
            val baos = ByteArrayOutputStream()
            val w = LEDataOutputStream(baos)
            try {
                AllBasicTypes_UEBACodec.encode(ctx, w, data)
                w.flush()
            } finally {
                w.close()
            }
            val reBytes = baos.toByteArray()
            val bais = ByteArrayInputStream(reBytes)
            val r = LEDataInputStream(bais)
            try {
                val reDecoded = AllBasicTypes_UEBACodec.decode(ctx, r)
                if (data != reDecoded) {
                    System.err.println("UEBA roundtrip mismatch")
                    exitProcess(1)
                }
            } finally {
                r.close()
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
        vf128 = BigDecimal("123456789.987654321"),
        vstr = "Hello, Baboon!",
        vbstr = ByteString.of(byteArrayOf(0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73)),
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
        // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
        vWireEnum = WireEnum.Cafe,
    )
}

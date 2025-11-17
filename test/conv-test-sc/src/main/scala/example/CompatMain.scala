package example

import convtest.testpkg.{AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec}
import baboon.runtime.shared.{BaboonCodecContext, LEDataOutputStream, LEDataInputStream, ByteString}
import java.io.{File, FileOutputStream, FileInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Paths}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.collection.immutable.{List, Map, Set}

object CompatMain {
  def main(args: Array[String]): Unit = {
    // Create sample data with all basic types
    val sampleData = createSampleData()

    // Create output directories - use absolute path relative to project root
    val baseDir = Paths.get("../../target/compat-test").toAbsolutePath.normalize()
    val scalaJsonDir = baseDir.resolve("scala-json")
    val scalaUebaDir = baseDir.resolve("scala-ueba")

    Files.createDirectories(scalaJsonDir)
    Files.createDirectories(scalaUebaDir)

    // Serialize to JSON
    val ctx = baboon.runtime.shared.BaboonCodecContext.Default
    val json = AllBasicTypes_JsonCodec.instance.encode(ctx, sampleData)
    val jsonStr = json.spaces2
    Files.write(scalaJsonDir.resolve("all-basic-types.json"), jsonStr.getBytes("UTF-8"))
    println(s"Written JSON to ${scalaJsonDir.resolve("all-basic-types.json")}")

    // Serialize to UEBA
    val baos = new ByteArrayOutputStream()
    val uebaWriter = new LEDataOutputStream(baos)
    try {
      AllBasicTypes_UEBACodec.instance.encode(ctx, uebaWriter, sampleData)
      uebaWriter.flush()
      val uebaBytes = baos.toByteArray
      Files.write(scalaUebaDir.resolve("all-basic-types.ueba"), uebaBytes)
      println(s"Written UEBA to ${scalaUebaDir.resolve("all-basic-types.ueba")}")
    } finally {
      uebaWriter.close()
    }

    println("Scala serialization complete!")
  }

  private def createSampleData(): AllBasicTypes = {
    AllBasicTypes(
      vi8 = 42.toByte,
      vi16 = 1234.toShort,
      vi32 = 123456,
      vi64 = 123456789L,

      vu8 = 200.toByte,
      vu16 = 50000.toShort,
      vu32 = 3000000000L.toInt,
      vu64 = 10000000000L,

      vf32 = 3.14159f,
      vf64 = 2.718281828,
      vf128 = BigDecimal("123456789.987654321"),

      vstr = "Hello, Baboon!",
      vbstr = ByteString(Array[Byte](0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73)), // "Hello Bytes"
      vuid = UUID.fromString("12345678-1234-5678-1234-567812345678"),

      vbit = true,

      vtsu = OffsetDateTime.of(2024, 6, 15, 12, 30, 45, 123456789, ZoneOffset.UTC),
      vtso = OffsetDateTime.of(2024, 6, 15, 14, 30, 45, 987654321, ZoneOffset.ofHours(2)),

      voptStr = Some("optional value"),
      vlstI32 = List(1, 2, 3, 4, 5),
      vsetStr = Set("apple", "banana", "cherry"),
      vmapStrI32 = Map("one" -> 1, "two" -> 2, "three" -> 3),

      voptLst = Some(List("nested", "list", "values")),
      vlstOpt = List(Some(10), None, Some(20), Some(30)),
      vmapLst = Map("numbers" -> List(1L, 2L, 3L), "more" -> List(4L, 5L, 6L))
    )
  }
}

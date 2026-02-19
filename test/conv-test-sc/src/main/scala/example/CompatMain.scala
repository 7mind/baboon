package example

import convtest.testpkg.{AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec}
import baboon.runtime.shared.{BaboonCodecContext, ByteString, LEDataInputStream, LEDataOutputStream}
import io.circe.parser.parse
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Paths}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.collection.immutable.{List, Map, Set}

object CompatMain {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case "write" :: outputDir :: format :: Nil =>
        val dir = Paths.get(outputDir).toAbsolutePath.normalize()
        Files.createDirectories(dir)
        val sampleData = createSampleData()
        val ctx        = BaboonCodecContext.Default
        format match {
          case "json" => writeJson(ctx, sampleData, dir.toString)
          case "ueba" => writeUeba(ctx, sampleData, dir.toString)
          case _ =>
            System.err.println(s"Unknown format: $format")
            sys.exit(1)
        }

      case "read" :: filePath :: Nil =>
        readAndVerify(filePath)

      case _ =>
        runLegacy()
    }
  }

  private def runLegacy(): Unit = {
    val sampleData = createSampleData()
    val baseDir    = Paths.get("../../target/compat-test").toAbsolutePath.normalize()

    val scalaJsonDir = baseDir.resolve("scala-json")
    val scalaUebaDir = baseDir.resolve("scala-ueba")
    Files.createDirectories(scalaJsonDir)
    Files.createDirectories(scalaUebaDir)

    val ctx = BaboonCodecContext.Default
    writeJson(ctx, sampleData, scalaJsonDir.toString)
    writeUeba(ctx, sampleData, scalaUebaDir.toString)

    println("Scala serialization complete!")
  }

  private def writeJson(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String): Unit = {
    val json    = AllBasicTypes_JsonCodec.instance.encode(ctx, data)
    val jsonStr = json.spaces2
    val path    = Paths.get(outputDir).resolve("all-basic-types.json")
    Files.write(path, jsonStr.getBytes("UTF-8"))
    println(s"Written JSON to $path")
  }

  private def writeUeba(ctx: BaboonCodecContext, data: AllBasicTypes, outputDir: String): Unit = {
    val baos       = new ByteArrayOutputStream()
    val uebaWriter = new LEDataOutputStream(baos)
    try {
      AllBasicTypes_UEBACodec.instance.encode(ctx, uebaWriter, data)
      uebaWriter.flush()
      val uebaBytes = baos.toByteArray
      val path      = Paths.get(outputDir).resolve("all-basic-types.ueba")
      Files.write(path, uebaBytes)
      println(s"Written UEBA to $path")
    } finally {
      uebaWriter.close()
    }
  }

  private def readAndVerify(filePath: String): Unit = {
    val ctx  = BaboonCodecContext.Default
    val path = Paths.get(filePath)
    val data: AllBasicTypes = try {
      if (filePath.endsWith(".json")) {
        val jsonStr = new String(Files.readAllBytes(path), "UTF-8")
        val json    = parse(jsonStr).getOrElse(throw new RuntimeException(s"Failed to parse JSON from $filePath"))
        AllBasicTypes_JsonCodec.instance.decode(ctx, json) match {
          case Right(d)  => d
          case Left(err) => throw new RuntimeException(s"JSON decode failed: $err")
        }
      } else if (filePath.endsWith(".ueba")) {
        val bytes      = Files.readAllBytes(path)
        val bais       = new ByteArrayInputStream(bytes)
        val uebaReader = new LEDataInputStream(bais)
        try {
          AllBasicTypes_UEBACodec.instance.decode(ctx, uebaReader) match {
            case Right(d)  => d
            case Left(err) => throw new RuntimeException(s"UEBA decode failed: $err")
          }
        } finally {
          uebaReader.close()
        }
      } else {
        System.err.println(s"Unknown file extension: $filePath")
        sys.exit(1)
        throw new RuntimeException("unreachable")
      }
    } catch {
      case e: Exception =>
        System.err.println(s"Deserialization failed: ${e.getMessage}")
        sys.exit(1)
        throw new RuntimeException("unreachable")
    }

    if (data.vstr != "Hello, Baboon!") {
      System.err.println(s"vstr mismatch: expected 'Hello, Baboon!', got '${data.vstr}'")
      sys.exit(1)
    }
    if (data.vi32 != 123456) {
      System.err.println(s"vi32 mismatch: expected 123456, got ${data.vi32}")
      sys.exit(1)
    }
    if (!data.vbit) {
      System.err.println(s"vbit mismatch: expected true, got ${data.vbit}")
      sys.exit(1)
    }

    // Roundtrip
    try {
      if (filePath.endsWith(".json")) {
        val reEncoded = AllBasicTypes_JsonCodec.instance.encode(ctx, data)
        AllBasicTypes_JsonCodec.instance.decode(ctx, reEncoded) match {
          case Right(reDecoded) =>
            if (data != reDecoded) {
              System.err.println("JSON roundtrip mismatch")
              sys.exit(1)
            }
          case Left(err) =>
            System.err.println(s"JSON roundtrip decode failed: $err")
            sys.exit(1)
        }
      } else {
        val baos = new ByteArrayOutputStream()
        val w    = new LEDataOutputStream(baos)
        try {
          AllBasicTypes_UEBACodec.instance.encode(ctx, w, data)
          w.flush()
        } finally {
          w.close()
        }
        val reBytes = baos.toByteArray
        val bais    = new ByteArrayInputStream(reBytes)
        val r       = new LEDataInputStream(bais)
        try {
          AllBasicTypes_UEBACodec.instance.decode(ctx, r) match {
            case Right(reDecoded) =>
              if (data != reDecoded) {
                System.err.println("UEBA roundtrip mismatch")
                sys.exit(1)
              }
            case Left(err) =>
              System.err.println(s"UEBA roundtrip decode failed: $err")
              sys.exit(1)
          }
        } finally {
          r.close()
        }
      }
    } catch {
      case e: Exception =>
        System.err.println(s"Roundtrip failed: ${e.getMessage}")
        sys.exit(1)
    }

    println("OK")
  }

  private def createSampleData(): AllBasicTypes = {
    AllBasicTypes(
      vi8        = 42.toByte,
      vi16       = 1234.toShort,
      vi32       = 123456,
      vi64       = 123456789L,
      vu8        = 200.toByte,
      vu16       = 50000.toShort,
      vu32       = 3000000000L.toInt,
      vu64       = 10000000000L,
      vf32       = 3.14159f,
      vf64       = 2.718281828,
      vf128      = BigDecimal("123456789.987654321"),
      vstr       = "Hello, Baboon!",
      vbstr      = ByteString(Array[Byte](0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73)),
      vuid       = UUID.fromString("12345678-1234-5678-1234-567812345678"),
      vbit       = true,
      vtsu       = OffsetDateTime.of(2024, 6, 15, 12, 30, 45, 123456789, ZoneOffset.UTC),
      vtso       = OffsetDateTime.of(2024, 6, 15, 14, 30, 45, 987654321, ZoneOffset.ofHours(2)),
      voptStr    = Some("optional value"),
      vlstI32    = List(1, 2, 3, 4, 5),
      vsetStr    = Set("apple", "banana", "cherry"),
      vmapStrI32 = Map("one" -> 1, "two" -> 2, "three" -> 3),
      voptLst    = Some(List("nested", "list", "values")),
      vlstOpt    = List(Some(10), None, Some(20), Some(30)),
      vmapLst    = Map("numbers" -> List(1L, 2L, 3L), "more" -> List(4L, 5L, 6L)),
    )
  }
}

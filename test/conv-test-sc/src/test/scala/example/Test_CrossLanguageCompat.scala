package example

import convtest.testpkg.{AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec}
import baboon.runtime.shared.LEDataInputStream
import org.scalatest.flatspec.AnyFlatSpec
import java.io.{File, FileInputStream}
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import io.circe.parser.parse

class Test_CrossLanguageCompat extends AnyFlatSpec {

  val baseDir = Paths.get("../../target/compat-test").toAbsolutePath.normalize()
  val scalaJsonFile = baseDir.resolve("scala-json/all-basic-types.json")
  val scalaUebaFile = baseDir.resolve("scala-ueba/all-basic-types.ueba")
  val csJsonFile = baseDir.resolve("cs-json/all-basic-types.json")
  val csUebaFile = baseDir.resolve("cs-ueba/all-basic-types.ueba")

  "Scala JSON deserialization" should "read Scala-generated JSON" in {
    val jsonStr = new String(Files.readAllBytes(scalaJsonFile), StandardCharsets.UTF_8)
    val json = parse(jsonStr).getOrElse(fail(s"Failed to parse JSON from $scalaJsonFile"))

    val ctx = baboon.runtime.shared.BaboonCodecContext.Default
    val decoded = AllBasicTypes_JsonCodec.instance.decode(ctx, json)

    decoded match {
      case Right(data) =>
        println(s"Successfully decoded Scala JSON: ${data.vstr}")
        assert(data.vstr == "Hello, Baboon!")
        assert(data.vi32 == 123456)
        assert(data.vbit)
      case Left(error) =>
        fail(s"Failed to decode Scala JSON: $error")
    }
  }

  "Scala JSON deserialization" should "read C#-generated JSON" in {
    val jsonStr = new String(Files.readAllBytes(csJsonFile), StandardCharsets.UTF_8)
    val json = parse(jsonStr).getOrElse(fail(s"Failed to parse JSON from $csJsonFile"))

    val ctx = baboon.runtime.shared.BaboonCodecContext.Default
    val decoded = AllBasicTypes_JsonCodec.instance.decode(ctx, json)

    decoded match {
      case Right(data) =>
        println(s"Successfully decoded C# JSON: ${data.vstr}")
        assert(data.vstr == "Hello, Baboon!")
        assert(data.vi32 == 123456)
        assert(data.vbit)
      case Left(error) =>
        fail(s"Failed to decode C# JSON: $error")
    }
  }

  "Scala UEBA deserialization" should "read Scala-generated UEBA" in {
    val uebaBytes = Files.readAllBytes(scalaUebaFile)
    val fis = new FileInputStream(scalaUebaFile.toFile)
    try {
      val reader = new LEDataInputStream(fis)
      val ctx = baboon.runtime.shared.BaboonCodecContext.Default
      val decoded = AllBasicTypes_UEBACodec.instance.decode(ctx, reader)

      println(s"Successfully decoded Scala UEBA: ${decoded.vstr}")
      assert(decoded.vstr == "Hello, Baboon!")
      assert(decoded.vi32 == 123456)
      assert(decoded.vbit)
    } finally {
      fis.close()
    }
  }

  "Scala UEBA deserialization" should "read C#-generated UEBA" in {
    val uebaBytes = Files.readAllBytes(csUebaFile)
    val fis = new FileInputStream(csUebaFile.toFile)
    try {
      val reader = new LEDataInputStream(fis)
      val ctx = baboon.runtime.shared.BaboonCodecContext.Default
      val decoded = AllBasicTypes_UEBACodec.instance.decode(ctx, reader)

      println(s"Successfully decoded C# UEBA: ${decoded.vstr}")
      assert(decoded.vstr == "Hello, Baboon!")
      assert(decoded.vi32 == 123456)
      assert(decoded.vbit)
    } finally {
      fis.close()
    }
  }

  "Cross-language comparison" should "verify Scala and C# JSON produce equivalent data" in {
    val scalaJsonStr = new String(Files.readAllBytes(scalaJsonFile), StandardCharsets.UTF_8)
    val scalaJson = parse(scalaJsonStr).getOrElse(fail("Failed to parse Scala JSON"))

    val csJsonStr = new String(Files.readAllBytes(csJsonFile), StandardCharsets.UTF_8)
    val csJson = parse(csJsonStr).getOrElse(fail("Failed to parse C# JSON"))

    val ctx = baboon.runtime.shared.BaboonCodecContext.Default
    val scalaDecoded = AllBasicTypes_JsonCodec.instance.decode(ctx, scalaJson).getOrElse(fail("Failed to decode Scala JSON"))
    val csDecoded = AllBasicTypes_JsonCodec.instance.decode(ctx, csJson).getOrElse(fail("Failed to decode C# JSON"))

    println(s"Comparing Scala and C# JSON data:")
    println(s"  Scala: vi8=${scalaDecoded.vi8}, vi16=${scalaDecoded.vi16}, vi32=${scalaDecoded.vi32}, vi64=${scalaDecoded.vi64}")
    println(s"  C#:    vi8=${csDecoded.vi8}, vi16=${csDecoded.vi16}, vi32=${csDecoded.vi32}, vi64=${csDecoded.vi64}")
    println(s"  Scala: vf32=${scalaDecoded.vf32}, vf64=${scalaDecoded.vf64}, vf128=${scalaDecoded.vf128}")
    println(s"  C#:    vf32=${csDecoded.vf32}, vf64=${csDecoded.vf64}, vf128=${csDecoded.vf128}")
    println(s"  Scala: vtsu=${scalaDecoded.vtsu}, vtso=${scalaDecoded.vtso}")
    println(s"  C#:    vtsu=${csDecoded.vtsu}, vtso=${csDecoded.vtso}")

    assert(scalaDecoded == csDecoded, "Scala and C# JSON data should be equal")
  }

  "Cross-language comparison" should "verify Scala and C# UEBA produce equivalent data" in {
    val scalaFis = new FileInputStream(scalaUebaFile.toFile)
    val scalaDecoded = try {
      val reader = new LEDataInputStream(scalaFis)
      val ctx = baboon.runtime.shared.BaboonCodecContext.Default
      AllBasicTypes_UEBACodec.instance.decode(ctx, reader)
    } finally {
      scalaFis.close()
    }

    val csFis = new FileInputStream(csUebaFile.toFile)
    val csDecoded = try {
      val reader = new LEDataInputStream(csFis)
      val ctx = baboon.runtime.shared.BaboonCodecContext.Default
      AllBasicTypes_UEBACodec.instance.decode(ctx, reader)
    } finally {
      csFis.close()
    }

    println(s"Comparing Scala and C# UEBA data:")
    println(s"  Scala: vi8=${scalaDecoded.vi8}, vi16=${scalaDecoded.vi16}, vi32=${scalaDecoded.vi32}, vi64=${scalaDecoded.vi64}")
    println(s"  C#:    vi8=${csDecoded.vi8}, vi16=${csDecoded.vi16}, vi32=${csDecoded.vi32}, vi64=${csDecoded.vi64}")
    println(s"  Scala: vf32=${scalaDecoded.vf32}, vf64=${scalaDecoded.vf64}, vf128=${scalaDecoded.vf128}")
    println(s"  C#:    vf32=${csDecoded.vf32}, vf64=${csDecoded.vf64}, vf128=${csDecoded.vf128}")
    println(s"  Scala: vtsu=${scalaDecoded.vtsu}, vtso=${scalaDecoded.vtso}")
    println(s"  C#:    vtsu=${csDecoded.vtsu}, vtso=${csDecoded.vtso}")

    assert(scalaDecoded == csDecoded, "Scala and C# UEBA data should be equal")
  }
}

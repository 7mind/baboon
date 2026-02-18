package example

import convtest.testpkg.{AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec}
import baboon.runtime.shared.{BaboonCodecContext, LEDataInputStream}
import org.scalatest.flatspec.AnyFlatSpec
import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import io.circe.parser.parse

class Test_CrossLanguageCompat extends AnyFlatSpec {

  private val baseDir = Paths.get("../../target/compat-test").toAbsolutePath.normalize()
  private val ctx     = BaboonCodecContext.Default

  // Helper methods
  private def readJsonFile(source: String, format: String): AllBasicTypes = {
    val file    = baseDir.resolve(s"$source-json/all-basic-types.json")
    val jsonStr = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val json    = parse(jsonStr).getOrElse(fail(s"Failed to parse $format JSON from $file"))
    AllBasicTypes_JsonCodec.instance.decode(ctx, json) match {
      case Right(data) => data
      case Left(error) => fail(s"Failed to decode $format JSON: $error")
    }
  }

  private def readUebaFile(source: String, format: String): AllBasicTypes = {
    val file = baseDir.resolve(s"$source-ueba/all-basic-types.ueba")
    val fis  = new FileInputStream(file.toFile)
    try {
      val reader = new LEDataInputStream(fis)
      AllBasicTypes_UEBACodec.instance.decode(ctx, reader) match {
        case Right(data) => data
        case Left(error) => fail(s"Failed to decode $format UEBA: $error")
      }
    } finally {
      fis.close()
    }
  }

  private def assertBasicFields(data: AllBasicTypes, label: String): Unit = {
    println(s"Successfully decoded $label: ${data.vstr}")
    assert(data.vstr == "Hello, Baboon!")
    assert(data.vi32 == 123456)
    assert(data.vbit)
  }

  private def printComparison(label: String, scala: AllBasicTypes, cs: AllBasicTypes): Unit = {
    println(s"Comparing Scala and C# $label data:")
    println(s"  Scala: vi8=${scala.vi8}, vi16=${scala.vi16}, vi32=${scala.vi32}, vi64=${scala.vi64}")
    println(s"  C#:    vi8=${cs.vi8}, vi16=${cs.vi16}, vi32=${cs.vi32}, vi64=${cs.vi64}")
    println(s"  Scala: vf32=${scala.vf32}, vf64=${scala.vf64}, vf128=${scala.vf128}")
    println(s"  C#:    vf32=${cs.vf32}, vf64=${cs.vf64}, vf128=${cs.vf128}")
    println(s"  Scala: vtsu=${scala.vtsu}, vtso=${scala.vtso}")
    println(s"  C#:    vtsu=${cs.vtsu}, vtso=${cs.vtso}")
  }

  // JSON Tests
  "Scala JSON deserialization" should "read Scala-generated JSON" in {
    assertBasicFields(readJsonFile("scala", "Scala JSON"), "Scala JSON")
  }

  it should "read C#-generated JSON" in {
    assertBasicFields(readJsonFile("cs", "C# JSON"), "C# JSON")
  }

  it should "read Rust-generated JSON" in {
    assertBasicFields(readJsonFile("rust", "Rust JSON"), "Rust JSON")
  }

  // UEBA Tests
  "Scala UEBA deserialization" should "read Scala-generated UEBA" in {
    assertBasicFields(readUebaFile("scala", "Scala UEBA"), "Scala UEBA")
  }

  it should "read C#-generated UEBA" in {
    assertBasicFields(readUebaFile("cs", "C# UEBA"), "C# UEBA")
  }

  it should "read Rust-generated UEBA" in {
    assertBasicFields(readUebaFile("rust", "Rust UEBA"), "Rust UEBA")
  }

  // Cross-language comparison
  "Cross-language comparison" should "verify Scala and C# JSON produce equivalent data" in {
    val scalaData = readJsonFile("scala", "Scala JSON")
    val csData    = readJsonFile("cs", "C# JSON")
    printComparison("JSON", scalaData, csData)
    assert(scalaData == csData, "Scala and C# JSON data should be equal")
  }

  it should "verify Scala and C# UEBA produce equivalent data" in {
    val scalaData = readUebaFile("scala", "Scala UEBA")
    val csData    = readUebaFile("cs", "C# UEBA")
    printComparison("UEBA", scalaData, csData)
    assert(scalaData == csData, "Scala and C# UEBA data should be equal")
  }

  it should "verify Scala and Rust JSON produce equivalent data" in {
    val scalaData = readJsonFile("scala", "Scala JSON")
    val rustData  = readJsonFile("rust", "Rust JSON")
    assert(scalaData == rustData, "Scala and Rust JSON data should be equal")
  }

  it should "verify Scala and Rust UEBA produce equivalent data" in {
    val scalaData = readUebaFile("scala", "Scala UEBA")
    val rustData  = readUebaFile("rust", "Rust UEBA")
    assert(scalaData == rustData, "Scala and Rust UEBA data should be equal")
  }

  // Python, TypeScript, Kotlin, Java, Dart
  it should "read Python-generated JSON" in {
    assertBasicFields(readJsonFile("python", "Python JSON"), "Python JSON")
  }

  it should "read TypeScript-generated JSON" in {
    assertBasicFields(readJsonFile("typescript", "TypeScript JSON"), "TypeScript JSON")
  }

  it should "read Kotlin-generated JSON" in {
    assertBasicFields(readJsonFile("kotlin", "Kotlin JSON"), "Kotlin JSON")
  }

  it should "read Java-generated JSON" in {
    assertBasicFields(readJsonFile("java", "Java JSON"), "Java JSON")
  }

  it should "read Dart-generated JSON" in {
    assertBasicFields(readJsonFile("dart", "Dart JSON"), "Dart JSON")
  }

  "Scala UEBA deserialization" should "read Python-generated UEBA" in {
    assertBasicFields(readUebaFile("python", "Python UEBA"), "Python UEBA")
  }

  it should "read TypeScript-generated UEBA" in {
    assertBasicFields(readUebaFile("typescript", "TypeScript UEBA"), "TypeScript UEBA")
  }

  it should "read Kotlin-generated UEBA" in {
    assertBasicFields(readUebaFile("kotlin", "Kotlin UEBA"), "Kotlin UEBA")
  }

  it should "read Java-generated UEBA" in {
    assertBasicFields(readUebaFile("java", "Java UEBA"), "Java UEBA")
  }

  it should "read Dart-generated UEBA" in {
    assertBasicFields(readUebaFile("dart", "Dart UEBA"), "Dart UEBA")
  }
}

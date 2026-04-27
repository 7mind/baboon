package example

import convtest.testpkg.{
  AllBasicTypes,
  AllBasicTypes_JsonCodec,
  AllBasicTypes_UEBACodec,
  AnyShowcase,
  AnyShowcase_JsonCodec,
  AnyShowcase_UEBACodec,
  InnerPayload,
  InnerPayload_JsonCodec,
  InnerPayload_UEBACodec,
}
import baboon.runtime.shared.{AnyOpaque, AnyOpaqueJson, AnyOpaqueUeba, BaboonCodecContext, LEDataInputStream}
import org.scalatest.flatspec.AnyFlatSpec
import java.io.{ByteArrayInputStream, FileInputStream}
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

  it should "read Swift-generated JSON" in {
    val file = baseDir.resolve("swift-json/all-basic-types.json")
    assume(Files.exists(file), "Swift JSON file not found, skipping")
    assertBasicFields(readJsonFile("swift", "Swift JSON"), "Swift JSON")
  }

  it should "read Swift-generated UEBA" in {
    val file = baseDir.resolve("swift-ueba/all-basic-types.ueba")
    assume(Files.exists(file), "Swift UEBA file not found, skipping")
    assertBasicFields(readUebaFile("swift", "Swift UEBA"), "Swift UEBA")
  }

  // ---------------------------------------------------------------------------------------------
  // AnyShowcase cross-language tests (M13 / PR 13.1) — Scala + C# baseline.
  //
  // Both languages serialize an `AnyShowcase` DTO with one slot per `any` variant (A/B/C/D1/D2/D3
  // + opt + lst). Each slot holds an `InnerPayload(label, count)` whose label/count is unique per
  // slot. Reading either language's fixture and decoding the inner payloads must yield the same
  // sequence of InnerPayload values.
  //
  // PR 13.2 will fan out to the remaining 7 languages (Rust/Kotlin/KMP/Java/TS/Dart/Swift/Python).
  // ---------------------------------------------------------------------------------------------

  private val expectedAnyPayloads: Seq[InnerPayload] = Seq(
    InnerPayload("variant-A", 1),
    InnerPayload("variant-B", 2),
    InnerPayload("variant-C", 3),
    InnerPayload("variant-D1", 4),
    InnerPayload("variant-D2", 5),
    InnerPayload("variant-D3", 6),
    InnerPayload("opt-any", 7),
    InnerPayload("lst-any-0", 8),
  )

  private def readAnyShowcaseJson(source: String): AnyShowcase = {
    val file    = baseDir.resolve(s"$source-json/any-showcase.json")
    val jsonStr = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val json    = parse(jsonStr).getOrElse(fail(s"Failed to parse $source any-showcase JSON from $file"))
    AnyShowcase_JsonCodec.instance.decode(ctx, json) match {
      case Right(d)  => d
      case Left(err) => fail(s"Failed to decode $source any-showcase JSON: $err")
    }
  }

  private def readAnyShowcaseUeba(source: String): AnyShowcase = {
    val file = baseDir.resolve(s"$source-ueba/any-showcase.ueba")
    val fis  = new FileInputStream(file.toFile)
    try {
      val r = new LEDataInputStream(fis)
      AnyShowcase_UEBACodec.instance.decode(ctx, r) match {
        case Right(d)  => d
        case Left(err) => fail(s"Failed to decode $source any-showcase UEBA: $err")
      }
    } finally {
      fis.close()
    }
  }

  // Decode an AnyOpaque whose payload is known to be InnerPayload. This bypasses
  // facade.decodeAny — the partial-meta variants (B/C/D1/D2/D3) lack the full (domain, version,
  // typeid) triple required for facade resolution; their statics live in the codec generator only.
  // For cross-language interop the pertinent property is that the inner-payload bytes/json are
  // recoverable from the AnyOpaque content slot, which is what the InnerPayload_*Codec verifies.
  private def decodeInner(o: AnyOpaque): InnerPayload = o match {
    case AnyOpaqueUeba(_, bytes) =>
      val r = new LEDataInputStream(new ByteArrayInputStream(bytes))
      try {
        InnerPayload_UEBACodec.instance.decode(BaboonCodecContext.Compact, r) match {
          case Right(p)  => p
          case Left(err) => fail(s"Inner UEBA decode failed: ${err.getMessage}")
        }
      } finally {
        r.close()
      }
    case AnyOpaqueJson(_, json) =>
      InnerPayload_JsonCodec.instance.decode(BaboonCodecContext.Compact, json) match {
        case Right(p)  => p
        case Left(err) => fail(s"Inner JSON decode failed: ${err.getMessage}")
      }
  }

  private def decodeAllPayloads(v: AnyShowcase): Seq[InnerPayload] = {
    val slots: Seq[AnyOpaque] = Seq(
      v.vAnyA,
      v.vAnyB,
      v.vAnyC,
      v.vAnyD1,
      v.vAnyD2,
      v.vAnyD3,
      v.optAny.getOrElse(fail("optAny was None; expected Some")),
      v.lstAny.headOption.getOrElse(fail("lstAny was empty; expected one element")),
    )
    slots.map(decodeInner)
  }

  "AnyShowcase JSON" should "decode Scala-emitted fixture into the expected payloads" in {
    val decoded = decodeAllPayloads(readAnyShowcaseJson("scala"))
    assert(decoded == expectedAnyPayloads)
  }

  it should "decode C#-emitted fixture into the expected payloads" in {
    val decoded = decodeAllPayloads(readAnyShowcaseJson("cs"))
    assert(decoded == expectedAnyPayloads)
  }

  it should "produce the same payloads from Scala and C# JSON fixtures" in {
    val scala = decodeAllPayloads(readAnyShowcaseJson("scala"))
    val cs    = decodeAllPayloads(readAnyShowcaseJson("cs"))
    assert(scala == cs)
  }

  "AnyShowcase UEBA" should "decode Scala-emitted fixture into the expected payloads" in {
    val decoded = decodeAllPayloads(readAnyShowcaseUeba("scala"))
    assert(decoded == expectedAnyPayloads)
  }

  it should "decode C#-emitted fixture into the expected payloads" in {
    val decoded = decodeAllPayloads(readAnyShowcaseUeba("cs"))
    assert(decoded == expectedAnyPayloads)
  }

  it should "produce the same payloads from Scala and C# UEBA fixtures" in {
    val scala = decodeAllPayloads(readAnyShowcaseUeba("scala"))
    val cs    = decodeAllPayloads(readAnyShowcaseUeba("cs"))
    assert(scala == cs)
  }

  it should "produce byte-identical Scala and C# UEBA fixtures" in {
    val scalaBytes = Files.readAllBytes(baseDir.resolve("scala-ueba/any-showcase.ueba"))
    val csBytes    = Files.readAllBytes(baseDir.resolve("cs-ueba/any-showcase.ueba"))
    assert(java.util.Arrays.equals(scalaBytes, csBytes), "Scala and C# UEBA bytes diverged")
  }
}

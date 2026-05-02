package example

import convtest.testpkg.{
  AllBasicTypes,
  AllBasicTypes_JsonCodec,
  AllBasicTypes_UEBACodec,
  AnyShowcase,
  AnyShowcase_JsonCodec,
  AnyShowcase_UEBACodec,
  BaboonCodecsJson,
  BaboonCodecsUeba,
  CompositeId,
  InnerPayload,
  InnerPayload_JsonCodec,
  InnerPayload_UEBACodec,
  ItemId,
  PointId,
  WireEnum,
}
// PR-I.1a (M24 Phase 3.1) — Custom-foreign KeyCodec hook fixture. Stringy
// foreign FStr maps to java.lang.String; the default identity FStr_KeyCodec
// impl handles encode/decode of map keys without host registration.
import convtest.m24foreign.{ForeignKeyHolder, ForeignKeyHolder_JsonCodec, ItemKey}
// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
// Closes PR-G-D01. Locks parity for the 6 non-string builtin map-key types
// already covered structurally by PR-G's TS unification.
import convtest.m26builtinkeys.{BuiltinMapKeyHolder, BuiltinMapKeyHolder_JsonCodec, BuiltinMapKeyHolder_UEBACodec}
import baboon.runtime.shared.{
  AnyMeta,
  AnyOpaque,
  AnyOpaqueJson,
  AnyOpaqueUeba,
  BaboonCodecContext,
  BaboonCodecsFacade,
  BaboonDomainVersion,
  ByteString,
  LEDataInputStream,
  LEDataOutputStream,
}
import io.circe.Json
import io.circe.parser.parse
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Paths}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.collection.immutable.{List, Map, Set}

object CompatMain {
  // Domain constants — match the convtest.testpkg domain at version 2.0.0 (where AnyShowcase + InnerPayload live).
  private val DomainId    = "convtest.testpkg"
  private val DomainVer   = "2.0.0"
  private val InnerTypeId = "convtest.testpkg/:#InnerPayload"

  def main(args: Array[String]): Unit = {
    args.toList match {
      case "write" :: outputDir :: format :: Nil =>
        val dir = Paths.get(outputDir).toAbsolutePath.normalize()
        Files.createDirectories(dir)
        val sampleData = createSampleData()
        val sampleAny  = createSampleAnyShowcase()
        val ctx        = BaboonCodecContext.Default
        val facadeCtx  = BaboonCodecContext.WithFacade(useIndices = false, freshFacade())
        format match {
          case "json" =>
            writeJson(ctx, sampleData, dir.toString)
            writeJsonAny(facadeCtx, sampleAny, dir.toString)
          case "ueba" =>
            writeUeba(ctx, sampleData, dir.toString)
            writeUebaAny(facadeCtx, sampleAny, dir.toString)
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
    val sampleAny  = createSampleAnyShowcase()
    val baseDir    = Paths.get("../../target/compat-test").toAbsolutePath.normalize()

    val scalaJsonDir = baseDir.resolve("scala-json")
    val scalaUebaDir = baseDir.resolve("scala-ueba")
    val scalaReprDir = baseDir.resolve("scala-repr")
    Files.createDirectories(scalaJsonDir)
    Files.createDirectories(scalaUebaDir)
    Files.createDirectories(scalaReprDir)

    val ctx       = BaboonCodecContext.Default
    val facadeCtx = BaboonCodecContext.WithFacade(useIndices = false, freshFacade())
    writeJson(ctx, sampleData, scalaJsonDir.toString)
    writeUeba(ctx, sampleData, scalaUebaDir.toString)
    writeJsonAny(facadeCtx, sampleAny, scalaJsonDir.toString)
    writeUebaAny(facadeCtx, sampleAny, scalaUebaDir.toString)
    writePointIdRepr(sampleData.vPointId, scalaReprDir.toString)
    writeForeignKeyHolderJson(ctx, createForeignKeyHolderSample(), scalaJsonDir.toString)
    writeBuiltinMapKeyHolderJson(ctx, createBuiltinMapKeyHolderSample(), scalaJsonDir.toString)
    writeBuiltinMapKeyHolderUeba(ctx, createBuiltinMapKeyHolderSample(), scalaUebaDir.toString)

    println("Scala serialization complete!")
  }

  // PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
  // Canonical single-entry maps; each key uses the canonical string form
  // produced by `value.toString` across all 10 backends for the safe subset
  // (i32/i64 with positive values; u32/u64 with small positive values to
  // avoid Scala signed-Long unsigned-encoding divergence; bit; uid).
  private def createBuiltinMapKeyHolderSample(): BuiltinMapKeyHolder = {
    BuiltinMapKeyHolder(
      mi32 = Map(42                   -> "v32"),
      mi64 = Map(9223372036854775807L -> "vmax"),
      mu32 = Map(7                    -> "vu32"),
      mbit = Map(true                 -> "vt"),
      muid = Map(UUID.fromString("00000000-0000-0000-0000-000000000001") -> "vid"),
    )
  }

  private def writeBuiltinMapKeyHolderJson(ctx: BaboonCodecContext, data: BuiltinMapKeyHolder, outputDir: String): Unit = {
    val json    = BuiltinMapKeyHolder_JsonCodec.instance.encode(ctx, data)
    // Compact form (`noSpaces`) so the cross-language byte-identity assertion
    // can compare files literally. Reference fixture lives at
    // `test/conv-test/json-data/m26-builtin-map-keys.json`.
    val jsonStr = json.noSpaces
    val path    = Paths.get(outputDir).resolve("m26-builtin-map-keys.json")
    Files.write(path, jsonStr.getBytes("UTF-8"))
    println(s"Written JSON to $path")
  }

  private def writeBuiltinMapKeyHolderUeba(ctx: BaboonCodecContext, data: BuiltinMapKeyHolder, outputDir: String): Unit = {
    val baos       = new ByteArrayOutputStream()
    val uebaWriter = new LEDataOutputStream(baos)
    try {
      BuiltinMapKeyHolder_UEBACodec.instance.encode(ctx, uebaWriter, data)
      uebaWriter.flush()
      val uebaBytes = baos.toByteArray
      val path      = Paths.get(outputDir).resolve("m26-builtin-map-keys.ueba")
      Files.write(path, uebaBytes)
      println(s"Written UEBA to $path")
    } finally {
      uebaWriter.close()
    }
  }

  // PR-I.1a (M24 Phase 3.1) — Custom-foreign KeyCodec hook canonical fixture.
  // The map keys go through FStr_KeyCodec (default identity impl for the stringy
  // foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
  private def createForeignKeyHolderSample(): ForeignKeyHolder = {
    ForeignKeyHolder(
      m = Map(
        ItemKey(v = "alpha") -> "v1",
        ItemKey(v = "beta")  -> "v2",
      )
    )
  }

  private def writeForeignKeyHolderJson(ctx: BaboonCodecContext, data: ForeignKeyHolder, outputDir: String): Unit = {
    val json    = ForeignKeyHolder_JsonCodec.instance.encode(ctx, data)
    val jsonStr = json.spaces2
    val path    = Paths.get(outputDir).resolve("m24-foreign-keycodec.json")
    Files.write(path, jsonStr.getBytes("UTF-8"))
    println(s"Written JSON to $path")
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

  // PR-57e (M18.4e) — cross-language identifier repr (toString) byte-identity.
  // Per spec §7 the repr/toString form is a separate invariant from the JSON/UEBA wire bytes;
  // we write it as a per-language artifact so the Scala-side test can assert all 10 backends
  // produce byte-identical output for the same canonical PointId value.
  private def writePointIdRepr(pid: PointId, outputDir: String): Unit = {
    val path = Paths.get(outputDir).resolve("point-id.txt")
    // No trailing newline — exact byte match across all languages.
    Files.write(path, pid.toString.getBytes("UTF-8"))
    println(s"Written repr to $path")
  }

  private def writeJsonAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: String): Unit = {
    val json    = AnyShowcase_JsonCodec.instance.encode(ctx, data)
    val jsonStr = json.spaces2
    val path    = Paths.get(outputDir).resolve("any-showcase.json")
    Files.write(path, jsonStr.getBytes("UTF-8"))
    println(s"Written JSON to $path")
  }

  private def writeUebaAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: String): Unit = {
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    try {
      AnyShowcase_UEBACodec.instance.encode(ctx, out, data)
      out.flush()
      val bytes = baos.toByteArray
      val path  = Paths.get(outputDir).resolve("any-showcase.ueba")
      Files.write(path, bytes)
      println(s"Written UEBA to $path")
    } finally {
      out.close()
    }
  }

  private def readAndVerify(filePath: String): Unit = {
    if (filePath.endsWith("any-showcase.json") || filePath.endsWith("any-showcase.ueba")) {
      readAndVerifyAnyShowcase(filePath)
      return
    }
    val ctx  = BaboonCodecContext.Default
    val path = Paths.get(filePath)
    val data: AllBasicTypes =
      try {
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

  private def readAndVerifyAnyShowcase(filePath: String): Unit = {
    // Decode-side does not require a facade: AnyShowcase decode produces only same-branch
    // AnyOpaque values (JSON wire → AnyOpaqueJson, UEBA wire → AnyOpaqueUeba), and we then decode
    // the inner payload directly via InnerPayload_*Codec.
    val ctx       = BaboonCodecContext.Default
    val path      = Paths.get(filePath)
    val data: AnyShowcase =
      try {
        if (filePath.endsWith(".json")) {
          val jsonStr = new String(Files.readAllBytes(path), "UTF-8")
          val json    = parse(jsonStr).getOrElse(throw new RuntimeException(s"Failed to parse JSON from $filePath"))
          AnyShowcase_JsonCodec.instance.decode(ctx, json) match {
            case Right(d)  => d
            case Left(err) => throw new RuntimeException(s"AnyShowcase JSON decode failed: $err")
          }
        } else {
          val bytes = Files.readAllBytes(path)
          val r     = new LEDataInputStream(new ByteArrayInputStream(bytes))
          try {
            AnyShowcase_UEBACodec.instance.decode(ctx, r) match {
              case Right(d)  => d
              case Left(err) => throw new RuntimeException(s"AnyShowcase UEBA decode failed: $err")
            }
          } finally {
            r.close()
          }
        }
      } catch {
        case e: Exception =>
          System.err.println(s"AnyShowcase deserialization failed: ${e.getMessage}")
          sys.exit(1)
          throw new RuntimeException("unreachable")
      }

    val expected = expectedInnerPayloads()
    val decoded  = decodeAllPayloads(data)
    expected.zip(decoded).zipWithIndex.foreach {
      case ((exp, got), idx) =>
        if (exp != got) {
          System.err.println(s"AnyShowcase payload $idx mismatch: expected $exp, got $got")
          sys.exit(1)
        }
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
      // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
      vWireEnum  = WireEnum.Cafe,
      // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
      // i32 LE values on UEBA — byte-identical to a `data` of the same shape
      // per docs/spec/identifier-repr.md §1.3 / §7.
      vPointId   = PointId(42, -7),
      // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
      // types — single- or multi-field — use canonical repr toString as the
      // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
      // Canonical deterministic uuids ensure cross-language byte-identity.
      vmapItemIdU32 = Map(
        ItemId(UUID.fromString("00000000-0000-0000-0000-000000000001")) -> 1,
        ItemId(UUID.fromString("00000000-0000-0000-0000-000000000002")) -> 2,
      ),
      vmapCompositeIdU32 = Map(
        CompositeId(
          UUID.fromString("00000000-0000-0000-0000-0000000000aa"),
          UUID.fromString("00000000-0000-0000-0000-0000000000bb"),
        ) -> 100,
        CompositeId(
          UUID.fromString("00000000-0000-0000-0000-0000000000cc"),
          UUID.fromString("00000000-0000-0000-0000-0000000000dd"),
        ) -> 200,
      ),
    )
  }

  // Expected logical InnerPayload contents per AnyShowcase slot, in deterministic order:
  // [vAnyA, vAnyB, vAnyC, vAnyD1, vAnyD2, vAnyD3, optAny, lstAny[0]].
  // Each language emits the same sequence so cross-language reads can compare positionally.
  // Distinct labels guard against field-shuffling regressions in any one language's emitter.
  private def expectedInnerPayloads(): Seq[InnerPayload] = Seq(
    InnerPayload("variant-A", 1),
    InnerPayload("variant-B", 2),
    InnerPayload("variant-C", 3),
    InnerPayload("variant-D1", 4),
    InnerPayload("variant-D2", 5),
    InnerPayload("variant-D3", 6),
    InnerPayload("opt-any", 7),
    InnerPayload("lst-any-0", 8),
  )

  // Resolve every AnyOpaque slot in a decoded AnyShowcase to a typed InnerPayload by decoding the
  // payload directly via InnerPayload_*Codec. We deliberately do NOT use facade.decodeAny for the
  // partial-meta variants (B/C/D1/D2/D3): decodeAny requires fully-populated meta on the input
  // (domain + version + typeid) and the wire only carries what each variant's kind byte claims.
  // The whole point of this test is to verify that cross-language UEBA/JSON encoding preserves the
  // inner payload bytes/json in the AnyOpaque content slot — we don't need facade resolution to
  // assert that. (facade.decodeAny is exercised by per-language stub tests.)
  private def decodeAllPayloads(v: AnyShowcase): Seq[InnerPayload] = {
    val opaques: Seq[AnyOpaque] = Seq(
      v.vAnyA,
      v.vAnyB,
      v.vAnyC,
      v.vAnyD1,
      v.vAnyD2,
      v.vAnyD3,
      v.optAny.getOrElse(throw new RuntimeException("optAny was None; expected Some")),
      v.lstAny.headOption.getOrElse(throw new RuntimeException("lstAny was empty; expected one element")),
    )
    opaques.map(decodeInner)
  }

  // Decode an AnyOpaque whose payload is known to be InnerPayload.
  private def decodeInner(o: AnyOpaque): InnerPayload = o match {
    case AnyOpaqueUeba(_, bytes) =>
      val r = new LEDataInputStream(new ByteArrayInputStream(bytes))
      try {
        InnerPayload_UEBACodec.instance.decode(BaboonCodecContext.Compact, r) match {
          case Right(p)  => p
          case Left(err) => throw new RuntimeException(s"InnerPayload UEBA decode failed: ${err.getMessage}")
        }
      } finally {
        r.close()
      }
    case AnyOpaqueJson(_, json) =>
      InnerPayload_JsonCodec.instance.decode(BaboonCodecContext.Compact, json) match {
        case Right(p)  => p
        case Left(err) => throw new RuntimeException(s"InnerPayload JSON decode failed: ${err.getMessage}")
      }
  }

  // A facade with the convtest.testpkg/2.0.0 codecs registered so cross-format conversions and
  // decodeAny can resolve InnerPayload via (domain, version, typeid). Conversions/meta are not
  // wired — the cross-format helpers only need the codec sides.
  private def freshFacade(): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    val _ = f.register(BaboonDomainVersion(DomainId, DomainVer), BaboonCodecsJson, BaboonCodecsUeba)
    f
  }

  // Build the canonical AnyShowcase fixture. Untyped variants (A/B/C) carry full meta on wire so
  // any decoder can resolve InnerPayload from the wire alone. Typed variants (D1/D2/D3) carry only
  // the meta bits their kind byte claims; statics fill the rest at decode time.
  // All slots use the JSON branch (AnyOpaqueJson) for the JSON-side encode and the UEBA branch
  // (AnyOpaqueUeba) for the UEBA-side encode by re-building the holder for each format. We expose
  // a single AnyShowcase factory and let the writer pick the appropriate ctx; cross-format encode
  // resolves via the facade for branches that don't match the target wire format.
  private def createSampleAnyShowcase(): AnyShowcase = {
    val payloads = expectedInnerPayloads()
    val (a, b, c, d1, d2, d3, optP, lstP) =
      (payloads(0), payloads(1), payloads(2), payloads(3), payloads(4), payloads(5), payloads(6), payloads(7))

    // Pre-compute UEBA bytes and JSON for each payload.
    def uebaBytes(p: InnerPayload): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val out  = new LEDataOutputStream(baos)
      try {
        InnerPayload_UEBACodec.instance.encode(BaboonCodecContext.Compact, out, p)
        out.flush()
      } finally {
        out.close()
      }
      baos.toByteArray
    }
    def asJson(p: InnerPayload): Json = InnerPayload_JsonCodec.instance.encode(BaboonCodecContext.Compact, p)

    // Untyped variants A/B/C: AnyOpaqueJson with full wire meta; cross-format encoder will resolve
    // to UEBA via the wire-typeid lookup. We pick JSON here so the JSON-side encode is native and
    // the UEBA-side encode exercises facade.jsonToUebaBytes (covers PR 2.3 cross-format plumbing).
    val metaA = AnyMeta(0x07.toByte, Some(DomainId), Some(DomainVer), Some(InnerTypeId))
    val metaB = AnyMeta(0x03.toByte, None, Some(DomainVer), Some(InnerTypeId))
    val metaC = AnyMeta(0x01.toByte, None, None, Some(InnerTypeId))

    // Typed variants D1/D2/D3: meta omits whatever the kind byte says is absent; statics resolve.
    val metaD1 = AnyMeta(0x06.toByte, Some(DomainId), Some(DomainVer), None)
    val metaD2 = AnyMeta(0x02.toByte, None, Some(DomainVer), None)
    val metaD3 = AnyMeta(0x00.toByte, None, None, None)

    // For maximum cross-format coverage, we mix branches: A/B/C as JSON branch, D1/D2/D3 as UEBA
    // branch. The JSON-side encoder will cross-convert D1/D2/D3 (UEBA→JSON) via the facade; the
    // UEBA-side encoder will cross-convert A/B/C (JSON→UEBA) via the facade. opt/lst use one of
    // each to cover both branches in nested positions.
    AnyShowcase(
      vAnyA  = AnyOpaqueJson(metaA, asJson(a)),
      vAnyB  = AnyOpaqueJson(metaB, asJson(b)),
      vAnyC  = AnyOpaqueJson(metaC, asJson(c)),
      vAnyD1 = AnyOpaqueUeba(metaD1, uebaBytes(d1)),
      vAnyD2 = AnyOpaqueUeba(metaD2, uebaBytes(d2)),
      vAnyD3 = AnyOpaqueUeba(metaD3, uebaBytes(d3)),
      optAny = Some(AnyOpaqueJson(AnyMeta(0x07.toByte, Some(DomainId), Some(DomainVer), Some(InnerTypeId)), asJson(optP))),
      lstAny = List(AnyOpaqueUeba(AnyMeta(0x06.toByte, Some(DomainId), Some(DomainVer), None), uebaBytes(lstP))),
    )
  }
}

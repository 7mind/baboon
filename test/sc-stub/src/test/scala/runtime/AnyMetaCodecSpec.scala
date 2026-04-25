// NOTE: This test references generated runtime symbols (LEDataOutputStream,
// LEDataInputStream, AnyMeta, AnyMetaCodec, BaboonCodecException, ...) which
// are copied into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Running `sbt test`
// directly from the source tree will fail with missing symbols; run the
// test suite from the codegen'd copy.
package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class AnyMetaCodecSpec extends AnyFunSuite {

  // (kind, domain?, version?, typeid?) for all six locked meta-kind bytes.
  private val cases: List[(Byte, Option[String], Option[String], Option[String])] = List(
    (0x07.toByte, Some("com.example.dom"), Some("1.2.3"), Some("MyType")), // A
    (0x03.toByte, None, Some("1.2.3"), Some("MyType")), // B
    (0x01.toByte, None, None, Some("MyType")), // C
    (0x06.toByte, Some("com.example.dom"), Some("1.2.3"), None), // D1
    (0x02.toByte, None, Some("1.2.3"), None), // D2
    (0x00.toByte, None, None, None), // D3
  )

  test("AnyMeta construction enforces kind/Option invariant") {
    // bit 2 of kind 0x07 says domain MUST be present
    intercept[IllegalArgumentException] {
      AnyMeta(0x07.toByte, None, Some("v"), Some("t"))
    }
    // bit 2 of kind 0x03 says domain MUST be absent
    intercept[IllegalArgumentException] {
      AnyMeta(0x03.toByte, Some("d"), Some("v"), Some("t"))
    }
    // bit 0 of kind 0x06 says typeid MUST be absent
    intercept[IllegalArgumentException] {
      AnyMeta(0x06.toByte, Some("d"), Some("v"), Some("t"))
    }
  }

  test("AnyMeta rejects reserved meta-kind bytes 0x04 and 0x05") {
    // 0x04 has DOMAIN_BIT set but is reserved per spec v1 wire format
    intercept[IllegalArgumentException] {
      AnyMeta(0x04.toByte, Some("d"), None, None)
    }
    // 0x05 has DOMAIN_BIT + TYPEID_BIT set but is reserved per spec v1
    intercept[IllegalArgumentException] {
      AnyMeta(0x05.toByte, Some("d"), None, Some("t"))
    }
  }

  test("AnyMetaCodec.writeBin/readBin round-trip across all six kind bytes") {
    cases.foreach {
      case (kind, dom, ver, tid) =>
        val meta = AnyMeta(kind, dom, ver, tid)
        val baos = new ByteArrayOutputStream()
        val out  = new LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()

        val in    = new LEDataInputStream(new ByteArrayInputStream(baos.toByteArray))
        val round = AnyMetaCodec.readBin(in)
        assert(round == meta, s"binary round-trip failed for kind 0x${(kind & 0xFF).toHexString}")
    }
  }

  test("AnyMetaCodec.writeBin emits exactly [kind][optional ULEB128-prefixed UTF-8 strings]") {
    // kind 0x07: 1 byte kind + 3 strings, each varint-len + UTF-8 bytes
    val meta = AnyMeta(0x07.toByte, Some("a"), Some("b"), Some("c"))
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    AnyMetaCodec.writeBin(meta, out)
    out.flush()
    // 1 (kind) + (1+1)*3 = 7 bytes
    assert(baos.toByteArray.length == 7, s"unexpected size for kind 0x07: ${baos.toByteArray.length}")

    // kind 0x00: 1 byte kind, no strings
    val empty = AnyMeta(0x00.toByte, None, None, None)
    val baos2 = new ByteArrayOutputStream()
    val out2  = new LEDataOutputStream(baos2)
    AnyMetaCodec.writeBin(empty, out2)
    out2.flush()
    assert(baos2.toByteArray.length == 1, s"unexpected size for kind 0x00: ${baos2.toByteArray.length}")
    assert(baos2.toByteArray()(0) == 0x00.toByte)
  }

  test("AnyMetaCodec.writeBin/readBin round-trip for non-ASCII UTF-8 strings") {
    val meta = AnyMeta(0x07.toByte, Some("日本語"), Some("1.2.3"), Some("タイプ"))
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    AnyMetaCodec.writeBin(meta, out)
    out.flush()
    val in    = new LEDataInputStream(new ByteArrayInputStream(baos.toByteArray))
    val round = AnyMetaCodec.readBin(in)
    assert(round == meta, s"non-ASCII UTF-8 round-trip failed: $round")
  }

  test("AnyMetaCodec.writeBin/readBin round-trip for empty string (ULEB128 length 0)") {
    val meta = AnyMeta(0x01.toByte, None, None, Some(""))
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    AnyMetaCodec.writeBin(meta, out)
    out.flush()
    val bytes = baos.toByteArray
    // 1 (kind) + 1 (ULEB128 length 0) = 2 bytes
    assert(bytes.length == 2, s"unexpected size for empty-string kind 0x01: ${bytes.length}")
    assert(bytes(0) == 0x01.toByte)
    assert(bytes(1) == 0x00.toByte)

    val in    = new LEDataInputStream(new ByteArrayInputStream(bytes))
    val round = AnyMetaCodec.readBin(in)
    assert(round == meta)
  }

  test("AnyMetaCodec.readBinWithLength reports bytes consumed and tolerates trailing meta-extension bytes") {
    // Write a valid kind 0x07 meta plus 5 bytes of "future meta extension" garbage at the end.
    val meta = AnyMeta(0x07.toByte, Some("d"), Some("v"), Some("t"))
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    AnyMetaCodec.writeBin(meta, out)
    out.flush()
    val metaBytes = baos.toByteArray
    val tail      = Array[Byte](0x11, 0x22, 0x33, 0x44, 0x55)

    val combined = metaBytes ++ tail
    val in       = new LEDataInputStream(new ByteArrayInputStream(combined))

    val (parsed, bytesRead) = AnyMetaCodec.readBinWithLength(in)
    assert(parsed == meta, s"expected $meta, got $parsed")
    assert(bytesRead == metaBytes.length, s"expected $metaBytes.length bytes consumed, got $bytesRead")

    // Caller would skip (anyMetaLen - bytesRead) bytes within the on-wire meta-length window.
    // Simulate the codec-generator's skip:
    val anyMetaLen = metaBytes.length + tail.length // outer wire claims this much meta
    val skipped    = in.skipBytes(anyMetaLen - bytesRead)
    assert(skipped == tail.length, s"expected to skip ${tail.length} trailing meta-extension bytes, skipped $skipped")
  }

  test("AnyMetaCodec.writeBin/readBin round-trip for string >= 128 bytes (multi-byte ULEB128 prefix)") {
    val longStr = "a" * 128 // exactly 128 ASCII bytes, triggers 2-byte ULEB128 prefix
    val meta    = AnyMeta(0x01.toByte, None, None, Some(longStr))
    val baos    = new ByteArrayOutputStream()
    val out     = new LEDataOutputStream(baos)
    AnyMetaCodec.writeBin(meta, out)
    out.flush()
    val bytes = baos.toByteArray
    // 1 (kind) + 2 (ULEB128 for 128 = 0x80 0x01) + 128 (UTF-8 bytes) = 131 bytes
    assert(bytes.length == 131, s"unexpected size for 128-byte string: ${bytes.length}")
    assert(bytes(0) == 0x01.toByte)
    assert((bytes(1) & 0xFF) == 0x80, s"expected ULEB128 first byte 0x80, got 0x${(bytes(1) & 0xFF).toHexString}")
    assert((bytes(2) & 0xFF) == 0x01, s"expected ULEB128 second byte 0x01, got 0x${(bytes(2) & 0xFF).toHexString}")

    val in    = new LEDataInputStream(new ByteArrayInputStream(bytes))
    val round = AnyMetaCodec.readBin(in)
    assert(round == meta)
  }

  test("AnyMetaCodec.writeJson/readJson round-trip across all six kind bytes") {
    cases.foreach {
      case (kind, dom, ver, tid) =>
        val meta  = AnyMeta(kind, dom, ver, tid)
        val json  = AnyMetaCodec.writeJson(meta)
        val round = AnyMetaCodec.readJson(json)
        assert(round == Right(meta), s"JSON round-trip failed for kind 0x${(kind & 0xFF).toHexString}: $round")
    }
  }

  test("AnyMetaCodec.writeJson emits only the keys claimed by the kind bitmask") {
    val meta = AnyMeta(0x01.toByte, None, None, Some("T"))
    val json = AnyMetaCodec.writeJson(meta)
    val obj  = json.asObject.get
    assert(obj.contains("$ak"))
    assert(obj.contains("$at"))
    assert(!obj.contains("$ad"))
    assert(!obj.contains("$av"))
    assert(obj("$ak").contains(Json.fromInt(0x01)))
    assert(obj("$at").contains(Json.fromString("T")))
  }

  test("AnyMetaCodec.readJson rejects payloads where presence does not match kind") {
    // kind 0x07 (all bits set) but missing $ad
    val bad    = Json.obj("$ak" -> Json.fromInt(0x07), "$av" -> Json.fromString("v"), "$at" -> Json.fromString("t"))
    val result = AnyMetaCodec.readJson(bad)
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("$ad"), s"expected error to mention $$ad: $msg")

    // kind 0x00 (no bits set) but extra $at
    val bad2    = Json.obj("$ak" -> Json.fromInt(0x00), "$at" -> Json.fromString("t"))
    val result2 = AnyMetaCodec.readJson(bad2)
    assert(result2.isLeft, s"expected Left, got $result2")
    val msg2 = result2.swap.toOption.get.getMessage
    assert(msg2.contains("$at"), s"expected error to mention $$at: $msg2")
  }

  test("AnyMetaCodec.readJson rejects $ak as a non-numeric string") {
    val bad    = Json.obj("$ak" -> Json.fromString("not-a-number"), "$at" -> Json.fromString("T"))
    val result = AnyMetaCodec.readJson(bad)
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("$ak"), s"expected error to mention $$ak: $msg")
  }

  test("decodeAny Left path: AnyOpaqueUeba with incomplete meta") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T")) // kind C: only typeid
    val result = facade.decodeAny(AnyOpaqueUeba(meta, Array.empty[Byte]))
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("domain"), s"expected message to mention 'domain': $msg")
    assert(msg.contains("version"), s"expected message to mention 'version': $msg")
  }

  test("decodeAny Left path: AnyOpaqueJson with incomplete meta") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T")) // kind C: only typeid
    val result = facade.decodeAny(AnyOpaqueJson(meta, Json.Null))
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("domain"), s"expected message to mention 'domain': $msg")
    assert(msg.contains("version"), s"expected message to mention 'version': $msg")
  }

  test("BaboonCodecContext.WithFacade exposes facade; Compact/Indexed do not") {
    val facade = new BaboonCodecsFacade {}
    assert(BaboonCodecContext.Compact.facade.isEmpty)
    assert(BaboonCodecContext.Indexed.facade.isEmpty)

    val withFacadeCompact = BaboonCodecContext.WithFacade(useIndices = false, facade)
    assert(withFacadeCompact.facade.contains(facade))
    assert(!withFacadeCompact.useIndices)

    val withFacadeIndexed = BaboonCodecContext.WithFacade(useIndices = true, facade)
    assert(withFacadeIndexed.facade.contains(facade))
    assert(withFacadeIndexed.useIndices)
  }

  test("jsonToUebaBytes Left path: incomplete meta") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T")) // kind C: only typeid
    val result = facade.jsonToUebaBytes(meta, Json.Null)
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("domain"), s"expected message to mention 'domain': $msg")
    assert(msg.contains("version"), s"expected message to mention 'version': $msg")
  }

  test("uebaToJson Left path: incomplete meta") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T")) // kind C: only typeid
    val result = facade.uebaToJson(meta, Array.empty[Byte])
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("domain"), s"expected message to mention 'domain': $msg")
    assert(msg.contains("version"), s"expected message to mention 'version': $msg")
  }

  test("jsonToUebaBytes Left path: no codec registered") {
    val facade = new BaboonCodecsFacade {}
    // Complete meta but no domain registered → CodecNotFound from getCodec.
    val meta   = AnyMeta(0x07.toByte, Some("com.example.unknown"), Some("1.0.0"), Some("Unknown"))
    val result = facade.jsonToUebaBytes(meta, Json.obj("x" -> Json.fromInt(1)))
    assert(result.isLeft, s"expected Left, got $result")
    assert(result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound], s"expected CodecNotFound, got ${result.swap.toOption.get}")
  }

  test("uebaToJson Left path: no codec registered") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x07.toByte, Some("com.example.unknown"), Some("1.0.0"), Some("Unknown"))
    val result = facade.uebaToJson(meta, Array.empty[Byte])
    assert(result.isLeft, s"expected Left, got $result")
    assert(result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound], s"expected CodecNotFound, got ${result.swap.toOption.get}")
  }

  // Locks in the encoder envelope invariant. The JSON encoder builds the wire envelope as
  // `AnyMetaCodec.writeJson(meta).mapObject(_.add("$c", inner))`. If `writeJson` ever returned a
  // non-object Json, `mapObject` would silently no-op and the `$c` key would be dropped. (D08.)
  test("AnyMetaCodec.writeJson always returns a JSON object across all six kind bytes") {
    cases.foreach {
      case (kind, dom, ver, tid) =>
        val meta = AnyMeta(kind, dom, ver, tid)
        val json = AnyMetaCodec.writeJson(meta)
        assert(json.isObject, s"writeJson must return an object for kind 0x${(kind & 0xFF).toHexString}, got: $json")
    }
  }

  test("jsonToUebaBytes static-fallback: missing meta.domain falls back to staticDomain") {
    val facade = new BaboonCodecsFacade {}
    // kind 0x03 (B): no domain on wire, version + typeid present.
    val meta   = AnyMeta(0x03.toByte, None, Some("1.0.0"), Some("T"))
    val result = facade.jsonToUebaBytes(meta, Json.Null, staticDomain = Some("com.example.unknown"))
    // Should reach codec lookup (fail with CodecNotFound), not the missing-meta DecoderFailure.
    assert(result.isLeft, s"expected Left, got $result")
    assert(
      result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound],
      s"expected CodecNotFound (static fallback wired), got ${result.swap.toOption.get}",
    )
  }

  test("jsonToUebaBytes static-fallback: missing meta.version falls back to staticVersion") {
    val facade = new BaboonCodecsFacade {}
    // kind 0x05 is reserved; use kind 0x01 (only typeid) and supply both staticDomain + staticVersion.
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T"))
    val result = facade.jsonToUebaBytes(meta, Json.Null, staticDomain = Some("com.example.unknown"), staticVersion = Some("1.0.0"))
    assert(result.isLeft, s"expected Left, got $result")
    assert(
      result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound],
      s"expected CodecNotFound (static fallback wired), got ${result.swap.toOption.get}",
    )
  }

  test("jsonToUebaBytes static-fallback: missing meta.typeid falls back to staticTypeid") {
    val facade = new BaboonCodecsFacade {}
    // kind 0x06 (D1): domain + version on wire, typeid absent.
    val meta   = AnyMeta(0x06.toByte, Some("com.example.unknown"), Some("1.0.0"), None)
    val result = facade.jsonToUebaBytes(meta, Json.Null, staticTypeid = Some("Inner"))
    assert(result.isLeft, s"expected Left, got $result")
    assert(
      result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound],
      s"expected CodecNotFound (static fallback wired), got ${result.swap.toOption.get}",
    )
  }

  test("jsonToUebaBytes static-fallback: D3 (no meta on wire) requires all three statics") {
    val facade = new BaboonCodecsFacade {}
    // kind 0x00 (D3): nothing on wire.
    val meta = AnyMeta(0x00.toByte, None, None, None)
    val result = facade.jsonToUebaBytes(
      meta,
      Json.Null,
      staticDomain  = Some("com.example.unknown"),
      staticVersion = Some("1.0.0"),
      staticTypeid  = Some("Inner"),
    )
    assert(result.isLeft, s"expected Left, got $result")
    assert(
      result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound],
      s"expected CodecNotFound (static fallback wired), got ${result.swap.toOption.get}",
    )
  }

  test("jsonToUebaBytes static-fallback: meta wins over static (override semantics)") {
    val facade = new BaboonCodecsFacade {}
    // meta has all three present; statics differ — meta should win, so resolution targets meta.domain.
    val meta = AnyMeta(0x07.toByte, Some("com.example.metawins"), Some("1.0.0"), Some("MetaT"))
    val result = facade.jsonToUebaBytes(
      meta,
      Json.Null,
      staticDomain  = Some("com.example.staticloses"),
      staticVersion = Some("9.9.9"),
      staticTypeid  = Some("StaticT"),
    )
    // No domain registered for either, so this must fail with CodecNotFound. The error message
    // should reference meta.domain ("metawins"), not the static ("staticloses").
    assert(result.isLeft, s"expected Left, got $result")
    val e = result.swap.toOption.get
    assert(e.isInstanceOf[BaboonCodecException.CodecNotFound], s"expected CodecNotFound, got $e")
    assert(
      e.getMessage.contains("metawins"),
      s"expected error to reference wire-meta domain 'metawins' (override semantics), got: ${e.getMessage}",
    )
  }

  test("uebaToJson static-fallback: D3 wire (no meta components) resolves via statics") {
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x00.toByte, None, None, None)
    val result = facade.uebaToJson(
      meta,
      Array.empty[Byte],
      staticDomain  = Some("com.example.unknown"),
      staticVersion = Some("1.0.0"),
      staticTypeid  = Some("Inner"),
    )
    assert(result.isLeft, s"expected Left, got $result")
    assert(
      result.swap.toOption.get.isInstanceOf[BaboonCodecException.CodecNotFound],
      s"expected CodecNotFound (static fallback wired), got ${result.swap.toOption.get}",
    )
  }

  test("decodeAny still requires complete meta (no static fallback for user-facing path)") {
    // Regression: PR 2.3 added static-fallback support to the cross-format helpers; decodeAny
    // (PR 2.1) is user-facing without static context and must keep its all-Some-required contract.
    val facade = new BaboonCodecsFacade {}
    val meta   = AnyMeta(0x01.toByte, None, None, Some("T"))
    val result = facade.decodeAny(AnyOpaqueUeba(meta, Array.empty[Byte]))
    assert(result.isLeft, s"expected Left, got $result")
    val msg = result.swap.toOption.get.getMessage
    assert(msg.contains("domain"), s"expected message to mention 'domain': $msg")
    assert(msg.contains("version"), s"expected message to mention 'version': $msg")
  }

  test("AnyOpaqueUeba equality compares bytes by content, not reference") {
    val meta = AnyMeta(0x07.toByte, Some("d"), Some("v"), Some("t"))

    val a = AnyOpaqueUeba(meta, Array[Byte](1, 2, 3))
    val b = AnyOpaqueUeba(meta, Array[Byte](1, 2, 3))
    assert(a == b, "content-equal arrays must yield equal AnyOpaqueUeba")
    assert(a.hashCode() == b.hashCode(), "equal AnyOpaqueUeba must have equal hashCode")

    val c = AnyOpaqueUeba(meta, Array[Byte](1, 2, 4))
    assert(a != c, "content-different arrays must yield non-equal AnyOpaqueUeba")

    val emptyA = AnyOpaqueUeba(meta, Array.emptyByteArray)
    val emptyB = AnyOpaqueUeba(meta, new Array[Byte](0))
    assert(emptyA == emptyB, "two distinct empty arrays must yield equal AnyOpaqueUeba")
  }
}

// NOTE: This test references generated runtime symbols (LEDataOutputStream,
// LEDataInputStream, AnyMeta, AnyMetaCodec, BaboonCodecsFacade, ...) AND generated
// DTO/codec symbols (my.ok.Holder, my.ok.Inner, etc.) which are copied/generated
// into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Running `sbt test`
// directly from the source tree will fail with missing symbols; run the
// test suite from the codegen'd copy.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 2.4 / M2 close).
// Exercises the `any-ok` fixture's six DSL variants (A=any, B=any[domain:this],
// C=any[domain:current], D1=any[Inner], D2=any[domain:this,Inner],
// D3=any[domain:current,Inner]) plus the three nested positions (opt/lst/map-value).
package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class AnyRoundTripSpec extends AnyFunSuite {

  // Fresh per-spec facade: registers Holder/Inner codecs from the my.ok domain so the
  // cross-format helpers and decodeAny can resolve `(domain, version, typeid)` triples.
  // We deliberately avoid relying on a global/static facade so this spec stays self-contained.
  //
  // BaboonCodecsFacade.getCodec branches on (modelVersion vs minVersion vs maxVersion). For
  // single-version domains (modelVersion == min == max), the non-`exact` cases fall through to
  // the catch-all "Unsupported domain version" error — a latent defect in the runtime predating
  // PR 2.4. The `decodeFromBin`, `decodeAny`, and cross-convert helpers all use `exact=false`,
  // so they would all break here. The workaround is to register a synthetic future-version
  // (2.0.0) for the same domain — bumping `max` past `model` so the second case-arm matches and
  // `getCodecMaxCompat` is reached. The synthetic version's meta is shared with 1.0.0; nothing
  // looks it up because `versionsMeta.get(modelVersion=1.0.0)` succeeds first.
  private def freshFacade(): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(
      BaboonDomainVersion(my.ok.Holder.baboonDomainIdentifier, my.ok.Holder.baboonDomainVersion),
      my.ok.BaboonCodecsJson,
      my.ok.BaboonCodecsUeba,
      my.ok.BaboonMetadata,
    )
    // Synthetic newer version (no codecs) so 1.0.0 is treated as "not the latest" and dispatches
    // through getCodecMaxCompat.
    f.register(
      BaboonDomainVersion(my.ok.Holder.baboonDomainIdentifier, "2.0.0"),
      my.ok.BaboonMetadata,
    )
    f
  }

  private val facade: BaboonCodecsFacade = freshFacade()
  private val ctxWithFacade: BaboonCodecContext.WithFacade = BaboonCodecContext.WithFacade(useIndices = false, facade)

  // Domain/version/typeid strings the codec generator emits as static fallbacks.
  private val domainId   = "my.ok"
  private val versionStr = "1.0.0"
  private val innerType  = "my.ok/:#Inner"

  // ---------------------------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------------------------

  // Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
  // for D-variant fields (which the encoder accepts only when meta.kind matches the field's variant).
  private def innerToUebaBytes(inner: my.ok.Inner): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    try {
      my.ok.Inner_UEBACodec.encode(BaboonCodecContext.Compact, out, inner)
    } finally {
      out.close()
    }
    baos.toByteArray
  }

  private def innerToJson(inner: my.ok.Inner): Json = {
    my.ok.Inner_JsonCodec.encode(BaboonCodecContext.Compact, inner)
  }

  // The fixture's six variants → their static-emitted (domain, version, typeid) triples.
  // Mirrors the codec-generator output for Holder (see PR 2.3 docs §"Per-variant static emission").
  // Used to populate AnyMeta with bits that match the field's variant kind.
  private val sampleInner: my.ok.Inner = my.ok.Inner(42)
  private val innerBytes: Array[Byte]  = innerToUebaBytes(sampleInner)
  private val innerJson: Json          = innerToJson(sampleInner)

  // Construct AnyMeta for each variant, populating the bits the kind byte claims.
  // For untyped variants A/B/C, payload is just an empty byte sequence (no inner type to encode).
  private def metaA: AnyMeta = AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some("opaque.Type")) // typeid is opaque text on wire
  private def metaB: AnyMeta = AnyMeta(0x03.toByte, None, Some(versionStr), Some("opaque.Type"))
  private def metaC: AnyMeta = AnyMeta(0x01.toByte, None, None, Some("opaque.Type"))
  // For D-variants the typeid must be ABSENT on wire when the bit is unset; we still place a real
  // Inner payload as bytes since the round-trip framing doesn't inspect it.
  private def metaD1: AnyMeta = AnyMeta(0x06.toByte, Some(domainId), Some(versionStr), None)
  private def metaD2: AnyMeta = AnyMeta(0x02.toByte, None, Some(versionStr), None)
  private def metaD3: AnyMeta = AnyMeta(0x00.toByte, None, None, None)

  // Build a complete Holder with one AnyOpaqueUeba per variant. UEBA branch round-trips natively
  // (no facade needed for encode/decode — facade is only consulted for cross-convert).
  private def buildUebaHolder(): my.ok.Holder = {
    my.ok.Holder(
      fAny               = AnyOpaqueUeba(metaA, Array[Byte](1, 2, 3)),
      fDomainThis        = AnyOpaqueUeba(metaB, Array[Byte](4, 5)),
      fDomainCurrent     = AnyOpaqueUeba(metaC, Array[Byte](6)),
      fUnderlying        = AnyOpaqueUeba(metaD1, innerBytes),
      fThisUnderlying    = AnyOpaqueUeba(metaD2, innerBytes),
      fCurrentUnderlying = AnyOpaqueUeba(metaD3, innerBytes),
      fOpt               = Some(AnyOpaqueUeba(metaA, Array[Byte](7))), // opt[any] -> A variant
      fLst               = List(AnyOpaqueUeba(metaD1, innerBytes)),    // lst[any[Inner]] -> D1 variant
      fMapValue          = Map("k1" -> AnyOpaqueUeba(metaA, Array[Byte](8))),
    )
  }

  // Build a Holder using AnyOpaqueJson branches everywhere — encoding to UEBA via facade
  // cross-convert (jsonToUebaBytes), then decoding returns AnyOpaqueUeba branches.
  private def buildJsonHolder(): my.ok.Holder = {
    // We need real inner JSON for D variants because cross-convert decodes them.
    // For A/B/C: inner type isn't statically known; we use a JSON object that the registered
    // codec for `Inner` (resolved via meta.typeid="my.ok/:#Inner") will decode.
    val innerJ = innerJson
    my.ok.Holder(
      // For untyped variants the typeid must point to a registered codec for cross-convert to succeed.
      fAny               = AnyOpaqueJson(metaWithTypeid(0x07.toByte, Some(domainId), Some(versionStr), innerType), innerJ),
      fDomainThis        = AnyOpaqueJson(metaWithTypeid(0x03.toByte, None, Some(versionStr), innerType), innerJ),
      fDomainCurrent     = AnyOpaqueJson(metaWithTypeid(0x01.toByte, None, None, innerType), innerJ),
      fUnderlying        = AnyOpaqueJson(metaD1, innerJ),
      fThisUnderlying    = AnyOpaqueJson(metaD2, innerJ),
      fCurrentUnderlying = AnyOpaqueJson(metaD3, innerJ),
      fOpt               = Some(AnyOpaqueJson(metaWithTypeid(0x07.toByte, Some(domainId), Some(versionStr), innerType), innerJ)),
      fLst               = List(AnyOpaqueJson(metaD1, innerJ)),
      fMapValue          = Map("k1" -> AnyOpaqueJson(metaWithTypeid(0x07.toByte, Some(domainId), Some(versionStr), innerType), innerJ)),
    )
  }

  private def metaWithTypeid(kind: Byte, dom: Option[String], ver: Option[String], tid: String): AnyMeta = {
    AnyMeta(kind, dom, ver, Some(tid))
  }

  private def encodeUebaBytes(value: my.ok.Holder, ctx: BaboonCodecContext): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    try {
      my.ok.Holder_UEBACodec.encode(ctx, out, value)
    } finally {
      out.close()
    }
    baos.toByteArray
  }

  private def decodeUebaBytes(bytes: Array[Byte]): my.ok.Holder = {
    val in = new LEDataInputStream(new ByteArrayInputStream(bytes))
    my.ok.Holder_UEBACodec.decode(BaboonCodecContext.Compact, in) match {
      case Right(h) => h
      case Left(t)  => throw new AssertionError(s"UEBA decode failed: $t")
    }
  }

  // ---------------------------------------------------------------------------------------------
  // 1. Per-variant UEBA round-trip (all 6 variants in one Holder + 3 nested positions)
  // ---------------------------------------------------------------------------------------------

  test("UEBA round-trip: all six variants + opt/lst/map-value preserve content") {
    val original = buildUebaHolder()
    val bytes    = encodeUebaBytes(original, BaboonCodecContext.Compact)
    val decoded  = decodeUebaBytes(bytes)
    assert(decoded == original, s"UEBA round-trip diverged: $decoded vs $original")
  }

  test("UEBA round-trip with useIndices=true (Indexed context) preserves content") {
    val original = buildUebaHolder()
    val bytes    = encodeUebaBytes(original, BaboonCodecContext.Indexed)
    val decoded  = decodeUebaBytes(bytes)
    assert(decoded == original)
  }

  test("UEBA decode of all six variants yields AnyOpaqueUeba with matching meta-kind bytes") {
    val original = buildUebaHolder()
    val bytes    = encodeUebaBytes(original, BaboonCodecContext.Compact)
    val decoded  = decodeUebaBytes(bytes)
    // Per-field kind assertion locks in PR 2.2's wire layout.
    assert(decoded.fAny.meta.kind == 0x07.toByte, "fAny variant A")
    assert(decoded.fDomainThis.meta.kind == 0x03.toByte, "fDomainThis variant B")
    assert(decoded.fDomainCurrent.meta.kind == 0x01.toByte, "fDomainCurrent variant C")
    assert(decoded.fUnderlying.meta.kind == 0x06.toByte, "fUnderlying variant D1")
    assert(decoded.fThisUnderlying.meta.kind == 0x02.toByte, "fThisUnderlying variant D2")
    assert(decoded.fCurrentUnderlying.meta.kind == 0x00.toByte, "fCurrentUnderlying variant D3")
    // Native UEBA decode path always returns AnyOpaqueUeba (never Json).
    decoded.fAny match {
      case _: AnyOpaqueUeba => ()
      case _                => fail("UEBA decode must yield AnyOpaqueUeba")
    }
  }

  // ---------------------------------------------------------------------------------------------
  // 2. Per-variant JSON round-trip
  // ---------------------------------------------------------------------------------------------

  // For JSON encoding we use AnyOpaqueJson directly so no facade cross-convert is needed.
  // The fixture's by-default JSON codecs handle the envelope.
  private def buildJsonNativeHolder(): my.ok.Holder = {
    // Use trivial inner JSON values; cross-convert tests will use real Inner JSON elsewhere.
    val arbitraryJson = Json.obj("payload" -> Json.fromInt(42))
    my.ok.Holder(
      fAny               = AnyOpaqueJson(metaA, arbitraryJson),
      fDomainThis        = AnyOpaqueJson(metaB, arbitraryJson),
      fDomainCurrent     = AnyOpaqueJson(metaC, arbitraryJson),
      fUnderlying        = AnyOpaqueJson(metaD1, innerJson),
      fThisUnderlying    = AnyOpaqueJson(metaD2, innerJson),
      fCurrentUnderlying = AnyOpaqueJson(metaD3, innerJson),
      fOpt               = Some(AnyOpaqueJson(metaA, arbitraryJson)),
      fLst               = List(AnyOpaqueJson(metaD1, innerJson)),
      fMapValue          = Map("k1" -> AnyOpaqueJson(metaA, arbitraryJson)),
    )
  }

  test("JSON round-trip: all six variants + opt/lst/map-value preserve content") {
    val original = buildJsonNativeHolder()
    val json     = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
    val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json) match {
      case Right(h) => h
      case Left(t)  => fail(s"JSON decode failed: $t")
    }
    assert(decoded == original, s"JSON round-trip diverged: $decoded vs $original")
  }

  test("JSON decode yields AnyOpaqueJson and preserves meta-kind across variants") {
    val original = buildJsonNativeHolder()
    val json     = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
    val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json) match {
      case Right(h) => h
      case Left(t)  => fail(s"JSON decode failed: $t")
    }
    decoded.fAny match {
      case _: AnyOpaqueJson => ()
      case _                => fail("JSON decode must yield AnyOpaqueJson")
    }
    assert(decoded.fAny.meta.kind == 0x07.toByte)
    assert(decoded.fDomainThis.meta.kind == 0x03.toByte)
    assert(decoded.fDomainCurrent.meta.kind == 0x01.toByte)
    assert(decoded.fUnderlying.meta.kind == 0x06.toByte)
    assert(decoded.fThisUnderlying.meta.kind == 0x02.toByte)
    assert(decoded.fCurrentUnderlying.meta.kind == 0x00.toByte)
  }

  // ---------------------------------------------------------------------------------------------
  // 3. Cross-format conversion via facade
  // ---------------------------------------------------------------------------------------------

  test("cross-format: encode JSON-Holder to UEBA via facade.jsonToUebaBytes; decode back") {
    // buildJsonHolder uses AnyOpaqueJson branches for ALL fields; encoding to UEBA forces
    // jsonToUebaBytes per field. After decode the branches are AnyOpaqueUeba.
    val original = buildJsonHolder()
    val bytes    = encodeUebaBytes(original, ctxWithFacade)
    val decoded  = decodeUebaBytes(bytes)

    // Re-encode to UEBA using the now-Ueba-branched holder (no facade needed) and assert byte
    // equality — proves the cross-converted bytes match a native UEBA encode of the equivalent value.
    val rebytes = encodeUebaBytes(decoded, BaboonCodecContext.Compact)
    assert(java.util.Arrays.equals(bytes, rebytes), "JSON->UEBA cross-convert produced non-canonical bytes")
  }

  test("cross-format: encode Ueba-Holder to JSON via facade.uebaToJson; decode back") {
    // buildUebaHolder uses AnyOpaqueUeba branches everywhere; encoding to JSON triggers
    // uebaToJson for each field. Decoding the resulting JSON yields AnyOpaqueJson branches.
    val ueba = buildUebaHolder()
    // For untyped variants A/B/C the static fallbacks are insufficient (kind A: no statics on wire,
    // and A's static set is all-None per spec). The wire meta itself must carry full triples; we
    // already populated metaA with all three. uebaToJson will resolve via wire-meta.typeid.
    // BUT: the typeid we put in metaA ("opaque.Type") is NOT registered. To make cross-convert
    // succeed for A/B/C in this test we substitute typeid=innerType so the registered Inner codec
    // resolves and the bytes deserialize as Inner.
    val crossable = ueba.copy(
      fAny           = AnyOpaqueUeba(AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some(innerType)), innerBytes),
      fDomainThis    = AnyOpaqueUeba(AnyMeta(0x03.toByte, None, Some(versionStr), Some(innerType)), innerBytes),
      fDomainCurrent = AnyOpaqueUeba(AnyMeta(0x01.toByte, None, None, Some(innerType)), innerBytes),
      // D variants: typeid is resolved via static fallback in the codec generator emission, no wire change.
      fOpt      = Some(AnyOpaqueUeba(AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some(innerType)), innerBytes)),
      fMapValue = Map("k1" -> AnyOpaqueUeba(AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some(innerType)), innerBytes)),
    )
    val json = my.ok.Holder_JsonCodec.encode(ctxWithFacade, crossable)
    // Sanity-decode the JSON to ensure the envelope is well-formed.
    val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json) match {
      case Right(h) => h
      case Left(t)  => fail(s"post-cross-convert JSON decode failed: $t")
    }
    decoded.fAny match {
      case _: AnyOpaqueJson => ()
      case _                => fail("decode after cross-convert must yield AnyOpaqueJson")
    }
    // The meta-kinds round-trip through the envelope.
    assert(decoded.fAny.meta.kind == 0x07.toByte)
    assert(decoded.fUnderlying.meta.kind == 0x06.toByte)
    assert(decoded.fCurrentUnderlying.meta.kind == 0x00.toByte) // D3: kind=0x00, statics filled it in
  }

  test("cross-format: D3 (no meta on wire) succeeds via static fallbacks emitted by codec generator") {
    // PR-06-D01 regression: D3 has all-None meta on wire; the codec-generator emits
    // (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these, the
    // facade can't resolve and cross-convert fails.
    // We construct an isolated D3 case: a Holder with default UEBA branches everywhere except
    // fCurrentUnderlying which carries an AnyOpaqueJson (forcing jsonToUebaBytes via D3 statics).
    val mixed = buildUebaHolder().copy(
      fCurrentUnderlying = AnyOpaqueJson(metaD3, innerJson)
    )
    // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
    val bytes   = encodeUebaBytes(mixed, ctxWithFacade)
    val decoded = decodeUebaBytes(bytes)
    assert(decoded.fCurrentUnderlying.meta.kind == 0x00.toByte)
    decoded.fCurrentUnderlying match {
      case AnyOpaqueUeba(_, b) =>
        // Decoding the bytes through the inner UEBA codec must yield the original Inner.
        val in = new LEDataInputStream(new ByteArrayInputStream(b))
        my.ok.Inner_UEBACodec.decode(BaboonCodecContext.Compact, in) match {
          case Right(inner) => assert(inner == sampleInner, s"D3 cross-convert produced wrong Inner: $inner")
          case Left(t)      => fail(s"D3 cross-convert payload could not be decoded as Inner: $t")
        }
      case _ => fail("expected AnyOpaqueUeba after UEBA decode")
    }
  }

  // ---------------------------------------------------------------------------------------------
  // 4. facade.decodeAny end-to-end
  // ---------------------------------------------------------------------------------------------

  test("facade.decodeAny resolves a UEBA-encoded Inner to a typed Inner") {
    // Variant A meta carries full (domain, version, typeid) — no static fallback needed.
    val meta   = AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some(innerType))
    val opaque = AnyOpaqueUeba(meta, innerBytes)
    val result = facade.decodeAny(opaque)
    result match {
      case Right(decoded: my.ok.Inner) => assert(decoded == sampleInner)
      case Right(other)                => fail(s"decodeAny returned unexpected type: $other")
      case Left(e)                     => fail(s"decodeAny failed: $e")
    }
  }

  test("facade.decodeAny resolves a JSON-encoded Inner to a typed Inner") {
    val meta   = AnyMeta(0x07.toByte, Some(domainId), Some(versionStr), Some(innerType))
    val opaque = AnyOpaqueJson(meta, innerJson)
    val result = facade.decodeAny(opaque)
    result match {
      case Right(decoded: my.ok.Inner) => assert(decoded == sampleInner)
      case Right(other)                => fail(s"decodeAny returned unexpected type: $other")
      case Left(e)                     => fail(s"decodeAny failed: $e")
    }
  }

  // ---------------------------------------------------------------------------------------------
  // 5. Forward-compat: trailing meta-extension bytes within the on-wire meta-length window
  // ---------------------------------------------------------------------------------------------

  test("forward-compat: extra meta-extension bytes inside meta-length window are skipped on UEBA decode") {
    // Strategy: encode a Holder normally, then surgically patch the FIRST any-field's
    // meta-length to claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder
    // must consume the meta, observe the gap (anyMetaLen - bytesRead), skipBytes(gap), and
    // continue parsing without error. PR 2.2's `readBinWithLength` + `skipBytes` path is tested
    // here in situ inside a real generated codec — narrower coverage of the same behaviour exists
    // in AnyMetaCodecSpec but not at the codec-emission layer.
    val original = buildUebaHolder()
    val bytes    = encodeUebaBytes(original, BaboonCodecContext.Compact)

    // Layout of the first any-field on the wire (Compact, useIndices=false):
    // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
    // header byte is at offset 0.
    val headerLen          = 1
    val anyLengthOffset    = headerLen           // 4 bytes
    val anyMetaLenOffset   = headerLen + 4       // 4 bytes
    val anyMetaStartOffset = headerLen + 4 + 4
    // Read the original anyLength + anyMetaLen.
    val origAnyLength = readI32LE(bytes, anyLengthOffset)
    val origAnyMetaLen = readI32LE(bytes, anyMetaLenOffset)

    val extension     = Array[Byte](0x11, 0x22, 0x33, 0x44, 0x55)
    val newAnyMetaLen = origAnyMetaLen + extension.length
    val newAnyLength  = origAnyLength + extension.length

    // Build a patched buffer:
    //   [header][i32 newAnyLength][i32 newAnyMetaLen][orig meta bytes][extension][blob][rest...]
    val origMetaSlice = bytes.slice(anyMetaStartOffset, anyMetaStartOffset + origAnyMetaLen)
    val origBlobAndRestStart = anyMetaStartOffset + origAnyMetaLen
    val origBlobAndRest      = bytes.slice(origBlobAndRestStart, bytes.length)

    val patched = new ByteArrayOutputStream()
    val pOut    = new LEDataOutputStream(patched)
    try {
      pOut.writeByte(bytes(0).toInt) // header
      pOut.writeInt(newAnyLength)
      pOut.writeInt(newAnyMetaLen)
      pOut.write(origMetaSlice)
      pOut.write(extension)
      pOut.write(origBlobAndRest)
    } finally {
      pOut.close()
    }
    val patchedBytes = patched.toByteArray

    // Decode must succeed and produce a Holder structurally equal to the original.
    val decoded = decodeUebaBytes(patchedBytes)
    assert(decoded == original, s"forward-compat decode diverged: $decoded vs $original")
  }

  private def readI32LE(bytes: Array[Byte], offset: Int): Int = {
    (bytes(offset) & 0xFF) |
    ((bytes(offset + 1) & 0xFF) << 8) |
    ((bytes(offset + 2) & 0xFF) << 16) |
    ((bytes(offset + 3) & 0xFF) << 24)
  }

  // ---------------------------------------------------------------------------------------------
  // 6. Misc: facade context plumbing
  // ---------------------------------------------------------------------------------------------

  test("encoding AnyOpaqueJson into UEBA without facade in ctx fails fast") {
    val mixed = buildUebaHolder().copy(
      fAny = AnyOpaqueJson(metaA, Json.obj("x" -> Json.fromInt(1)))
    )
    val baos = new ByteArrayOutputStream()
    val out  = new LEDataOutputStream(baos)
    val ex = intercept[BaboonCodecException] {
      try {
        my.ok.Holder_UEBACodec.encode(BaboonCodecContext.Compact, out, mixed)
      } finally {
        out.close()
      }
    }
    assert(ex.getMessage.contains("facade"), s"expected facade-mention error, got: ${ex.getMessage}")
  }

  test("encoding AnyOpaqueUeba into JSON without facade in ctx fails fast") {
    val mixed = buildJsonNativeHolder().copy(
      fAny = AnyOpaqueUeba(metaA, Array[Byte](1, 2))
    )
    val ex = intercept[BaboonCodecException] {
      val _ = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, mixed)
    }
    assert(ex.getMessage.contains("facade"), s"expected facade-mention error, got: ${ex.getMessage}")
  }

  // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?, $av?, $at?)
  // alongside the $c content key. Any change to the envelope that drops one of these would break
  // cross-language interop. Lock it in now (PR 2.4) so the wire spec can't drift silently.
  test("JSON envelope: encoded any-field carries $ak (+ optional $ad/$av/$at) and $c") {
    val original = buildJsonNativeHolder()
    val json     = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
    val obj      = json.asObject.getOrElse(fail("Holder JSON must be an object"))

    // fAny variant A → all four keys present.
    val anyField = obj("fAny").getOrElse(fail("missing fAny")).asObject.getOrElse(fail("fAny must be an object"))
    assert(anyField.contains("$ak"))
    assert(anyField.contains("$ad"))
    assert(anyField.contains("$av"))
    assert(anyField.contains("$at"))
    assert(anyField.contains("$c"))
    assert(anyField("$ak").contains(Json.fromInt(0x07)))

    // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
    val d3 = obj("fCurrentUnderlying").getOrElse(fail("missing fCurrentUnderlying")).asObject.getOrElse(fail("must be an object"))
    assert(d3.contains("$ak"))
    assert(d3.contains("$c"))
    assert(!d3.contains("$ad"))
    assert(!d3.contains("$av"))
    assert(!d3.contains("$at"))
    assert(d3("$ak").contains(Json.fromInt(0x00)))
  }

}

// NOTE: This test references generated symbols (fwde2e.fwd.*, fwde2e.chain.*) which are
// copied into this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/sc-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the UEBA envelope's writer-side forward policy
// (docs/forward-compat.md, "Envelope integration (UEBA)"): the v1 binary envelope has a
// single bound slot, `domainVersionMinCompat`. Under the default Strict policy it carries
// the byte-identical bound (unchanged behaviour); under Tolerant it carries the prefix bound
// for the chosen index mode, and a reader deployed before the writer's version decodes the
// payload with its newest codec.
package runtime

import baboon.runtime.shared._
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayInputStream

class ForwardCompatBinEnvelopeSpec extends AnyFunSuite {

  private val tolerantCompact: BaboonCodecContext = BaboonCodecContext.Custom(useIndices = false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, None)
  private val tolerantIndexed: BaboonCodecContext = BaboonCodecContext.Custom(useIndices = true, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, None)

  private def minCompatOf(bytes: Array[Byte]): String =
    BaboonTypeMeta.readMeta(new LEDataInputStream(new ByteArrayInputStream(bytes))).get.domainVersionMinCompat

  // a reader deployed before fwde2e.fwd 2.0.0 existed: knows 1.0.0 only
  private def oldFwdReader(): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(
      BaboonDomainVersion("fwde2e.fwd", "1.0.0"),
      fwde2e.fwd.v1_0_0.BaboonCodecsJson,
      fwde2e.fwd.v1_0_0.BaboonCodecsUeba,
      fwde2e.fwd.v1_0_0.BaboonMetadata,
    )
    f
  }

  private val fwdWriter: BaboonCodecsFacade = fwde2e.fwd.DomainFwde2eFwdFacade

  test("Strict (default) writer publishes the byte-identical bound: envelopes are unchanged and the old reader refuses") {
    assert(BaboonCodecContext.Compact.forwardWritePolicy == ForwardWritePolicy.Strict)
    assert(BaboonCodecContext.Indexed.forwardWritePolicy == ForwardWritePolicy.Strict)

    val appended = fwdWriter.encodeToBin(BaboonCodecContext.Compact, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    assert(minCompatOf(appended) == "2.0.0", "fresh field layout in 2.0.0: bound elided, equals the domain version")
    assert(oldFwdReader().decodeFromBin(appended).isLeft)

    val stable = fwdWriter.encodeToBin(BaboonCodecContext.Compact, fwde2e.fwd.FwdStable("same")).toTry.get
    assert(minCompatOf(stable) == "1.0.0")
    assert(oldFwdReader().decodeFromBin(stable).toTry.get == fwde2e.fwd.v1_0_0.FwdStable("same"))
  }

  test("Tolerant writer publishes the compact prefix bound and the old reader decodes with its own codec") {
    val appended = fwdWriter.encodeToBin(tolerantCompact, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    assert(minCompatOf(appended) == "1.0.0")
    assert(oldFwdReader().decodeFromBin(appended).toTry.get == fwde2e.fwd.v1_0_0.FwdAppendVar(42, "hello"))

    // json-additive only (mid-position insert): no UEBA prefix bound — the envelope is
    // byte-identical to the Strict one and the old reader still refuses
    val inserted       = fwde2e.fwd.FwdMidInsert(7, Some(99), "zed")
    val insertedBytes  = fwdWriter.encodeToBin(tolerantCompact, inserted).toTry.get
    assert(insertedBytes.sameElements(fwdWriter.encodeToBin(BaboonCodecContext.Compact, inserted).toTry.get))
    assert(oldFwdReader().decodeFromBin(insertedBytes).isLeft)

    // byte-identical type: the bound is 1.0.0 under both policies
    val stable = fwde2e.fwd.FwdStable("same")
    assert(fwdWriter.encodeToBin(tolerantCompact, stable).toTry.get.sameElements(fwdWriter.encodeToBin(BaboonCodecContext.Compact, stable).toTry.get))

    // grown enum: not forward-readable at all — unchanged envelope, refused
    val enumHost      = fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)
    val enumHostBytes = fwdWriter.encodeToBin(tolerantCompact, enumHost).toTry.get
    assert(enumHostBytes.sameElements(fwdWriter.encodeToBin(BaboonCodecContext.Compact, enumHost).toTry.get))
    assert(oldFwdReader().decodeFromBin(enumHostBytes).isLeft)
  }

  test("indexed payload: a variable-length appended field only earns prefix-compact, so no bound is lowered") {
    val appended = fwdWriter.encodeToBin(tolerantIndexed, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    assert(minCompatOf(appended) == "2.0.0")
    assert(oldFwdReader().decodeFromBin(appended).isLeft)
    // the writer's metadata distinguishes the two modes
    assert(fwde2e.fwd.FwdAppendVar.baboonMinReaderVersions(BaboonTypeMeta.UEBA_PREFIX_COMPACT_TIER) == "1.0.0")
    assert(fwde2e.fwd.FwdAppendVar.baboonMinReaderVersions(BaboonTypeMeta.UEBA_PREFIX_ANY_MODE_TIER) == "2.0.0")
  }

  test("three-version chain: the Tolerant bound is the chain minimum and each older reader decodes with its newest codec") {
    val value = fwde2e.chain.ChainAppend(1, Some("b"), Some("c"))
    val bytes = chainWriter.encodeToBin(tolerantCompact, value).toTry.get
    assert(minCompatOf(bytes) == "1.0.0")
    assert(midReader().decodeFromBin(bytes).toTry.get == fwde2e.chain.v2_0_0.ChainAppend(1, Some("b")))

    val v1Only = new BaboonCodecsFacade {}
    v1Only.register(
      BaboonDomainVersion("fwde2e.chain", "1.0.0"),
      fwde2e.chain.v1_0_0.BaboonCodecsJson,
      fwde2e.chain.v1_0_0.BaboonCodecsUeba,
      fwde2e.chain.v1_0_0.BaboonMetadata,
    )
    assert(v1Only.decodeFromBin(bytes).toTry.get == fwde2e.chain.v1_0_0.ChainAppend(1))
  }

  // a reader deployed before fwde2e.chain 3.0.0 existed: knows 1.0.0 and 2.0.0
  private def midReader(): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(
      BaboonDomainVersion("fwde2e.chain", "1.0.0"),
      fwde2e.chain.v1_0_0.BaboonCodecsJson,
      fwde2e.chain.v1_0_0.BaboonCodecsUeba,
      fwde2e.chain.v1_0_0.BaboonMetadata,
    )
    f.register(
      BaboonDomainVersion("fwde2e.chain", "2.0.0"),
      fwde2e.chain.v2_0_0.BaboonCodecsJson,
      fwde2e.chain.v2_0_0.BaboonCodecsUeba,
      fwde2e.chain.v2_0_0.BaboonMetadata,
    )
    f
  }

  private val chainWriter: BaboonCodecsFacade = fwde2e.chain.DomainFwde2eChainFacade

  test("reader decodes a newer prefix-readable payload with its newest codec, not the bound version's") {
    val value = fwde2e.chain.ChainAppend(1, Some("b"), Some("c"))
    // a 3.0.0 envelope whose minCompat names 1.0.0 — what the Tolerant writer policy publishes
    val meta    = BaboonTypeMeta.from(value).copy(domainVersionMinCompat = "1.0.0")
    val bytes   = chainWriter.encodeToBin(BaboonCodecContext.Compact, value, Some(meta)).toTry.get
    val decoded = midReader().decodeFromBin(bytes).toTry.get
    // 2.0.0 is prefix-readable too (readability is monotone along the chain), so `b` must survive
    assert(decoded == fwde2e.chain.v2_0_0.ChainAppend(1, Some("b")))
  }
}

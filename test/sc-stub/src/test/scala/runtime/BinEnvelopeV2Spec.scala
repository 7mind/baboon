// NOTE: This test references generated symbols (fwde2e.fwd.*, fwde2e.chain.*) which are
// copied into this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/sc-stub/). Run from the codegen'd copy.
//
// Binary envelope metaVersion 2 (docs/spec/codec-envelope.md §2.1.3): the JSON-equivalent
// layout carrying BOTH bounds — `domainVersionMinCompat` (byte-identical) and
// `domainVersionReadableMin` (prefix bound for the payload's index mode) — behind a flags byte.
// Writers stay on v1 unless the context selects v2; readers accept both.
package runtime

import baboon.runtime.shared._
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class BinEnvelopeV2Spec extends AnyFunSuite {

  // a reader deployed before fwde2e.fwd 2.0.0 existed: knows 1.0.0 only
  private def oldFwdReader(policy: ForwardReadPolicy): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(
      BaboonDomainVersion("fwde2e.fwd", "1.0.0"),
      fwde2e.fwd.v1_0_0.BaboonCodecsJson,
      fwde2e.fwd.v1_0_0.BaboonCodecsUeba,
      fwde2e.fwd.v1_0_0.BaboonMetadata,
    )
    f.forwardReadPolicy = policy
    f
  }

  private def bytesOf(write: LEDataOutputStream => Unit): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val dos  = new LEDataOutputStream(baos)
    write(dos)
    baos.toByteArray
  }

  private def readMeta(bytes: Array[Byte]): Option[BaboonTypeMeta] =
    BaboonTypeMeta.readMeta(new LEDataInputStream(new ByteArrayInputStream(bytes)))

  // Hand-assembled v2 envelope: 02 | domainId | domainVersion | flags | [minCompat] | [readableMin] | typeId | payload
  private def v2Envelope(flags: Int, minCompat: Option[String], readableMin: Option[String], typeId: String, payload: Array[Byte]): Array[Byte] =
    bytesOf { dos =>
      dos.write(2)
      BaboonBinTools.writeString(dos, "fwde2e.fwd")
      BaboonBinTools.writeString(dos, "2.0.0")
      dos.write(flags)
      minCompat.foreach(v => BaboonBinTools.writeString(dos, v))
      readableMin.foreach(v => BaboonBinTools.writeString(dos, v))
      BaboonBinTools.writeString(dos, typeId)
      dos.write(payload)
    }

  private val appended        = fwde2e.fwd.FwdAppendVar(42, "hi", Some("t"))
  private val appendedPayload = bytesOf(dos => fwde2e.fwd.FwdAppendVar_UEBACodec.instance.encode(BaboonCodecContext.Compact, dos, appended))

  private val fwdWriter: BaboonCodecsFacade   = fwde2e.fwd.DomainFwde2eFwdFacade
  private val chainWriter: BaboonCodecsFacade = fwde2e.chain.DomainFwde2eChainFacade
  private val v2Compact: BaboonCodecContext   = BaboonCodecContext.Custom(useIndices = false, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, None)
  private val v2Indexed: BaboonCodecContext   = BaboonCodecContext.Custom(useIndices = true, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, None)

  private def midChainReader(policy: ForwardReadPolicy): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(BaboonDomainVersion("fwde2e.chain", "1.0.0"), fwde2e.chain.v1_0_0.BaboonCodecsJson, fwde2e.chain.v1_0_0.BaboonCodecsUeba, fwde2e.chain.v1_0_0.BaboonMetadata)
    f.register(BaboonDomainVersion("fwde2e.chain", "2.0.0"), fwde2e.chain.v2_0_0.BaboonCodecsJson, fwde2e.chain.v2_0_0.BaboonCodecsUeba, fwde2e.chain.v2_0_0.BaboonMetadata)
    f.forwardReadPolicy = policy
    f
  }

  // the header shared by every fwde2e.fwd 2.0.0 envelope, then the layout-specific bytes
  private def fwdHead(metaVersion: Int): Array[Byte] = bytesOf { dos =>
    dos.write(metaVersion); BaboonBinTools.writeString(dos, "fwde2e.fwd"); BaboonBinTools.writeString(dos, "2.0.0")
  }
  private def str(s: String): Array[Byte] = bytesOf(dos => BaboonBinTools.writeString(dos, s))

  test("the default context still writes v1: envelopes are byte-identical to before") {
    assert(BaboonCodecContext.Compact.envelopeVersion == BaboonEnvelopeVersion.V1)
    assert(BaboonCodecContext.Indexed.envelopeVersion == BaboonEnvelopeVersion.V1)
    val env = fwdWriter.encodeToBin(BaboonCodecContext.Compact, appended).toTry.get
    assert(env.head == 1.toByte)
    assert(env.sameElements(fwdHead(1) ++ Array[Byte](0) ++ str(fwde2e.fwd.FwdAppendVar.baboonTypeIdentifier) ++ appendedPayload))
  }

  test("v2 writer publishes both bounds with JSON elision rules, per type") {
    // appended var-len field: identical bound elided (= 2.0.0), prefix-compact bound 1.0.0 -> flags 0b10
    val app = fwdWriter.encodeToBin(v2Compact, appended).toTry.get
    assert(app.sameElements(fwdHead(2) ++ Array[Byte](0x02) ++ str("1.0.0") ++ str(fwde2e.fwd.FwdAppendVar.baboonTypeIdentifier) ++ appendedPayload))
    val appMeta = readMeta(app).get
    assert(appMeta.metaVersion == 2.toByte && appMeta.domainVersionMinCompat == "2.0.0" && appMeta.domainVersionReadableMin == "1.0.0")

    // indexed: prefix-any-mode bound is 2.0.0 -> nothing to publish, flags 0
    val appIdx = fwdWriter.encodeToBin(v2Indexed, appended).toTry.get
    assert(appIdx.startsWith(fwdHead(2) ++ Array[Byte](0x00) ++ str(fwde2e.fwd.FwdAppendVar.baboonTypeIdentifier)))

    // byte-identical since 1.0.0: minCompat 1.0.0, readableMin equal to it and elided -> flags 0b01
    val stable = fwdWriter.encodeToBin(v2Compact, fwde2e.fwd.FwdStable("s")).toTry.get
    assert(stable.startsWith(fwdHead(2) ++ Array[Byte](0x01) ++ str("1.0.0") ++ str(fwde2e.fwd.FwdStable.baboonTypeIdentifier)))

    // json-additive only / not forward-readable: no bound at all -> flags 0
    val mid  = fwdWriter.encodeToBin(v2Compact, fwde2e.fwd.FwdMidInsert(7, Some(99), "z")).toTry.get
    val enum = fwdWriter.encodeToBin(v2Compact, fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)).toTry.get
    assert(mid.startsWith(fwdHead(2) ++ Array[Byte](0x00) ++ str(fwde2e.fwd.FwdMidInsert.baboonTypeIdentifier)))
    assert(enum.startsWith(fwdHead(2) ++ Array[Byte](0x00) ++ str(fwde2e.fwd.FwdEnumHost.baboonTypeIdentifier)))

    // under v2 the writer policy is irrelevant: both bounds travel anyway
    val v2Tolerant = BaboonCodecContext.Custom(useIndices = false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V2, None)
    assert(fwdWriter.encodeToBin(v2Tolerant, appended).toTry.get.sameElements(app))
  }

  test("v2 round-trips through the writer's own facade and through old readers under each policy") {
    val app = fwdWriter.encodeToBin(v2Compact, appended).toTry.get
    assert(fwdWriter.decodeFromBin(app).toTry.get == appended)
    assert(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(app).toTry.get == fwde2e.fwd.v1_0_0.FwdAppendVar(42, "hi"))
    assert(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(app).isLeft)

    val stable = fwdWriter.encodeToBin(v2Compact, fwde2e.fwd.FwdStable("s")).toTry.get
    assert(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(stable).toTry.get == fwde2e.fwd.v1_0_0.FwdStable("s"))
    assert(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(stable).toTry.get == fwde2e.fwd.v1_0_0.FwdStable("s"))

    val enum = fwdWriter.encodeToBin(v2Compact, fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)).toTry.get
    assert(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(enum).isLeft)
  }

  test("three-version chain in v2: Lossless is enforceable for binary, Tolerant decodes with the newest codec") {
    val value = fwde2e.chain.ChainAppend(1, Some("b"), Some("c"))
    val env   = chainWriter.encodeToBin(v2Compact, value).toTry.get
    val meta  = readMeta(env).get
    assert(meta.domainVersionMinCompat == "3.0.0" && meta.domainVersionReadableMin == "1.0.0")
    assert(midChainReader(ForwardReadPolicy.Tolerant).decodeFromBin(env).toTry.get == fwde2e.chain.v2_0_0.ChainAppend(1, Some("b")))
    assert(midChainReader(ForwardReadPolicy.Lossless).decodeFromBin(env).isLeft, "v2 carries the identical bound separately, so Lossless can refuse")
  }

  test("v2 readMeta rejects unknown flag bits; unknown metaVersions stay rejected") {
    assert(readMeta(v2Envelope(0x04, None, None, "T", Array.empty)).isEmpty)
    assert(readMeta(v2Envelope(0x03, Some("1.5.0"), Some("1.0.0"), "T", Array.empty)).map(m => (m.domainVersionMinCompat, m.domainVersionReadableMin)).contains(("1.5.0", "1.0.0")))
    assert(readMeta(bytesOf(dos => { dos.write(3); BaboonBinTools.writeString(dos, "d"); BaboonBinTools.writeString(dos, "1.0.0") })).isEmpty)
  }

  test("reader accepts a v2 envelope: readable-min lets a Tolerant old reader decode, Lossless refuses") {
    // identical bound elided (= 2.0.0), prefix-compact bound 1.0.0 present: flags = 0b10
    val env = v2Envelope(0x02, None, Some("1.0.0"), fwde2e.fwd.FwdAppendVar.baboonTypeIdentifier, appendedPayload)
    val meta = readMeta(env).getOrElse(fail("v2 envelope must be readable"))
    assert(meta.metaVersion == 2.toByte)
    assert(meta.domainVersionMinCompat == "2.0.0")
    assert(meta.domainVersionReadableMin == "1.0.0")

    assert(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(env).toTry.get == fwde2e.fwd.v1_0_0.FwdAppendVar(42, "hi"))
    assert(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(env).isLeft, "Lossless must not honour the readable-min bound")
  }
}

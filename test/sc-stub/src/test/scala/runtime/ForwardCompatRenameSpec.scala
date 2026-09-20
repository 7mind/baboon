// NOTE: This test references generated symbols (fwde2e.rename.*) which are copied into
// this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/sc-stub/). Run from the codegen'd copy.
//
// Per-format forward readability (docs/forward-compat.md, "Per-format readability").
// A declared field rename is the one change where the two wire formats disagree in
// opposite directions: UEBA identifies a field by position and never writes its name,
// so the bytes do not move; JSON identifies it by name, so the old key is simply gone.
package runtime

import baboon.runtime.shared._
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream

class ForwardCompatRenameSpec extends AnyFunSuite {

  private val writer: BaboonCodecsFacade = fwde2e.rename.DomainFwde2eRenameFacade

  // a reader deployed before the rename: knows fwde2e.rename 1.0.0 only
  private def oldReader(policy: ForwardReadPolicy): BaboonCodecsFacade = {
    val f = new BaboonCodecsFacade {}
    f.register(
      BaboonDomainVersion("fwde2e.rename", "1.0.0"),
      fwde2e.rename.v1_0_0.BaboonCodecsJson,
      fwde2e.rename.v1_0_0.BaboonCodecsUeba,
      fwde2e.rename.v1_0_0.BaboonMetadata,
    )
    f.forwardReadPolicy = policy
    f
  }

  private val v1Tolerant = BaboonCodecContext.Custom(useIndices = false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, None)
  private val v2Compact  = BaboonCodecContext.Custom(useIndices = false, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, None)

  private val current = fwde2e.rename.RenamedHolder(42, "hi")
  private val legacy  = fwde2e.rename.v1_0_0.RenamedHolder(42, "hi")

  private def bytesOf(write: LEDataOutputStream => Unit): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    write(new LEDataOutputStream(baos))
    baos.toByteArray
  }

  test("the rename leaves the UEBA payload byte-identical and moves the JSON key") {
    // The 1.0.0 codec ships without an encoder (deprecated encoders are off by
    // default), so the byte claim is pinned against the layout 1.0.0 defines:
    // mode byte, i32 `a`, then the length-prefixed str that 1.0.0 calls `b`.
    val expected = Array[Byte](0x00, 0x2A, 0x00, 0x00, 0x00, 0x02, 0x68, 0x69)
    val payload  = bytesOf(dos => fwde2e.rename.RenamedHolder_UEBACodec.instance.encode(BaboonCodecContext.Compact, dos, current))
    assert(payload.sameElements(expected), "UEBA never puts a field name on the wire")

    val newJson = fwde2e.rename.RenamedHolder_JsonCodec.instance.encode(BaboonCodecContext.Compact, current)
    assert(newJson.hcursor.downField("r").as[String].toOption.contains("hi"))
    assert(newJson.hcursor.downField("b").focus.isEmpty, "the old JSON key is gone")
  }

  test("the writer publishes the UEBA bounds across the rename and withholds the JSON one") {
    val bounds = fwde2e.rename.RenamedHolder.baboonMinReaderVersions
    assert(bounds("prefix-compact") == "1.0.0")
    assert(bounds("prefix-any-mode") == "1.0.0")
    assert(bounds("json-additive") == "2.0.0")
    assert(bounds("identical") == "2.0.0")
    // the byte-identical bound is unchanged: a rename is not byte-identical in both formats
    assert(fwde2e.rename.RenamedHolder.baboonSameInVersions == List("2.0.0"))
    assert(fwde2e.rename.v1_0_0.RenamedHolder.baboonForwardReadable == Map("1.0.0" -> "identical", "2.0.0" -> "ueba-identical"))
  }

  test("a 1.0.0 codec decodes 2.0.0 UEBA bytes for the renamed field, under v1 and under v2") {
    val v1env = writer.encodeToBin(v1Tolerant, current).toTry.get
    assert(oldReader(ForwardReadPolicy.Tolerant).decodeFromBin(v1env).toTry.get == legacy)

    val v2env = writer.encodeToBin(v2Compact, current).toTry.get
    assert(oldReader(ForwardReadPolicy.Tolerant).decodeFromBin(v2env).toTry.get == legacy)
    // v2 carries the byte-identical bound separately, so a Lossless reader still refuses
    assert(oldReader(ForwardReadPolicy.Lossless).decodeFromBin(v2env).isLeft)
  }

  test("the default Strict v1 writer publishes no lowered bound, so the old reader refuses") {
    val strict = writer.encodeToBin(BaboonCodecContext.Compact, current).toTry.get
    assert(oldReader(ForwardReadPolicy.Tolerant).decodeFromBin(strict).isLeft)
  }

  test("JSON is not readable across the rename under either policy") {
    val json = writer.encodeToJson(current).toTry.get
    assert(oldReader(ForwardReadPolicy.Tolerant).decodeFromJson(json).toTry.get.isEmpty)
    assert(oldReader(ForwardReadPolicy.Lossless).decodeFromJson(json).toTry.get.isEmpty)
  }
}

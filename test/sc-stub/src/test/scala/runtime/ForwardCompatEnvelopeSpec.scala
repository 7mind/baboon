// NOTE: This test references generated symbols (fwde2e.fwd.*, fwde2e.fwd.v1_0_0.*)
// which are copied into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the JSON envelope's `$rv` (readable-min) bound
// (docs/forward-compat.md, "Envelope integration"): a facade that registers ONLY
// the old 1.0.0 domain version decodes envelopes produced by the 2.0.0 facade
// exactly where `$rv` says it can, under the Tolerant policy — and refuses under
// Lossless or when no `$rv` was published.
package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

@scala.annotation.nowarn("cat=deprecation")
class ForwardCompatEnvelopeSpec extends AnyFunSuite {

  // a reader deployed before 2.0.0 existed: knows fwde2e.fwd 1.0.0 only
  private def oldReader(policy: ForwardReadPolicy): BaboonCodecsFacade = {
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

  private val writer: BaboonCodecsFacade = fwde2e.fwd.DomainFwde2eFwdFacade

  private def field(json: Json, key: String): Option[String] = json.hcursor.downField(key).as[String].toOption

  test("writer publishes $rv only where the json-additive bound is older than the identical bound") {
    val appended = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    assert(field(appended, "$v").contains("2.0.0"))
    assert(field(appended, "$uv").isEmpty, "identical-min equals $v and is elided")
    assert(field(appended, "$rv").contains("1.0.0"))

    val inserted = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdMidInsert(7, Some(99), "zed")).toTry.get
    assert(field(inserted, "$rv").contains("1.0.0"))

    // unchanged type: byte-identical back to 1.0.0, so $uv carries the bound and $rv is elided
    val stable = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdStable("same")).toTry.get
    assert(field(stable, "$uv").contains("1.0.0"))
    assert(field(stable, "$rv").isEmpty)

    // grown enum: not forward-readable at all, no bounds published
    val enumHost = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)).toTry.get
    assert(field(enumHost, "$uv").isEmpty)
    assert(field(enumHost, "$rv").isEmpty)

    // generated metadata backing the envelope: identical bound == sameIn head
    assert(fwde2e.fwd.FwdAppendVar.baboonMinReaderVersions("identical") == fwde2e.fwd.FwdAppendVar.baboonSameInVersions.head)
    assert(fwde2e.fwd.FwdAppendVar.baboonMinReaderVersions("json-additive") == "1.0.0")
  }

  test("Tolerant old reader decodes newer json-additive payloads with its own codec, dropping unknown fields") {
    val reader   = oldReader(ForwardReadPolicy.Tolerant)
    val appended = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    val decoded  = reader.decodeFromJson(appended).toTry.get
    assert(decoded.contains(fwde2e.fwd.v1_0_0.FwdAppendVar(42, "hello")))

    val inserted = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdMidInsert(7, Some(99), "zed")).toTry.get
    assert(reader.decodeFromJson(inserted).toTry.get.contains(fwde2e.fwd.v1_0_0.FwdMidInsert(7, "zed")))

    // byte-identical types keep working as before ($uv path)
    val stable = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdStable("same")).toTry.get
    assert(reader.decodeFromJson(stable).toTry.get.contains(fwde2e.fwd.v1_0_0.FwdStable("same")))
  }

  test("Lossless old reader refuses newer payloads unless byte-identical") {
    val reader   = oldReader(ForwardReadPolicy.Lossless)
    val appended = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))).toTry.get
    // the facade reports "no codec resolves" as Right(None) (Option contract), not Left
    assert(reader.decodeFromJson(appended).toTry.get.isEmpty, "Lossless must not honour $rv")

    val stable = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdStable("same")).toTry.get
    assert(reader.decodeFromJson(stable).toTry.get.contains(fwde2e.fwd.v1_0_0.FwdStable("same")))
  }

  test("no published bound: the old reader refuses under either policy") {
    val enumHost = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)).toTry.get
    assert(oldReader(ForwardReadPolicy.Tolerant).decodeFromJson(enumHost).toTry.get.isEmpty)
    assert(oldReader(ForwardReadPolicy.Lossless).decodeFromJson(enumHost).toTry.get.isEmpty)
  }

  test("envelope round-trips $rv through readMeta") {
    val appended = writer.encodeToJson(BaboonCodecContext.Compact, fwde2e.fwd.FwdAppendVar(1, "b", Some("t"))).toTry.get
    val meta     = BaboonTypeMeta.readMeta(appended).get
    assert(meta.domainVersion == "2.0.0")
    assert(meta.domainVersionMinCompat == "2.0.0")
    assert(meta.domainVersionReadableMin == "1.0.0")
    assert(meta.versionReadableMin.contains(BaboonDomainVersion("fwde2e.fwd", "1.0.0")))
    assert(meta.versionMinCompat.isEmpty)
    // and the five-field constructor keeps readable-min = minCompat (pre-$rv envelopes)
    val legacy = BaboonTypeMeta(1.toByte, "fwde2e.fwd", "2.0.0", "1.5.0", "T")
    assert(legacy.domainVersionReadableMin == "1.5.0")
  }
}

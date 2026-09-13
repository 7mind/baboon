// NOTE: This test references generated symbols (fwde2e.fwd.*, fwde2e.fwd.v1_0_0.*)
// which are copied into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the forward-compatibility metadata
// (docs/drafts/20260911-0937-forward-compat-metadata.md): a codec generated for
// an OLDER version decodes blobs produced by a NEWER version's codec, exactly
// where the emitted `baboonForwardReadable` metadata promises it.
package runtime

import baboon.runtime.shared._
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

@scala.annotation.nowarn("cat=deprecation")
class ForwardCompatSpec extends AnyFunSuite {
  private val ctx = BaboonCodecContext.Compact

  private def uebaBytes[T](encode: (BaboonCodecContext, LEDataOutputStream, T) => Unit, value: T): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val dos  = new LEDataOutputStream(baos)
    encode(ctx, dos, value)
    baos.toByteArray
  }

  test("emitted forward metadata carries the expected tiers") {
    assert(fwde2e.fwd.v1_0_0.FwdAppendVar.baboonForwardReadable == Map("1.0.0" -> "identical", "2.0.0" -> "prefix-compact"))
    assert(fwde2e.fwd.v1_0_0.FwdMidInsert.baboonForwardReadable == Map("1.0.0" -> "identical", "2.0.0" -> "json-additive"))
    assert(fwde2e.fwd.v1_0_0.FwdStable.baboonForwardReadable == Map("1.0.0" -> "identical", "2.0.0" -> "identical"))
    assert(fwde2e.fwd.v1_0_0.FwdEnumHost.baboonForwardReadable == Map("1.0.0" -> "identical"))

    // latest version: no successors yet
    assert(fwde2e.fwd.FwdAppendVar.baboonForwardReadable == Map("2.0.0" -> "identical"))

    // domain-wide metadata object agrees with the per-type constants
    val meta = fwde2e.fwd.v1_0_0.BaboonMetadata
    assert(meta.forwardReadableVersions(fwde2e.fwd.v1_0_0.FwdAppendVar.baboonTypeIdentifier) == fwde2e.fwd.v1_0_0.FwdAppendVar.baboonForwardReadable)
  }

  test("PREFIX_COMPACT: old UEBA codec prefix-reads a compact blob with appended fields") {
    val value = fwde2e.fwd.FwdAppendVar(42, "hello", Some("appended"))
    val bytes = uebaBytes[fwde2e.fwd.FwdAppendVar](fwde2e.fwd.FwdAppendVar_UEBACodec.instance.encode, value)

    val bais    = new ByteArrayInputStream(bytes)
    val dis     = new LEDataInputStream(bais)
    val decoded = fwde2e.fwd.v1_0_0.FwdAppendVar_UEBACodec.instance.decode(ctx, dis).toTry.get

    assert(decoded.a == 42)
    assert(decoded.b == "hello")
    // prefix semantics: the appended field's bytes are left unconsumed
    assert(bais.available() > 0, "appended-field bytes must remain after the prefix read")
  }

  test("JSON_ADDITIVE: old JSON codec reads mid-inserted and appended fields at any position") {
    val inserted     = fwde2e.fwd.FwdMidInsert(7, Some(99), "zed")
    val insertedJson = fwde2e.fwd.FwdMidInsert_JsonCodec.instance.encode(ctx, inserted)
    val decMid       = fwde2e.fwd.v1_0_0.FwdMidInsert_JsonCodec.instance.decode(ctx, insertedJson).toTry.get
    assert(decMid.a == 7)
    assert(decMid.z == "zed")

    val appended     = fwde2e.fwd.FwdAppendVar(1, "b", Some("t"))
    val appendedJson = fwde2e.fwd.FwdAppendVar_JsonCodec.instance.encode(ctx, appended)
    val decApp       = fwde2e.fwd.v1_0_0.FwdAppendVar_JsonCodec.instance.decode(ctx, appendedJson).toTry.get
    assert(decApp.a == 1)
    assert(decApp.b == "b")
  }

  test("JSON_ADDITIVE does NOT extend to UEBA: mid-inserted field desyncs the positional read") {
    val value = fwde2e.fwd.FwdMidInsert(7, Some(99), "zed")
    val bytes = uebaBytes[fwde2e.fwd.FwdMidInsert](fwde2e.fwd.FwdMidInsert_UEBACodec.instance.encode, value)

    val bais   = new ByteArrayInputStream(bytes)
    val dis    = new LEDataInputStream(bais)
    val result = scala.util.Try(fwde2e.fwd.v1_0_0.FwdMidInsert_UEBACodec.instance.decode(ctx, dis).toTry.get)
    // the old codec reads the inserted opt bytes as the `z` string: garbage or failure, never the real value
    assert(result.isFailure || result.get.z != "zed")
  }

  test("IDENTICAL: unchanged type round-trips through the old codec byte-identically") {
    val value = fwde2e.fwd.FwdStable("same")
    val bytes = uebaBytes[fwde2e.fwd.FwdStable](fwde2e.fwd.FwdStable_UEBACodec.instance.encode, value)

    val bais    = new ByteArrayInputStream(bytes)
    val dis     = new LEDataInputStream(bais)
    val decoded = fwde2e.fwd.v1_0_0.FwdStable_UEBACodec.instance.decode(ctx, dis).toTry.get
    assert(decoded.s == "same")
    assert(bais.available() == 0, "identical type must consume the blob exactly")

    val json = fwde2e.fwd.FwdStable_JsonCodec.instance.encode(ctx, value)
    assert(fwde2e.fwd.v1_0_0.FwdStable_JsonCodec.instance.decode(ctx, json).toTry.get.s == "same")
  }

  test("negative control: a grown enum value is NOT readable by the old codec (metadata promises nothing)") {
    val value = fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C)

    val json    = fwde2e.fwd.FwdEnumHost_JsonCodec.instance.encode(ctx, value)
    val jsonDec = fwde2e.fwd.v1_0_0.FwdEnumHost_JsonCodec.instance.decode(ctx, json)
    assert(jsonDec.isLeft, "old JSON codec must reject the unknown enum member")

    val bytes   = uebaBytes[fwde2e.fwd.FwdEnumHost](fwde2e.fwd.FwdEnumHost_UEBACodec.instance.encode, value)
    val bais    = new ByteArrayInputStream(bytes)
    val dis     = new LEDataInputStream(bais)
    val uebaDec = scala.util.Try(fwde2e.fwd.v1_0_0.FwdEnumHost_UEBACodec.instance.decode(ctx, dis).toTry.get)
    assert(uebaDec.isFailure, "old UEBA codec must reject the unknown enum discriminant")
  }
}

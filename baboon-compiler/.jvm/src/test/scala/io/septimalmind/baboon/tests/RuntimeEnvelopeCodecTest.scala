package io.septimalmind.baboon.tests

import io.circe.Json
import io.circe.parser.parse
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, IssuePrinter}
import io.septimalmind.baboon.typer.{BaboonRuntimeEnvelopeCodec, BinEnvelopeOptions, BinEnvelopeVersion}
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

/** `BaboonRuntimeEnvelopeCodec`: JSON <-> UEBA conversion of top-level envelopes over the interpreter.
  *
  * The binary vectors are the cross-language goldens of docs/spec/codec-envelope.md § 2.1.4, pinned in
  * every backend (`BinEnvelopeGoldenTests.cs` and siblings), so agreement here is agreement with the
  * generated runtimes.
  */
final class RuntimeEnvelopeCodecTest extends BaboonTest[Either] {
  import RuntimeEnvelopeCodecTest.*

  private val V1Compact = BinEnvelopeOptions(BinEnvelopeVersion.V1, indexed = false)
  private val V1Indexed = BinEnvelopeOptions(BinEnvelopeVersion.V1, indexed = true)
  private val V2Compact = BinEnvelopeOptions(BinEnvelopeVersion.V2, indexed = false)
  private val V2Indexed = BinEnvelopeOptions(BinEnvelopeVersion.V2, indexed = true)

  private val AppendVarJson =
    """{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdAppendVar","$rv":"1.0.0","$c":{"a":42,"b":"hi","t":"t"}}"""
  private val StableJson   = """{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdStable","$uv":"1.0.0","$c":{"s":"s"}}"""
  private val EnumHostJson = """{"$mv":1,"$d":"fwde2e.fwd","$v":"2.0.0","$t":"fwde2e.fwd/:#FwdEnumHost","$c":{"e":"C"}}"""
  private val ChainJson =
    """{"$mv":1,"$d":"fwde2e.chain","$v":"3.0.0","$t":"fwde2e.chain/:#ChainAppend","$rv":"1.0.0","$c":{"a":1,"b":"b","c":"c"}}"""

  private val AppendVarV1Strict =
    "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"
  private val AppendVarV1Tolerant =
    "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"
  private val AppendVarV2Compact =
    "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"
  private val AppendVarV2Indexed =
    "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74"
  private val StableV1 =
    "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"
  private val StableV2 =
    "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"
  private val EnumHostV2 =
    "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02"
  private val ChainV2 =
    "02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63"

  private def load(loader: BaboonLoader[Either]): BaboonFamily = {
    val roots = List("baboon/fwd-e2e-ok", "baboon/fwd-e2e-chain-ok", "scheme-zip-ok/shapes")
    val files = roots.flatMap {
      root =>
        val path = IzResources.getPath(root).get.asInstanceOf[IzResources.LoadablePathReference].path
        IzFiles.walk(path.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    }
    loader.load(files).fold(issues => fail(s"fixture failed to load: $issues"), identity)
  }

  private def toUeba(codec: BaboonRuntimeEnvelopeCodec[Either], family: BaboonFamily, json: String, options: BinEnvelopeOptions): Either[BaboonIssue, String] = {
    codec.jsonToUeba(family, parse(json).fold(e => fail(s"bad test JSON: $e"), identity), options).map(bytes => hex(bytes))
  }

  private def toJson(codec: BaboonRuntimeEnvelopeCodec[Either], family: BaboonFamily, bytes: String): Either[BaboonIssue, String] = {
    codec.uebaToJson(family, unhex(bytes)).map(_.noSpaces)
  }

  private def rejects(result: Either[BaboonIssue, Any], fragment: String): Unit = {
    result match {
      case Right(value) => fail(s"expected a failure mentioning '$fragment', got $value")
      case Left(issue) =>
        val message = IssuePrinter[BaboonIssue].stringify(issue)
        assert(message.contains(fragment), s"'$message' does not mention '$fragment'")
    }
  }

  "envelope conversion" should {
    "write the cross-language golden bytes from JSON envelopes" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family = load(loader)
        assert(toUeba(codec, family, AppendVarJson, V1Compact) == Right(AppendVarV1Strict))
        assert(toUeba(codec, family, AppendVarJson, V2Compact) == Right(AppendVarV2Compact))
        // indexed: the appended field is variable-length, so the prefix-any-mode bound is 2.0.0 and elided
        assert(toUeba(codec, family, AppendVarJson, V2Indexed) == Right(AppendVarV2Indexed))
        assert(toUeba(codec, family, StableJson, V1Compact) == Right(StableV1))
        assert(toUeba(codec, family, StableJson, V2Compact) == Right(StableV2))
        assert(toUeba(codec, family, EnumHostJson, V2Compact) == Right(EnumHostV2))
        assert(toUeba(codec, family, ChainJson, V2Compact) == Right(ChainV2))
    }

    "read the golden bytes back into JSON envelopes with JSON-specific bounds" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family = load(loader)
        assert(toJson(codec, family, AppendVarV1Strict) == Right(AppendVarJson))
        assert(toJson(codec, family, AppendVarV2Compact) == Right(AppendVarJson))
        assert(toJson(codec, family, AppendVarV2Indexed) == Right(AppendVarJson))
        assert(toJson(codec, family, StableV1) == Right(StableJson))
        assert(toJson(codec, family, StableV2) == Right(StableJson))
        assert(toJson(codec, family, EnumHostV2) == Right(EnumHostJson))
        assert(toJson(codec, family, ChainV2) == Right(ChainJson))
        // a v1 Tolerant slot carries the prefix bound; the JSON envelope publishes the json-additive bound
        // and the byte-identical $uv recomputed from the model, not the copied binary bound
        assert(toJson(codec, family, AppendVarV1Tolerant) == Right(AppendVarJson))
    }

    "recompute bounds per format instead of copying them" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family = load(loader)
        // Order 2.0.0 renames a field: UEBA bytes are unchanged since 1.0.0, the JSON key is not
        val order =
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Order","$c":{"id":"6c0e3e4e-2d53-4c2a-9a39-5d3f1b0f7a11","colour":"Green","price":"1234567890123","tags":{"k":"v"},"at":{"x":1,"y":2},"shape":{"Square":{"side":3}},"label":null}}"""
        val v2   = codec.jsonToUeba(family, parse(order).toOption.get, V2Compact).fold(e => fail(e.toString), identity)
        val meta = new baboon.runtime.shared.LEDataInputStream(new java.io.ByteArrayInputStream(v2.toArray))
        val read = baboon.runtime.shared.BaboonTypeMetaCodec.readMeta(meta).get
        assert(read.domainVersionMinCompat == "2.0.0")
        assert(read.domainVersionReadableMin == "1.0.0")
        assert(toJson(codec, family, hex(v2)) == Right(order))
    }

    "round-trip DTO, enum, ADT, ADT branch, namespaced and foreign-carrying envelopes" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family = load(loader)
        val envelopes = List(
          // DTO with enum, rt-mapped foreign (i64 as decimal string), no-rt foreign map key, namespaced and ADT fields
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#Order","$c":{"id":"6c0e3e4e-2d53-4c2a-9a39-5d3f1b0f7a11","color":"Red","price":"-9007199254740993","tags":{"opaque key":"v","":"empty"},"at":{"x":-1,"y":2147483647},"shape":{"Circle":{"r":1.5}},"label":"x"}}""",
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Color","$uv":"1.0.0","$c":"Green"}""",
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/:#Shape","$uv":"1.0.0","$c":{"Circle":{"r":0.25}}}""",
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/[zipdemo.shapes/:#Shape]#Square","$c":{"side":7}}""",
          """{"$mv":1,"$d":"zipdemo.shapes","$v":"2.0.0","$t":"zipdemo.shapes/geo#Point","$uv":"1.0.0","$c":{"x":3,"y":4}}""",
        )
        for {
          envelope <- envelopes
          options  <- List(V1Compact, V1Indexed, V2Compact, V2Indexed)
        } {
          val bytes = toUeba(codec, family, envelope, options).fold(e => fail(s"$envelope / $options: ${IssuePrinter[BaboonIssue].stringify(e)}"), identity)
          assert(toJson(codec, family, bytes) == Right(envelope), s"$options")
        }
    }

    "round-trip u32 values across the whole unsigned range and reject values outside it" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family                     = load(loader)
        def counter(n: String): String = s"""{"$$mv":1,"$$d":"zipdemo.shapes","$$v":"1.0.0","$$t":"zipdemo.shapes/:#Counter","$$c":{"n":$n}}"""
        for (n <- List("0", "2147483647", "2147483648", "4294967295")) {
          val bytes = toUeba(codec, family, counter(n), V1Compact).fold(e => fail(s"$n: ${IssuePrinter[BaboonIssue].stringify(e)}"), identity)
          assert(toJson(codec, family, bytes) == Right(counter(n)))
        }
        assert(toUeba(codec, family, counter("4294967295"), V1Compact).map(_.takeRight(11)) == Right("FF FF FF FF"))
        for (n <- List("-1", "4294967296", "1.5")) {
          rejects(codec.jsonToUeba(family, parse(counter(n)).toOption.get, V1Compact), "Expected u32")
        }
    }

    "accept the documented legacy JSON metadata forms" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family       = load(loader)
        val legacyString = AppendVarJson.replace("\"$mv\":1", "\"$mv\":\"1\"")
        val absent       = AppendVarJson.replace("\"$mv\":1,", "")
        assert(toUeba(codec, family, legacyString, V1Compact) == Right(AppendVarV1Strict))
        assert(toUeba(codec, family, absent, V1Compact) == Right(AppendVarV1Strict))
        for (bad <- List("2", "16", "true", "null", "\" 1 \"", "1.5", "[]", "{}", "-1", "256")) {
          rejects(codec.jsonToUeba(family, parse(AppendVarJson.replace("\"$mv\":1", s"\"$$mv\":$bad")).toOption.get, V1Compact), "malformed JSON envelope metadata")
        }
    }

    "reject malformed JSON envelopes" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family                = load(loader)
        def json(s: String): Json = parse(s).fold(e => fail(e.toString), identity)
        rejects(codec.jsonToUeba(family, json("[]"), V1Compact), "must be a JSON object")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$c\"", "\"$x\":1,\"$c\"")), V1Compact), "unexpected JSON envelope keys: $x")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace(",\"$c\":{\"a\":42,\"b\":\"hi\",\"t\":\"t\"}", "")), V1Compact), "no '$c' content")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$d\":\"fwde2e.fwd\",", "")), V1Compact), "malformed JSON envelope metadata")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$v\":\"2.0.0\"", "\"$v\":2")), V1Compact), "malformed JSON envelope metadata")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"02.0.0\"")), V1Compact), "not in canonical form")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"two\"")), V1Compact), "is not a version")
        // $rv above $uv (which defaults to $v here)
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$rv\":\"1.0.0\"", "\"$rv\":\"3.0.0\"")), V1Compact), "readableMin <= minCompat <= version")
        rejects(codec.jsonToUeba(family, json(StableJson.replace("\"$uv\":\"1.0.0\"", "\"$uv\":\"3.0.0\"")), V1Compact), "readableMin <= minCompat <= version")
        // the payload must not carry fields the type does not declare: they would be dropped
        rejects(codec.jsonToUeba(family, json(StableJson.replace("{\"s\":\"s\"}", "{\"s\":\"s\",\"extra\":1}")), V1Compact), "does not declare: extra")
        // a declared identity the model contradicts: FwdAppendVar changed in 2.0.0
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$rv\":\"1.0.0\"", "\"$uv\":\"1.0.0\"")), V1Compact), "its 1.0.0 definition differs from 2.0.0")
    }

    "reject unknown identities and non-envelope types" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family                = load(loader)
        def json(s: String): Json = parse(s).fold(e => fail(e.toString), identity)
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("\"$d\":\"fwde2e.fwd\"", "\"$d\":\"no.such\"")), V1Compact), "domain 'no.such' is not loaded")
        rejects(codec.jsonToUeba(family, json(StableJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"1.5.0\"")), V1Compact), "is not loaded (loaded: 1.0.0, 2.0.0)")
        // the display name is not an identifier
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("fwde2e.fwd/:#FwdAppendVar", "FwdAppendVar")), V1Compact), "type 'FwdAppendVar' does not exist")
        rejects(codec.jsonToUeba(family, json(AppendVarJson.replace("fwde2e.fwd/:#FwdAppendVar", "fwde2e.fwd/:#Missing")), V1Compact), "does not exist")
        rejects(
          codec.jsonToUeba(family, json("""{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#Cents","$c":"1"}"""), V1Compact),
          "never travel in a top-level envelope",
        )
    }

    "keep the interpreter's rejection of no-rt foreign values and any fields" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family                = load(loader)
        def json(s: String): Json = parse(s).fold(e => fail(e.toString), identity)
        rejects(
          codec.jsonToUeba(family, json("""{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#WithOpaque","$c":{"o":"x"}}"""), V1Compact),
          "Foreign types without rt binding cannot be encoded",
        )
        rejects(
          codec.jsonToUeba(family, json("""{"$d":"zipdemo.shapes","$v":"1.0.0","$t":"zipdemo.shapes/:#WithAny","$c":{"v":{}}}"""), V1Compact),
          "`any`",
        )
    }

    "convert newer-writer envelopes only when they are provably byte-identical to a loaded version" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family                = load(loader)
        def json(s: String): Json = parse(s).fold(e => fail(e.toString), identity)
        // FwdStable 3.0.0 unchanged since 1.0.0: lossless; identity is preserved, nothing is upgraded
        val stable3 = StableJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"3.0.0\"")
        val v2      = toUeba(codec, family, stable3, V2Compact).fold(e => fail(e.toString), identity)
        assert(v2 == StableV2.replace("05 32 2E 30 2E 30", "05 33 2E 30 2E 30"))
        assert(toJson(codec, family, v2) == Right(stable3))
        // a JSON writer 3.0.0 whose byte-identical bound is above every loaded version: only a lossy forward read could decode it
        rejects(
          codec.jsonToUeba(family, json(AppendVarJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"3.0.0\"")), V1Compact),
          "byte-identical bound 3.0.0 reaches none of them",
        )
        // ... even when its json-additive bound would allow a tolerant read
        rejects(
          codec.jsonToUeba(
            family,
            json(AppendVarJson.replace("\"$v\":\"2.0.0\"", "\"$v\":\"3.0.0\"").replace("\"$rv\":\"1.0.0\"", "\"$uv\":\"3.0.0\",\"$rv\":\"2.0.0\"")),
            V1Compact,
          ),
          "reaches none of them",
        )
        // a v2 envelope whose readableMin reaches a loaded version but whose minCompat does not
        rejects(codec.uebaToJson(family, unhex(AppendVarV2Compact.replace("05 32 2E 30 2E 30 02", "05 33 2E 30 2E 30 02"))), "reaches none of them")
        // a v1 envelope from an unknown newer version cannot prove byte-identity at all
        rejects(codec.uebaToJson(family, unhex(StableV1.replace("05 32 2E 30 2E 30", "05 33 2E 30 2E 30"))), "binary v1 envelope cannot prove")
    }

    "reject malformed binary envelopes" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeEnvelopeCodec[Either]) =>
        val family = load(loader)
        rejects(codec.uebaToJson(family, Vector.empty), "empty input")
        rejects(codec.uebaToJson(family, unhex("03" + StableV1.drop(2))), "unsupported binary envelope metaVersion 3")
        rejects(codec.uebaToJson(family, unhex("10" + StableV1.drop(2))), "unsupported binary envelope metaVersion 16")
        rejects(codec.uebaToJson(family, unhex(StableV1.replace("2E 30 01 05 31", "2E 30 02 05 31"))), "v1 has an illegal flags byte")
        rejects(codec.uebaToJson(family, unhex(StableV2.replace("2E 30 01 05 31", "2E 30 05 05 31"))), "v2 has an illegal flags byte")
        rejects(codec.uebaToJson(family, unhex(StableV1.take(20))), "truncated binary envelope header")
        rejects(codec.uebaToJson(family, unhex(AppendVarV1Strict.dropRight(3))), "truncated payload")
        rejects(codec.uebaToJson(family, unhex(StableV1 + " 00")), "unread byte(s)")
        rejects(codec.uebaToJson(family, unhex(StableV1.replace("53 74 61 62 6C 65", "53 74 61 62 6C 66"))), "does not exist")
    }
  }
}

object RuntimeEnvelopeCodecTest {
  def hex(bytes: Vector[Byte]): String = bytes.map(b => f"${b & 0xFF}%02X").mkString(" ")

  def unhex(s: String): Vector[Byte] = {
    s.split(" ").toVector.filter(_.nonEmpty).map(Integer.parseInt(_, 16).toByte)
  }
}

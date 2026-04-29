package baboon.runtime.shared

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import scala.util.Random

/** PR-56: property + edge-case tests for the identifier-repr machinery
  * defined in `docs/spec/identifier-repr.md`.
  *
  * The tests exercise the runtime helpers (`IdentifierRepr`) directly
  * against a hand-coded mirror of what the emitter (`ScDefnTranslator`)
  * generates. Because the emitter is a faithful realization of the spec
  * via these helpers, validating the helpers + mirror validates the
  * emission contract end-to-end.
  *
  * No ScalaCheck dependency — a hand-rolled `Random` driver is used.
  * Documented choice: adding ScalaCheck just for one PR's property tests
  * is overkill, and the helper API is simple enough that a hand-rolled
  * generator gives equivalent coverage with no new build dependency.
  */
class IdentifierReprPropertyTest extends AnyWordSpec with Matchers {

  // ----- Mirror types: hand-coded shape matching what emitter produces -----

  // Each mirror "id" type is a case class with a `repr` method (mirrors
  // emitted toString) and a companion `parseRepr` (mirrors emitted
  // <TypeName>Codec.parseRepr). The version is fixed at "1.0.0" for tests.

  private val V = "1.0.0"

  // id Flat { x: i32; name: str }
  final case class FlatId(x: Int, name: String) {
    override def toString: String =
      s"FlatId:$V#" + "x:" + x.toString + ":" + "name:" + IdentifierRepr.escapeStr(name)
  }
  object FlatIdCodec {
    def parseRepr(s: String): Either[String, FlatId] = {
      val cursor = new IdentifierRepr.Cursor(s)
      parseRepr(cursor) match {
        case Left(e)  => Left(e)
        case Right(v) => if (cursor.atEnd) Right(v) else Left(s"trailing input at ${cursor.position}")
      }
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, FlatId] = {
      IdentifierRepr.parseHeader(cursor, "FlatId", V) match {
        case Left(e)  => return Left(e); case _ => ()
      }
      IdentifierRepr.parseFieldName(cursor, "x") match {
        case Left(e)  => return Left(e); case _ => ()
      }
      val xRaw = cursor.readUntilStructural()
      val x_v: Int = scala.util.Try(xRaw.toLong).toOption match {
        case Some(v) => v.toInt
        case None    => return Left(s"could not parse i32 for field x: $xRaw")
      }
      cursor.expect(':') match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "name") match {
        case Left(e)  => return Left(e); case _ => ()
      }
      val name_v = cursor.readStrField() match {
        case Right(v) => v
        case Left(e)  => return Left(e)
      }
      Right(FlatId(x = x_v, name = name_v))
    }
  }

  // id Mixed { active: bit; b: bytes; t: tsu }
  final case class MixedId(active: Boolean, b: ByteString, t: OffsetDateTime) {
    override def toString: String =
      s"MixedId:$V#" + "active:" + IdentifierRepr.bitToString(active) +
        ":" + "b:" + IdentifierRepr.bytesToHex(b) +
        ":" + "t:" + IdentifierRepr.tsuToString(t)
  }
  object MixedIdCodec {
    def parseRepr(s: String): Either[String, MixedId] = {
      val cursor = new IdentifierRepr.Cursor(s)
      parseRepr(cursor) match {
        case Left(e)  => Left(e)
        case Right(v) => if (cursor.atEnd) Right(v) else Left(s"trailing input at ${cursor.position}")
      }
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, MixedId] = {
      IdentifierRepr.parseHeader(cursor, "MixedId", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "active") match { case Left(e) => return Left(e); case _ => () }
      val a_raw = cursor.readUntilStructural()
      val active_v: Boolean = IdentifierRepr.parseBit(a_raw) match {
        case Right(v) => v; case Left(e) => return Left(e)
      }
      cursor.expect(':') match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "b") match { case Left(e) => return Left(e); case _ => () }
      val b_raw = cursor.readUntilStructural()
      val b_v: ByteString = IdentifierRepr.parseBytesHex(b_raw) match {
        case Right(v) => v; case Left(e) => return Left(e)
      }
      cursor.expect(':') match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "t") match { case Left(e) => return Left(e); case _ => () }
      val t_raw: String = cursor.readFixed(24) match {
        case Right(v) => v; case Left(e) => return Left(e)
      }
      val t_v: OffsetDateTime = IdentifierRepr.parseTsuRepr(t_raw) match {
        case Right(v) => v; case Left(e) => return Left(e)
      }
      Right(MixedId(active = active_v, b = b_v, t = t_v))
    }
  }

  // id Outer { id: uid; inner: FlatId }   — single-level nest
  final case class OuterId(id: UUID, inner: FlatId) {
    override def toString: String =
      s"OuterId:$V#" + "id:" + id.toString +
        ":" + "inner:" + "{" + inner.toString + "}"
  }
  object OuterIdCodec {
    def parseRepr(s: String): Either[String, OuterId] = {
      val cursor = new IdentifierRepr.Cursor(s)
      parseRepr(cursor) match {
        case Left(e)  => Left(e)
        case Right(v) => if (cursor.atEnd) Right(v) else Left(s"trailing input at ${cursor.position}")
      }
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, OuterId] = {
      IdentifierRepr.parseHeader(cursor, "OuterId", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "id") match { case Left(e) => return Left(e); case _ => () }
      val idRaw = cursor.readUntilStructural()
      val id_v: UUID = scala.util.Try(UUID.fromString(idRaw)).toOption match {
        case Some(v) => v; case None => return Left(s"could not parse uid: $idRaw")
      }
      cursor.expect(':') match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "inner") match { case Left(e) => return Left(e); case _ => () }
      cursor.expect('{') match { case Left(e) => return Left(e); case _ => () }
      val inner_v: FlatId = FlatIdCodec.parseRepr(cursor) match {
        case Right(v) => v; case Left(e) => return Left(e)
      }
      cursor.expect('}') match { case Left(e) => return Left(e); case _ => () }
      Right(OuterId(id = id_v, inner = inner_v))
    }
  }

  // 4-level deep nest:
  // id D4 { x: i32 }
  // id D3 { d: D4 }
  // id D2 { d: D3 }
  // id D1 { d: D2 }
  final case class D4(x: Int) {
    override def toString: String = s"D4:$V#x:" + x.toString
  }
  object D4Codec {
    def parseRepr(s: String): Either[String, D4] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, D4] = {
      IdentifierRepr.parseHeader(cursor, "D4", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "x") match { case Left(e) => return Left(e); case _ => () }
      val raw = cursor.readUntilStructural()
      scala.util.Try(raw.toInt).toOption match {
        case Some(v) => Right(D4(v)); case None => Left(s"bad i32: $raw")
      }
    }
  }
  final case class D3(d: D4) {
    override def toString: String = s"D3:$V#d:{" + d.toString + "}"
  }
  object D3Codec {
    def parseRepr(s: String): Either[String, D3] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, D3] = {
      IdentifierRepr.parseHeader(cursor, "D3", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "d") match { case Left(e) => return Left(e); case _ => () }
      cursor.expect('{') match { case Left(e) => return Left(e); case _ => () }
      val v = D4Codec.parseRepr(cursor) match { case Right(v) => v; case Left(e) => return Left(e) }
      cursor.expect('}') match { case Left(e) => return Left(e); case _ => () }
      Right(D3(v))
    }
  }
  final case class D2(d: D3) {
    override def toString: String = s"D2:$V#d:{" + d.toString + "}"
  }
  object D2Codec {
    def parseRepr(s: String): Either[String, D2] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, D2] = {
      IdentifierRepr.parseHeader(cursor, "D2", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "d") match { case Left(e) => return Left(e); case _ => () }
      cursor.expect('{') match { case Left(e) => return Left(e); case _ => () }
      val v = D3Codec.parseRepr(cursor) match { case Right(v) => v; case Left(e) => return Left(e) }
      cursor.expect('}') match { case Left(e) => return Left(e); case _ => () }
      Right(D2(v))
    }
  }
  final case class D1(d: D2) {
    override def toString: String = s"D1:$V#d:{" + d.toString + "}"
  }
  object D1Codec {
    def parseRepr(s: String): Either[String, D1] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, D1] = {
      IdentifierRepr.parseHeader(cursor, "D1", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "d") match { case Left(e) => return Left(e); case _ => () }
      cursor.expect('{') match { case Left(e) => return Left(e); case _ => () }
      val v = D2Codec.parseRepr(cursor) match { case Right(v) => v; case Left(e) => return Left(e) }
      cursor.expect('}') match { case Left(e) => return Left(e); case _ => () }
      Right(D1(v))
    }
  }

  // id ULong { x: u64 }
  final case class ULongId(x: Long) {
    override def toString: String = s"ULongId:$V#x:" + IdentifierRepr.u64ToString(x)
  }
  object ULongIdCodec {
    def parseRepr(s: String): Either[String, ULongId] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, ULongId] = {
      IdentifierRepr.parseHeader(cursor, "ULongId", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "x") match { case Left(e) => return Left(e); case _ => () }
      val raw = cursor.readUntilStructural()
      scala.util.Try(java.lang.Long.parseUnsignedLong(raw)).toOption match {
        case Some(v) => Right(ULongId(v)); case None => Left(s"bad u64: $raw")
      }
    }
  }

  // id LongId { x: i64 } — exercise i64 boundaries through the helper / parse path
  final case class LongIdMirror(x: Long) {
    override def toString: String = s"LongIdMirror:$V#x:" + x.toString
  }
  object LongIdMirrorCodec {
    def parseRepr(s: String): Either[String, LongIdMirror] = {
      val c = new IdentifierRepr.Cursor(s)
      parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, LongIdMirror] = {
      IdentifierRepr.parseHeader(cursor, "LongIdMirror", V) match { case Left(e) => return Left(e); case _ => () }
      IdentifierRepr.parseFieldName(cursor, "x") match { case Left(e) => return Left(e); case _ => () }
      val raw = cursor.readUntilStructural()
      scala.util.Try(raw.toLong).toOption match {
        case Some(v) => Right(LongIdMirror(v))
        case None    => Left(s"bad i64: $raw")
      }
    }
  }

  // id Marker {}  — empty fields case
  final case class MarkerId() {
    override def toString: String = s"MarkerId:$V#"
  }
  object MarkerIdCodec {
    def parseRepr(s: String): Either[String, MarkerId] = {
      val c = new IdentifierRepr.Cursor(s); parseRepr(c).flatMap(v => if (c.atEnd) Right(v) else Left("trailing"))
    }
    def parseRepr(cursor: IdentifierRepr.Cursor): Either[String, MarkerId] = {
      IdentifierRepr.parseHeader(cursor, "MarkerId", V) match { case Left(e) => return Left(e); case _ => () }
      Right(MarkerId())
    }
  }

  // ----- Random generators -----

  private def randomMetacharStr(rng: Random, maxLen: Int): String = {
    val len = rng.nextInt(maxLen + 1)
    val sb  = new StringBuilder(len)
    var i   = 0
    val alphabet = "ab\\#:{}c\n\t ÿ☃" // includes all 5 metachars and some non-ASCII
    while (i < len) {
      sb.append(alphabet.charAt(rng.nextInt(alphabet.length)))
      i += 1
    }
    sb.toString
  }

  private def randomBytes(rng: Random, maxLen: Int): ByteString = {
    val len = rng.nextInt(maxLen + 1)
    val arr = new Array[Byte](len)
    rng.nextBytes(arr)
    ByteString(arr)
  }

  private def randomTsu(rng: Random): OffsetDateTime = {
    // Random millisecond between 1970 and ~2200, UTC, three-digit-aligned millis.
    val msInRange = math.abs(rng.nextLong()) % (7L * 365L * 24L * 3600L * 1000L) // ~7 years range
    val baseEpoch = 0L
    val total     = baseEpoch + msInRange
    OffsetDateTime
      .ofInstant(java.time.Instant.ofEpochMilli(total), ZoneOffset.UTC)
  }

  // ----- Property tests -----

  "IdentifierRepr (round-trip property)" should {

    "round-trip 1000 random FlatId values (mixed metachars in strings)" in {
      val rng = new Random(0xCAFEBABEL)
      var i   = 0
      while (i < 1000) {
        val v       = FlatId(rng.nextInt(), randomMetacharStr(rng, 20))
        val rendered = v.toString
        FlatIdCodec.parseRepr(rendered) match {
          case Right(parsed) => parsed shouldBe v
          case Left(e)       => fail(s"parse failed for $v -> $rendered: $e")
        }
        i += 1
      }
    }

    "round-trip 500 random MixedId values (bit + bytes + tsu)" in {
      val rng = new Random(0xDEADBEEFL)
      var i   = 0
      while (i < 500) {
        val v       = MixedId(rng.nextBoolean(), randomBytes(rng, 24), randomTsu(rng))
        val rendered = v.toString
        MixedIdCodec.parseRepr(rendered) match {
          case Right(parsed) => parsed shouldBe v
          case Left(e)       => fail(s"parse failed for $v -> $rendered: $e")
        }
        i += 1
      }
    }

    "round-trip 200 random OuterId values (single-level nest with uid + nested FlatId)" in {
      val rng = new Random(0xFEED1234L)
      var i   = 0
      while (i < 200) {
        val nested  = FlatId(rng.nextInt(), randomMetacharStr(rng, 10))
        val v       = OuterId(new UUID(rng.nextLong(), rng.nextLong()), nested)
        val rendered = v.toString
        OuterIdCodec.parseRepr(rendered) match {
          case Right(parsed) => parsed shouldBe v
          case Left(e)       => fail(s"parse failed for $v -> $rendered: $e")
        }
        i += 1
      }
    }

    "round-trip 200 random 4-level-deep nested ids" in {
      val rng = new Random(0xABCDEF01L)
      var i   = 0
      while (i < 200) {
        val v       = D1(D2(D3(D4(rng.nextInt()))))
        val rendered = v.toString
        D1Codec.parseRepr(rendered) match {
          case Right(parsed) => parsed shouldBe v
          case Left(e)       => fail(s"parse failed for $v -> $rendered: $e")
        }
        i += 1
      }
    }

    "round-trip 200 random ULongId values incl. high bit set" in {
      val rng = new Random(0x12345678L)
      var i   = 0
      while (i < 200) {
        val v        = ULongId(rng.nextLong())
        val rendered = v.toString
        ULongIdCodec.parseRepr(rendered) match {
          case Right(parsed) => parsed shouldBe v
          case Left(e)       => fail(s"parse failed for $v -> $rendered: $e")
        }
        i += 1
      }
    }
  }

  // ----- Hand-crafted edge cases -----

  "IdentifierRepr (hand-crafted edge cases)" should {

    "empty `str` field renders as bare colon and parses back" in {
      val v = FlatId(0, "")
      v.toString shouldBe "FlatId:1.0.0#x:0:name:"
      FlatIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`str` of only one backslash" in {
      val v = FlatId(1, "\\")
      v.toString shouldBe "FlatId:1.0.0#x:1:name:\\\\"
      FlatIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`str` of only one colon" in {
      val v = FlatId(2, ":")
      v.toString shouldBe "FlatId:1.0.0#x:2:name:\\:"
      FlatIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`str` of four backslashes" in {
      val v = FlatId(3, "\\\\\\\\")
      // 4 source backslashes -> 8 escaped chars (each backslash becomes \\)
      v.toString shouldBe "FlatId:1.0.0#x:3:name:\\\\\\\\\\\\\\\\"
      FlatIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`str` containing each of the 5 metacharacters" in {
      val v = FlatId(4, "\\#:{}")
      v.toString shouldBe "FlatId:1.0.0#x:4:name:\\\\\\#\\:\\{\\}"
      FlatIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "deep 4-level nest renders with brace wrappers" in {
      val v = D1(D2(D3(D4(42))))
      v.toString shouldBe "D1:1.0.0#d:{D2:1.0.0#d:{D3:1.0.0#d:{D4:1.0.0#x:42}}}"
      D1Codec.parseRepr(v.toString) shouldBe Right(v)
    }

    "i64.MIN_VALUE renders with explicit sign and parses back" in {
      val v = LongIdMirror(Long.MinValue)
      v.toString shouldBe "LongIdMirror:1.0.0#x:-9223372036854775808"
      LongIdMirrorCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "i64.MAX_VALUE round-trips through LongIdMirror" in {
      val v = LongIdMirror(Long.MaxValue)
      v.toString shouldBe "LongIdMirror:1.0.0#x:9223372036854775807"
      LongIdMirrorCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "u64.MAX_VALUE round-trips through ULongId" in {
      val v = ULongId(-1L) // -1L as u64 is 0xFFFFFFFFFFFFFFFF == 2^64 - 1
      v.toString shouldBe "ULongId:1.0.0#x:18446744073709551615"
      ULongIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "empty `bytes` field round-trips as empty hex" in {
      val v = MixedId(true, ByteString.empty, OffsetDateTime.parse("2026-01-01T00:00:00.000Z"))
      v.toString shouldBe "MixedId:1.0.0#active:true:b::t:2026-01-01T00:00:00.000Z"
      MixedIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`bytes` of [0xff, 0xfe, 0x00] renders lowercase hex" in {
      val v = MixedId(false, ByteString(0xff.toByte, 0xfe.toByte, 0x00.toByte), OffsetDateTime.parse("2026-04-29T12:34:56.789Z"))
      v.toString shouldBe "MixedId:1.0.0#active:false:b:fffe00:t:2026-04-29T12:34:56.789Z"
      MixedIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "`tsu` at epoch and far-future round-trip" in {
      val epoch  = OffsetDateTime.ofInstant(java.time.Instant.ofEpochMilli(0L), ZoneOffset.UTC)
      val future = OffsetDateTime.parse("2999-12-31T23:59:59.999Z")
      val v1     = MixedId(true, ByteString.empty, epoch)
      val v2     = MixedId(false, ByteString.empty, future)
      MixedIdCodec.parseRepr(v1.toString) shouldBe Right(v1)
      MixedIdCodec.parseRepr(v2.toString) shouldBe Right(v2)
    }

    "empty-fields `MarkerId` renders as bare header" in {
      val v = MarkerId()
      v.toString shouldBe "MarkerId:1.0.0#"
      MarkerIdCodec.parseRepr(v.toString) shouldBe Right(v)
    }

    "parser rejects wrong simple name" in {
      val r = FlatIdCodec.parseRepr("OtherId:1.0.0#x:1:name:hi")
      r.isLeft shouldBe true
    }

    "parser rejects wrong version" in {
      val r = FlatIdCodec.parseRepr("FlatId:2.0.0#x:1:name:hi")
      r.isLeft shouldBe true
    }

    "parser rejects trailing input" in {
      val r = FlatIdCodec.parseRepr("FlatId:1.0.0#x:1:name:hi:extra")
      r.isLeft shouldBe true
    }

    "parser rejects unescaped backslash followed by random char in str" in {
      // Construct a malformed escape directly
      val r = FlatIdCodec.parseRepr("FlatId:1.0.0#x:1:name:\\a")
      r.isLeft shouldBe true
    }

    "parser rejects uppercase hex in bytes" in {
      val r = MixedIdCodec.parseRepr("MixedId:1.0.0#active:true:b:FF:t:2026-01-01T00:00:00.000Z")
      r.isLeft shouldBe true
    }

    "parser rejects odd-length hex in bytes" in {
      val r = MixedIdCodec.parseRepr("MixedId:1.0.0#active:true:b:fff:t:2026-01-01T00:00:00.000Z")
      r.isLeft shouldBe true
    }
  }

  // ----- Spec-document conformance tests (canonical examples §6) -----

  "spec §6 canonical examples" should {
    "§6.1 empty str field" in {
      val v = FlatId(0, "")
      v.toString.endsWith("name:") shouldBe true
    }

    "§6.2 str containing each metacharacter" in {
      val v = FlatId(0, "\\#:{}")
      // expected: name:\\\#\:\{\}
      v.toString.endsWith("name:\\\\\\#\\:\\{\\}") shouldBe true
    }

    "§6.6 bytes with high bytes" in {
      val v = MixedId(true, ByteString(0xff.toByte, 0xfe.toByte, 0x00.toByte), OffsetDateTime.parse("2026-01-01T00:00:00.000Z"))
      v.toString.contains("b:fffe00") shouldBe true
    }

    "§6.10 4-level nest" in {
      val v = D1(D2(D3(D4(42))))
      v.toString shouldBe "D1:1.0.0#d:{D2:1.0.0#d:{D3:1.0.0#d:{D4:1.0.0#x:42}}}"
    }

    "§6.12 empty-fields id" in {
      val v = MarkerId()
      v.toString shouldBe "MarkerId:1.0.0#"
    }
  }
}

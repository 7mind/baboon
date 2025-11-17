package baboon.runtime.shared

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.time.OffsetDateTime
import java.util.UUID

class BaboonRuntimeSharedTest extends AnyWordSpec with Matchers {

  "BaboonBinTools.writeBigDecimal and readBigDecimal" should {
    "round-trip positive decimal values" in {
      val values = Seq(
        BigDecimal("0"),
        BigDecimal("1"),
        BigDecimal("123.456"),
        BigDecimal("999999999.999999"),
        BigDecimal("0.001"),
        BigDecimal("1234567890.123456789")
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeBigDecimal(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readBigDecimal(input)

        // Compare using compareTo to handle scale differences
        result.compareTo(value) shouldBe 0
      }
    }

    "round-trip negative decimal values" in {
      val values = Seq(
        BigDecimal("-1"),
        BigDecimal("-123.456"),
        BigDecimal("-999999999.999999"),
        BigDecimal("-0.001")
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeBigDecimal(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readBigDecimal(input)

        // Compare using compareTo to handle scale differences
        result.compareTo(value) shouldBe 0
      }
    }

    "round-trip maximum scale values" in {
      // C# decimal supports scale 0-28, but trailing zeros may be lost
      val value = BigDecimal("1.2345678901234567890123456789") // 28 decimal places (max for C# decimal)

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeBigDecimal(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readBigDecimal(input)

      // Compare using compareTo to handle scale differences
      result.compareTo(value) shouldBe 0
    }
  }

  "BaboonBinTools.writeString and readString" should {
    "round-trip empty strings" in {
      val value = ""

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readString(input)

      result shouldBe value
    }

    "round-trip simple ASCII strings" in {
      val values = Seq(
        "hello",
        "world",
        "test",
        "The quick brown fox jumps over the lazy dog"
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeString(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readString(input)

        result shouldBe value
      }
    }

    "round-trip UTF-8 strings with special characters" in {
      val values = Seq(
        "Hello, 世界!",
        "Привет мир",
        "مرحبا بالعالم",
        "🚀🎉✨"
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeString(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readString(input)

        result shouldBe value
      }
    }

    "round-trip long strings (> 127 bytes)" in {
      val value = "a" * 1000

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readString(input)

      result shouldBe value
    }
  }

  "BaboonBinTools.writeUid and readUid" should {
    "round-trip random UUIDs" in {
      val values = Seq(
        UUID.randomUUID(),
        UUID.randomUUID(),
        UUID.randomUUID(),
        UUID.fromString("00000000-0000-0000-0000-000000000000"),
        UUID.fromString("ffffffff-ffff-ffff-ffff-ffffffffffff"),
        UUID.fromString("12345678-1234-5678-1234-567812345678")
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeUid(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readUid(input)

        result shouldBe value
      }
    }
  }

  "BaboonBinTools.writeTimestamp and readTimestamp" should {
    "round-trip UTC timestamps" in {
      val values = Seq(
        OffsetDateTime.parse("2025-01-01T00:00:00Z"),
        OffsetDateTime.parse("2025-11-14T12:34:56.789Z"),
        OffsetDateTime.parse("1970-01-01T00:00:00Z"),
        OffsetDateTime.parse("2099-12-31T23:59:59.999Z")
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeTimestamp(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readTimestamp(input)

        result shouldBe value
      }
    }

    "round-trip timestamps with different offsets" in {
      val values = Seq(
        OffsetDateTime.parse("2025-01-01T00:00:00+01:00"),
        OffsetDateTime.parse("2025-01-01T00:00:00-05:00"),
        OffsetDateTime.parse("2025-01-01T00:00:00+05:30"),
        OffsetDateTime.parse("2025-01-01T00:00:00-10:00")
      )

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)

        BaboonBinTools.writeTimestamp(writer, value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = BaboonBinTools.readTimestamp(input)

        result shouldBe value
      }
    }
  }

  "LEDataInputStream and LEDataOutputStream" should {
    "round-trip byte values" in {
      val value: Byte = 42

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)
      writer.writeByte(value.toInt)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = input.readByte()

      result shouldBe value
    }

    "round-trip short values in little-endian" in {
      val values = Seq[Short](0, 1, -1, 127, -128, 255, 256, Short.MaxValue, Short.MinValue)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeShort(value.toInt)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readShort()

        result shouldBe value
      }
    }

    "round-trip int values in little-endian" in {
      val values = Seq(0, 1, -1, 127, -128, 255, 256, 65535, 65536, Int.MaxValue, Int.MinValue)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeInt(value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readInt()

        result shouldBe value
      }
    }

    "round-trip long values in little-endian" in {
      val values = Seq(0L, 1L, -1L, Long.MaxValue, Long.MinValue)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeLong(value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readLong()

        result shouldBe value
      }
    }

    "round-trip float values in little-endian" in {
      val values = Seq(0.0f, 1.0f, -1.0f, Float.MaxValue, Float.MinValue, Float.MinPositiveValue, 3.14159f)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeFloat(value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readFloat()

        result shouldBe value
      }
    }

    "round-trip double values in little-endian" in {
      val values = Seq(0.0, 1.0, -1.0, Double.MaxValue, Double.MinValue, Double.MinPositiveValue, 3.141592653589793)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeDouble(value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readDouble()

        result shouldBe value
      }
    }

    "round-trip boolean values" in {
      val values = Seq(true, false)

      values.foreach { value =>
        val output = new ByteArrayOutputStream()
        val writer = new LEDataOutputStream(output)
        writer.writeBoolean(value)
        writer.flush()

        val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
        val result = input.readBoolean()

        result shouldBe value
      }
    }
  }

  "BaboonBinTools.toUnsignedBigInt" should {
    "convert positive long to BigInt" in {
      val value = 123456789L
      val result = BaboonBinTools.toUnsignedBigInt(value)
      result shouldBe BigInt(123456789L)
    }

    "convert negative long to unsigned BigInt" in {
      val value = -1L
      val result = BaboonBinTools.toUnsignedBigInt(value)
      result shouldBe BigInt("18446744073709551615") // 2^64 - 1
    }

    "convert Long.MinValue to unsigned BigInt" in {
      val value = Long.MinValue
      val result = BaboonBinTools.toUnsignedBigInt(value)
      result shouldBe BigInt("9223372036854775808") // 2^63
    }

    "convert zero to BigInt" in {
      val value = 0L
      val result = BaboonBinTools.toUnsignedBigInt(value)
      result shouldBe BigInt(0)
    }
  }

  "ByteString construction" should {
    "create from byte array" in {
      val bytes = Array[Byte](1, 2, 3, 4, 5)
      val bs = ByteString(bytes)
      bs.length shouldBe 5
      bs(0) shouldBe 1.toByte
      bs(4) shouldBe 5.toByte
    }

    "create from string" in {
      val bs = ByteString("hello")
      bs.toString shouldBe "hello"
      bs.length shouldBe 5
    }

    "create from hex string" in {
      val bs = ByteString.parseHex("48656C6C6F")
      bs.toString shouldBe "Hello"
    }

    "create empty ByteString" in {
      val bs = ByteString.empty
      bs.length shouldBe 0
      bs.isEmpty shouldBe true
    }

    "parse hex string with separators" in {
      val bs = ByteString.parseHex("48-65-6C-6C-6F")
      bs.toString shouldBe "Hello"
    }

    "fail on odd-length hex string" in {
      assertThrows[IllegalArgumentException] {
        ByteString.parseHex("ABC")
      }
    }
  }

  "ByteString equality and comparison" should {
    "equal identical ByteStrings" in {
      val bs1 = ByteString(Array[Byte](1, 2, 3))
      val bs2 = ByteString(Array[Byte](1, 2, 3))
      bs1 shouldBe bs2
      bs1.hashCode shouldBe bs2.hashCode
    }

    "not equal different ByteStrings" in {
      val bs1 = ByteString(Array[Byte](1, 2, 3))
      val bs2 = ByteString(Array[Byte](1, 2, 4))
      bs1 should not be bs2
    }

    "compare ByteStrings lexicographically" in {
      val bs1 = ByteString(Array[Byte](1, 2, 3))
      val bs2 = ByteString(Array[Byte](1, 2, 4))
      (bs1 < bs2) shouldBe true
      (bs2 > bs1) shouldBe true
      (bs1 <= bs2) shouldBe true
      (bs1 >= bs1) shouldBe true
    }

    "compare different lengths" in {
      val bs1 = ByteString(Array[Byte](1, 2))
      val bs2 = ByteString(Array[Byte](1, 2, 3))
      (bs1 < bs2) shouldBe true
    }
  }

  "ByteString concatenation" should {
    "concatenate two ByteStrings" in {
      val bs1 = ByteString("Hello")
      val bs2 = ByteString(" World")
      val result = bs1.concat(bs2)
      result.toString shouldBe "Hello World"
    }

    "concatenate using + operator" in {
      val bs1 = ByteString("Hello")
      val bs2 = ByteString(" World")
      val result = bs1 + bs2
      result.toString shouldBe "Hello World"
    }

    "concatenate multiple ByteStrings" in {
      val bs1 = ByteString("A")
      val bs2 = ByteString("B")
      val bs3 = ByteString("C")
      val result = bs1.concat(bs2, bs3)
      result.toString shouldBe "ABC"
    }
  }

  "ByteString encoding" should {
    "encode to hex string" in {
      val bs = ByteString("Hello")
      bs.toHexString shouldBe "48656C6C6F"
    }

    "encode empty ByteString" in {
      val bs = ByteString.empty
      bs.toHexString shouldBe ""
    }

    "round-trip through hex encoding" in {
      val original = ByteString(Array[Byte](0, 15, 255.toByte, 127, -128))
      val hex = original.toHexString
      val decoded = ByteString.parseHex(hex)
      decoded shouldBe original
    }
  }

  "ByteString substring operations" should {
    "extract substring" in {
      val bs = ByteString("Hello World")
      val sub = bs.substring(0, 5)
      sub.toString shouldBe "Hello"
    }

    "slice ByteString" in {
      val bs = ByteString("Hello World")
      val sliced = bs.slice(6, 11)
      sliced.toString shouldBe "World"
    }

    "take first n bytes" in {
      val bs = ByteString("Hello World")
      bs.take(5).toString shouldBe "Hello"
    }

    "drop first n bytes" in {
      val bs = ByteString("Hello World")
      bs.drop(6).toString shouldBe "World"
    }

    "check startsWith" in {
      val bs = ByteString("Hello World")
      val prefix = ByteString("Hello")
      bs.startsWith(prefix) shouldBe true
    }

    "check endsWith" in {
      val bs = ByteString("Hello World")
      val suffix = ByteString("World")
      bs.endsWith(suffix) shouldBe true
    }
  }

  "ByteString binary serialization" should {
    "round-trip empty ByteString" in {
      val value = ByteString.empty

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeByteString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readByteString(input)

      result shouldBe value
    }

    "round-trip simple ByteString" in {
      val value = ByteString("Hello World")

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeByteString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readByteString(input)

      result shouldBe value
      result.toString shouldBe "Hello World"
    }

    "round-trip ByteString with arbitrary bytes" in {
      val value = ByteString(Array[Byte](0, 1, 127, -128, -1, 42, 255.toByte))

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeByteString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readByteString(input)

      result shouldBe value
      result.toArray shouldBe value.toArray
    }

    "round-trip large ByteString" in {
      val largeBytes = Array.fill[Byte](10000)(42.toByte)
      val value = ByteString(largeBytes)

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeByteString(writer, value)
      writer.flush()

      val input = new LEDataInputStream(new ByteArrayInputStream(output.toByteArray))
      val result = BaboonBinTools.readByteString(input)

      result shouldBe value
      result.length shouldBe 10000
    }

    "encode length as uint32" in {
      val value = ByteString("test")

      val output = new ByteArrayOutputStream()
      val writer = new LEDataOutputStream(output)

      BaboonBinTools.writeByteString(writer, value)
      writer.flush()

      val bytes = output.toByteArray
      // First 4 bytes should be the length (4) in little-endian
      bytes(0) shouldBe 4.toByte
      bytes(1) shouldBe 0.toByte
      bytes(2) shouldBe 0.toByte
      bytes(3) shouldBe 0.toByte
      // Next 4 bytes should be "test"
      bytes(4) shouldBe 't'.toByte
      bytes(5) shouldBe 'e'.toByte
      bytes(6) shouldBe 's'.toByte
      bytes(7) shouldBe 't'.toByte
    }
  }

  "ByteString functional operations" should {
    "map over bytes" in {
      val bs = ByteString(Array[Byte](1, 2, 3))
      val mapped = bs.map(b => (b + 1).toByte)
      mapped.toArray shouldBe Array[Byte](2, 3, 4)
    }

    "filter bytes" in {
      val bs = ByteString(Array[Byte](1, 2, 3, 4, 5))
      val filtered = bs.filter(_ > 2)
      filtered.toArray shouldBe Array[Byte](3, 4, 5)
    }

    "fold bytes" in {
      val bs = ByteString(Array[Byte](1, 2, 3, 4, 5))
      val sum = bs.foldLeft(0)(_ + _)
      sum shouldBe 15
    }
  }
}

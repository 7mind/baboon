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
}

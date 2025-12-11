package baboon.runtime.shared {

  import java.nio.charset.{Charset, StandardCharsets}
  import scala.annotation.tailrec
  import scala.util.Try

  /**
   * Minimal immutable ByteString implementation with comparison and concatenation
   */
  class ByteString private (private val bytes: Array[Byte]) extends Ordered[ByteString] {

    // Properties
    def length: Int       = bytes.length
    def isEmpty: Boolean  = bytes.isEmpty
    def nonEmpty: Boolean = bytes.nonEmpty

    // Indexed access
    def apply(index: Int): Byte = bytes(index)

    // Get a safe copy of the bytes
    def toArray: Array[Byte] = bytes.clone()

    // Direct access to underlying bytes - USE WITH CAUTION!
    // Modifying the returned array will break immutability
    def underlyingUnsafe: Array[Byte] = bytes

    // Concatenation
    def concat(other: ByteString): ByteString = {
      if (other == null) throw new NullPointerException("Cannot concatenate with null")
      val result = new Array[Byte](bytes.length + other.bytes.length)
      System.arraycopy(bytes, 0, result, 0, bytes.length)
      System.arraycopy(other.bytes, 0, result, bytes.length, other.bytes.length)
      new ByteString(result)
    }

    def concat(others: ByteString*): ByteString = {
      val totalLength = bytes.length + others.map(_.length).sum
      val result      = new Array[Byte](totalLength)
      var offset      = 0

      System.arraycopy(bytes, 0, result, offset, bytes.length)
      offset += bytes.length

      others.foreach {
        other =>
          if (other != null) {
            System.arraycopy(other.bytes, 0, result, offset, other.bytes.length)
            offset += other.bytes.length
          }
      }

      new ByteString(result)
    }

    // Operator for concatenation
    def +(other: ByteString): ByteString  = concat(other)
    def ++(other: ByteString): ByteString = concat(other)

    // Comparison (for Ordered trait)
    override def compare(that: ByteString): Int = {
      if (that == null) return 1

      val minLength = Math.min(bytes.length, that.bytes.length)
      var i         = 0
      while (i < minLength) {
        val cmp = (bytes(i) & 0xFF) - (that.bytes(i) & 0xFF) // Unsigned comparison
        if (cmp != 0) return cmp
        i += 1
      }
      bytes.length - that.bytes.length
    }

    // Equality
    override def equals(obj: Any): Boolean = obj match {
      case null => false
      case that: ByteString =>
        if (this.bytes.length != that.bytes.length) false
        else bytes.sameElements(that.bytes)
      case _ => false
    }

    override def hashCode(): Int = {
      var hash = 17
      bytes.foreach {
        b =>
          hash = hash * 31 + b
      }
      hash
    }

    // String conversions
    def toString(charset: Charset): String = new String(bytes, charset)
    override def toString: String          = toString(StandardCharsets.UTF_8)

    def toHexString: String = bytes.map("%02X".format(_)).mkString

    // Substring operations
    def substring(startIndex: Int, length: Int): ByteString = {
      if (startIndex < 0 || startIndex >= bytes.length)
        throw new IndexOutOfBoundsException(s"Start index: $startIndex")
      if (length < 0 || startIndex + length > bytes.length)
        throw new IndexOutOfBoundsException(s"Length: $length")

      val result = new Array[Byte](length)
      System.arraycopy(bytes, startIndex, result, 0, length)
      new ByteString(result)
    }

    def slice(from: Int, until: Int): ByteString = {
      substring(from, until - from)
    }

    def take(n: Int): ByteString = substring(0, Math.min(n, bytes.length))
    def drop(n: Int): ByteString = substring(Math.min(n, bytes.length), Math.max(0, bytes.length - n))

    def startsWith(other: ByteString): Boolean = {
      if (other == null || other.length > length) false
      else {
        var i = 0
        while (i < other.length) {
          if (bytes(i) != other.bytes(i)) return false
          i += 1
        }
        true
      }
    }

    def endsWith(other: ByteString): Boolean = {
      if (other == null || other.length > length) false
      else {
        val offset = length - other.length
        var i      = 0
        while (i < other.length) {
          if (bytes(offset + i) != other.bytes(i)) return false
          i += 1
        }
        true
      }
    }

    // Functional operations
    def map(f: Byte => Byte): ByteString = {
      new ByteString(bytes.map(f))
    }

    def filter(f: Byte => Boolean): ByteString = {
      new ByteString(bytes.filter(f))
    }

    def foreach(f: Byte => Unit): Unit = {
      bytes.foreach(f)
    }

    def foldLeft[B](z: B)(f: (B, Byte) => B): B = {
      bytes.foldLeft(z)(f)
    }

    def foldRight[B](z: B)(f: (Byte, B) => B): B = {
      bytes.foldRight(z)(f)
    }
  }

  /**
   * ByteString companion object with factory methods
   */
  object ByteString {

    // Factory methods
    def apply(bytes: Array[Byte]): ByteString = {
      if (bytes == null) throw new NullPointerException("bytes cannot be null")
      new ByteString(bytes.clone()) // Clone to ensure immutability
    }

    def apply(bytes: Byte*): ByteString = {
      new ByteString(bytes.toArray)
    }

    def apply(string: String, charset: Charset = StandardCharsets.UTF_8): ByteString = {
      new ByteString(string.getBytes(charset))
    }

    def fromString(string: String, charset: Charset = StandardCharsets.UTF_8): ByteString = {
      apply(string, charset)
    }

    // Static method to parse hex-encoded string into ByteString
    def parseHex(hexString: String): ByteString = {
      if (hexString == null)
        throw new NullPointerException("hexString cannot be null")

      // Remove common separators and whitespace
      val cleanHex = hexString.replaceAll("[\\s:-]", "").trim

      if (cleanHex.isEmpty)
        return empty

      if (cleanHex.length % 2 != 0)
        throw new IllegalArgumentException(
          s"Invalid hex string length: ${cleanHex.length}. Hex string must have even length."
        )

      try {
        val bytes = cleanHex
          .grouped(2).map {
            byteStr =>
              Integer.parseInt(byteStr, 16).toByte
          }.toArray
        new ByteString(bytes)
      } catch {
        case e: NumberFormatException =>
          throw new IllegalArgumentException(s"Invalid hex characters in string: ${e.getMessage}", e)
      }
    }

    // Try version that returns Try[ByteString]
    def tryParse(hexString: String): Try[ByteString] = {
      Try(parseHex(hexString))
    }

    // Empty ByteString singleton
    val empty: ByteString = new ByteString(Array.empty[Byte])

    // Builder for efficient concatenation
    class Builder {
      private val buffer = scala.collection.mutable.ArrayBuffer[Byte]()

      def +=(b: Byte): this.type = {
        buffer += b
        this
      }

      def +=(bs: ByteString): this.type = {
        buffer ++= bs.bytes
        this
      }

      def ++=(bytes: Array[Byte]): this.type = {
        buffer ++= bytes
        this
      }

      def result(): ByteString = new ByteString(buffer.toArray)
      def clear(): Unit        = buffer.clear()
    }

    def newBuilder: Builder = new Builder

    // Implicit conversions (optional, use with care)
    implicit class ByteStringOps(val sc: StringContext) extends AnyVal {
      def bs(args: Any*): ByteString = {
        val strings     = sc.parts.iterator
        val expressions = args.iterator
        val buf         = new StringBuilder(strings.next())

        while (strings.hasNext) {
          buf.append(expressions.next())
          buf.append(strings.next())
        }

        ByteString(buf.toString())
      }
    }

    // Utility methods
    @tailrec
    def concat(first: ByteString, others: ByteString*): ByteString = {
      if (first == null) {
        if (others.isEmpty) empty
        else concat(others.head, others.tail: _*)
      } else {
        first.concat(others: _*)
      }
    }
  }
}

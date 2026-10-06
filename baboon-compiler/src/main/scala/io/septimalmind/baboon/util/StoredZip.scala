package io.septimalmind.baboon.util

import java.nio.ByteBuffer
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import scala.util.Try

final case class ZipArchiveEntry(path: String, isDirectory: Boolean, data: Array[Byte])

/** In-memory reader for ZIP archives whose entries are stored (compression method 0) — the form
  * `baboon :scheme --zip-output` writes. Pure Scala, so it runs unchanged on Scala.js, where
  * `java.util.zip` does not exist.
  *
  * Rejected: other compression methods, encryption, ZIP64, multi-disk archives, CRC-32 or size
  * mismatches, local headers that disagree with the central directory, entry names that are not
  * UTF-8 (general-purpose flag bit 11) or plain ASCII, and truncated or overlapping structures.
  * Entry paths are returned verbatim; path policy belongs to the caller.
  */
object StoredZipReader {
  private val EndOfCentralDirectorySignature = 0x06054B50
  private val CentralDirectorySignature      = 0x02014B50
  private val LocalHeaderSignature           = 0x04034B50

  private val EndOfCentralDirectorySize = 22
  private val CentralDirectoryFixedSize = 46
  private val LocalHeaderFixedSize      = 30
  private val MaxCommentLength          = 0xFFFF

  private val MethodStored         = 0
  private val FlagEncrypted        = 0x0001
  private val FlagStrongEncryption = 0x0040
  private val FlagUtf8Names        = 0x0800
  private val Zip64Marker16        = 0xFFFF
  private val Zip64Marker32        = 0xFFFFFFFFL
  private val AsciiLimit           = 0x80
  private val DirectorySuffix      = "/"

  def read(bytes: Array[Byte]): Either[String, List[ZipArchiveEntry]] = {
    for {
      eocd    <- findEndOfCentralDirectory(bytes)
      entries <- readCentralDirectory(bytes, eocd)
    } yield entries
  }

  private final case class EndOfCentralDirectory(offset: Int, entryCount: Int, directoryOffset: Long, directorySize: Long)

  private def findEndOfCentralDirectory(bytes: Array[Byte]): Either[String, EndOfCentralDirectory] = {
    val lowest = math.max(0, bytes.length - EndOfCentralDirectorySize - MaxCommentLength)
    // the record whose comment length accounts exactly for the bytes after it
    val candidate = (bytes.length - EndOfCentralDirectorySize to lowest by -1).find {
      offset =>
        u32(bytes, offset) == EndOfCentralDirectorySignature &&
        offset + EndOfCentralDirectorySize + u16(bytes, offset + 20) == bytes.length
    }
    candidate match {
      case None => Left("not a ZIP archive: no end-of-central-directory record")
      case Some(offset) =>
        val disk            = u16(bytes, offset + 4)
        val directoryDisk   = u16(bytes, offset + 6)
        val entriesOnDisk   = u16(bytes, offset + 8)
        val entryCount      = u16(bytes, offset + 10)
        val directorySize   = u32(bytes, offset + 12)
        val directoryOffset = u32(bytes, offset + 16)
        if (entryCount == Zip64Marker16 || directorySize == Zip64Marker32 || directoryOffset == Zip64Marker32) {
          Left("ZIP64 archives are not supported")
        } else if (disk != 0 || directoryDisk != 0 || entriesOnDisk != entryCount) {
          Left("multi-disk ZIP archives are not supported")
        } else if (directoryOffset + directorySize != offset) {
          Left("corrupt ZIP archive: the central directory does not end at the end-of-central-directory record")
        } else {
          Right(EndOfCentralDirectory(offset, entryCount, directoryOffset, directorySize))
        }
    }
  }

  private def readCentralDirectory(bytes: Array[Byte], eocd: EndOfCentralDirectory): Either[String, List[ZipArchiveEntry]] = {
    val entries = List.newBuilder[ZipArchiveEntry]
    var cursor  = eocd.directoryOffset.toInt
    var index   = 0
    var failure = Option.empty[String]
    while (failure.isEmpty && index < eocd.entryCount) {
      readCentralRecord(bytes, cursor, eocd) match {
        case Left(error) => failure = Some(s"corrupt ZIP archive: central directory record ${index + 1}: $error")
        case Right((entry, next)) =>
          entries += entry
          cursor = next
          index += 1
      }
    }
    failure match {
      case Some(error)                   => Left(error)
      case None if cursor != eocd.offset => Left("corrupt ZIP archive: the central directory size does not match its records")
      case None                          => Right(entries.result())
    }
  }

  private def readCentralRecord(bytes: Array[Byte], offset: Int, eocd: EndOfCentralDirectory): Either[String, (ZipArchiveEntry, Int)] = {
    if (offset + CentralDirectoryFixedSize > eocd.offset || u32(bytes, offset) != CentralDirectorySignature) {
      Left("missing or truncated record")
    } else {
      val flags          = u16(bytes, offset + 8)
      val method         = u16(bytes, offset + 10)
      val crc            = u32(bytes, offset + 16)
      val compressedSize = u32(bytes, offset + 20)
      val size           = u32(bytes, offset + 24)
      val nameLength     = u16(bytes, offset + 28)
      val extraLength    = u16(bytes, offset + 30)
      val commentLength  = u16(bytes, offset + 32)
      val localOffset    = u32(bytes, offset + 42)
      val next           = offset + CentralDirectoryFixedSize + nameLength + extraLength + commentLength
      if (next > eocd.offset) {
        Left("truncated record")
      } else {
        val nameBytes = bytes.slice(offset + CentralDirectoryFixedSize, offset + CentralDirectoryFixedSize + nameLength)
        for {
          name <- decodeName(nameBytes, flags)
          _ <-
            if ((flags & (FlagEncrypted | FlagStrongEncryption)) != 0) Left(s"entry '$name' is encrypted")
            else if (method != MethodStored) Left(s"entry '$name' uses compression method $method; only stored entries (method 0) are supported")
            else if (compressedSize != size) Left(s"entry '$name' is stored but its compressed and uncompressed sizes differ")
            else if (compressedSize == Zip64Marker32 || localOffset == Zip64Marker32) Left(s"entry '$name' needs ZIP64, which is not supported")
            else Right(())
          data <- readLocalData(bytes, localOffset, nameBytes, compressedSize, eocd).left.map(error => s"entry '$name': $error")
          _    <- if (Crc32.of(data) == crc) Right(()) else Left(s"entry '$name' fails its CRC-32 check")
        } yield (ZipArchiveEntry(name, name.endsWith(DirectorySuffix), data), next)
      }
    }
  }

  private def readLocalData(bytes: Array[Byte], localOffset: Long, centralName: Array[Byte], size: Long, eocd: EndOfCentralDirectory): Either[String, Array[Byte]] = {
    if (localOffset + LocalHeaderFixedSize > eocd.directoryOffset || u32(bytes, localOffset.toInt) != LocalHeaderSignature) {
      Left("missing or truncated local header")
    } else {
      val start       = localOffset.toInt
      val method      = u16(bytes, start + 8)
      val nameLength  = u16(bytes, start + 26)
      val extraLength = u16(bytes, start + 28)
      val dataStart   = start.toLong + LocalHeaderFixedSize + nameLength + extraLength
      val localName   = bytes.slice(start + LocalHeaderFixedSize, start + LocalHeaderFixedSize + nameLength)
      if (method != MethodStored) {
        Left("local header disagrees with the central directory on the compression method")
      } else if (!java.util.Arrays.equals(localName, centralName)) {
        Left("local header disagrees with the central directory on the entry name")
      } else if (dataStart + size > eocd.directoryOffset) {
        Left("entry data runs past the start of the central directory")
      } else {
        Right(bytes.slice(dataStart.toInt, (dataStart + size).toInt))
      }
    }
  }

  private def decodeName(nameBytes: Array[Byte], flags: Int): Either[String, String] = {
    if ((flags & FlagUtf8Names) != 0) {
      Utf8.decodeStrict(nameBytes).left.map(_ => "entry name is not valid UTF-8")
    } else if (nameBytes.forall(b => (b & 0xFF) < AsciiLimit)) {
      Right(new String(nameBytes, StandardCharsets.US_ASCII))
    } else {
      Left("entry name contains non-ASCII bytes without the UTF-8 flag (bit 11)")
    }
  }

  private def u16(bytes: Array[Byte], offset: Int): Int = {
    if (offset < 0 || offset + 2 > bytes.length) -1 else (bytes(offset) & 0xFF) | ((bytes(offset + 1) & 0xFF) << 8)
  }

  private def u32(bytes: Array[Byte], offset: Int): Long = {
    if (offset < 0 || offset + 4 > bytes.length) -1L
    else u16(bytes, offset).toLong | (u16(bytes, offset + 2).toLong << 16)
  }
}

/** CRC-32 (IEEE 802.3, reflected polynomial 0xEDB88320) as used by ZIP. */
object Crc32 {
  private val Polynomial = 0xEDB88320
  private val Table: Array[Int] = Array.tabulate(256) {
    n =>
      (0 until 8).foldLeft(n)((c, _) => if ((c & 1) != 0) Polynomial ^ (c >>> 1) else c >>> 1)
  }

  def of(data: Array[Byte]): Long = {
    val crc = data.foldLeft(0xFFFFFFFF)((c, b) => Table((c ^ b) & 0xFF) ^ (c >>> 8))
    (crc ^ 0xFFFFFFFF).toLong & 0xFFFFFFFFL
  }
}

object Utf8 {
  /** Decodes UTF-8, failing on malformed input instead of substituting U+FFFD. */
  def decodeStrict(bytes: Array[Byte]): Either[String, String] = {
    Try {
      StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(bytes))
        .toString
    }.toEither.left.map(_.toString)
  }
}

/** Writes stored (method 0) ZIP archives byte-for-byte deterministically: entries in the given order,
  * UTF-8 names (flag bit 11), sizes and CRC-32 in the local headers, the DOS timestamp 1980-01-01
  * 00:00:00, and no extra fields or comments. `java.util.zip` is not used: it adds an extended
  * timestamp derived from the default time zone.
  */
object StoredZipWriter {
  private val LocalHeaderSignature           = 0x04034B50L
  private val CentralDirectorySignature      = 0x02014B50L
  private val EndOfCentralDirectorySignature = 0x06054B50L
  private val VersionNeeded                  = 10
  private val VersionMadeBy                  = 20
  private val FlagUtf8Names                  = 0x0800
  private val MethodStored                   = 0
  private val DosTimeMidnight                = 0
  private val DosDate19800101                = 0x0021
  private val MaxEntries                     = 0xFFFF

  def write(entries: List[(String, Array[Byte])]): Array[Byte] = {
    require(entries.size <= MaxEntries, s"at most $MaxEntries entries fit an archive without ZIP64")
    val out       = new java.io.ByteArrayOutputStream()
    val directory = new java.io.ByteArrayOutputStream()
    entries.foreach {
      case (path, data) =>
        val name   = path.getBytes(StandardCharsets.UTF_8)
        val offset = out.size().toLong
        val crc    = Crc32.of(data)
        u32(out, LocalHeaderSignature)
        commonHeader(out, name, data, crc)
        u16(out, 0) // extra field length
        out.write(name)
        out.write(data)

        u32(directory, CentralDirectorySignature)
        u16(directory, VersionMadeBy)
        commonHeader(directory, name, data, crc)
        u16(directory, 0) // extra field length
        u16(directory, 0) // comment length
        u16(directory, 0) // disk number start
        u16(directory, 0) // internal attributes
        u32(directory, 0L) // external attributes
        u32(directory, offset)
        directory.write(name)
    }
    val directoryOffset = out.size().toLong
    out.write(directory.toByteArray)
    u32(out, EndOfCentralDirectorySignature)
    u16(out, 0) // this disk
    u16(out, 0) // central directory disk
    u16(out, entries.size)
    u16(out, entries.size)
    u32(out, directory.size().toLong)
    u32(out, directoryOffset)
    u16(out, 0) // comment length
    out.toByteArray
  }

  // the fields from "version needed" to "file name length", shared by local and central headers
  private def commonHeader(out: java.io.ByteArrayOutputStream, name: Array[Byte], data: Array[Byte], crc: Long): Unit = {
    u16(out, VersionNeeded)
    u16(out, FlagUtf8Names)
    u16(out, MethodStored)
    u16(out, DosTimeMidnight)
    u16(out, DosDate19800101)
    u32(out, crc)
    u32(out, data.length.toLong)
    u32(out, data.length.toLong)
    u16(out, name.length)
  }

  private def u16(out: java.io.ByteArrayOutputStream, value: Int): Unit = {
    out.write(value & 0xFF)
    out.write((value >>> 8) & 0xFF)
  }

  private def u32(out: java.io.ByteArrayOutputStream, value: Long): Unit = {
    u16(out, (value & 0xFFFF).toInt)
    u16(out, ((value >>> 16) & 0xFFFF).toInt)
  }
}

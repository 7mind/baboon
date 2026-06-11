package io.septimalmind.baboon.lsp.protocol

import io.circe._
import io.circe.parser._
import io.septimalmind.baboon.lsp.LspLogging
import io.septimalmind.baboon.util.BLogger

import java.io._
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.util.Try

/** JVM implementation of LSP transport using java.io streams.
  *
  * Implements JSON-RPC message framing with a Content-Length header. Per the LSP
  * spec, Content-Length is the number of BYTES of the UTF-8 message body — not a
  * char count. Headers are read directly off the raw InputStream as ASCII lines
  * (no Reader, no buffered read-ahead past the header terminator), and the body
  * is read as EXACTLY that many bytes and decoded as UTF-8. Routing the body
  * through a char Reader (as a prior implementation did) over-reads on multi-byte
  * input and corrupts the framing of the following message.
  *
  * [[readMessage]] returns None ONLY to signal that the connection has ended:
  * a clean EOF before any header byte. A mid-message EOF, a missing
  * Content-Length, or any other framing failure is logged and also yields None
  * (the frame cannot be recovered). A JSON parse failure on an otherwise
  * correctly-framed body is logged and skipped — the stream is positioned at the
  * next header, so the loop advances to the next frame rather than disconnecting.
  */
class JsonRpcTransport(in: InputStream, out: OutputStream, logger: BLogger) extends LspTransport {
  private val input  = new BufferedInputStream(in)
  private val writer = new BufferedOutputStream(out)

  private final val ContentLengthHeader = "Content-Length:"
  private final val CR                  = '\r'.toInt
  private final val LF                  = '\n'.toInt

  override def readMessage(): Option[Json] = readFrame()

  @tailrec
  private def readFrame(): Option[Json] = {
    readHeaders() match {
      case HeaderResult.EndOfStream =>
        // Clean disconnect before any header byte: this is the only None that
        // means "connection ended".
        None

      case HeaderResult.Failure(reason) =>
        logger.message(LspLogging.Context, s"LSP transport: malformed frame, dropping message: $reason")
        None

      case HeaderResult.Headers(contentLength) =>
        readBody(contentLength) match {
          case Left(reason) =>
            logger.message(LspLogging.Context, s"LSP transport: incomplete body, dropping message: $reason")
            None

          case Right(bytes) =>
            val content = new String(bytes, StandardCharsets.UTF_8)
            parse(content) match {
              case Right(json) =>
                Some(json)
              case Left(err) =>
                // The body was framed correctly; the stream is positioned at the
                // next header. Skip this frame and continue rather than treating
                // a parse error as a disconnect.
                logger.message(LspLogging.Context, s"LSP transport: invalid JSON in framed body, skipping: ${err.getMessage}")
                readFrame()
            }
        }
    }
  }

  /** Read header lines as ASCII directly off the raw stream, stopping at the empty
    * line that terminates the header block. Reads no byte past that terminator.
    */
  private def readHeaders(): HeaderResult = {
    var contentLength = -1
    var sawAnyByte    = false

    @tailrec
    def loop(): HeaderResult = {
      readHeaderLine() match {
        case HeaderLine.Eof =>
          if (sawAnyByte) {
            HeaderResult.Failure("end of stream inside header block")
          } else {
            HeaderResult.EndOfStream
          }

        case HeaderLine.Line(line) =>
          sawAnyByte = true
          if (line.isEmpty) {
            // Empty line terminates the header block.
            if (contentLength >= 0) {
              HeaderResult.Headers(contentLength)
            } else {
              HeaderResult.Failure("missing Content-Length header")
            }
          } else {
            if (line.startsWith(ContentLengthHeader)) {
              val raw = line.substring(ContentLengthHeader.length).trim
              Try(raw.toInt).toOption match {
                case Some(n) if n >= 0 => contentLength = n
                case _                 => return HeaderResult.Failure(s"invalid Content-Length value: '$raw'")
              }
            }
            // Any other header (e.g. Content-Type) is ignored.
            loop()
          }
      }
    }

    loop()
  }

  /** Read a single ASCII header line terminated by CRLF (a bare LF is tolerated).
    * The terminator is consumed but not returned. Returns [[HeaderLine.Eof]] only
    * when no byte at all was available before end-of-stream.
    */
  private def readHeaderLine(): HeaderLine = {
    val buf      = new ByteArrayOutputStream(64)
    var sawByte  = false
    var finished = false
    var result: HeaderLine = HeaderLine.Eof

    while (!finished) {
      val b = input.read()
      if (b == -1) {
        finished = true
        result =
          if (sawByte) HeaderLine.Line(new String(buf.toByteArray, StandardCharsets.US_ASCII))
          else HeaderLine.Eof
      } else if (b == LF) {
        finished = true
        result = HeaderLine.Line(new String(buf.toByteArray, StandardCharsets.US_ASCII))
      } else if (b == CR) {
        sawByte = true
        // Expect LF next; if it is something else, surface it as the next line's
        // first byte by pushing it back is not possible on a plain stream, so we
        // treat CR as the line terminator and consume a following LF if present.
        input.mark(1)
        val next = input.read()
        if (next != LF && next != -1) {
          input.reset()
        }
        finished = true
        result = HeaderLine.Line(new String(buf.toByteArray, StandardCharsets.US_ASCII))
      } else {
        sawByte = true
        buf.write(b)
      }
    }

    result
  }

  /** Read EXACTLY contentLength bytes from the raw stream, looping on short reads. */
  private def readBody(contentLength: Int): Either[String, Array[Byte]] = {
    val bytes = new Array[Byte](contentLength)
    var read  = 0
    while (read < contentLength) {
      val n = input.read(bytes, read, contentLength - read)
      if (n == -1) {
        return Left(s"expected $contentLength body bytes, got $read before EOF")
      }
      read += n
    }
    Right(bytes)
  }

  override def writeMessage(json: Json): Unit = {
    val content = json.noSpaces
    val bytes   = content.getBytes(StandardCharsets.UTF_8)
    val header  = s"Content-Length: ${bytes.length}\r\n\r\n"

    synchronized {
      writer.write(header.getBytes(StandardCharsets.UTF_8))
      writer.write(bytes)
      writer.flush()
    }
  }

  override def close(): Unit = {
    Try(input.close())
    Try(writer.close())
  }
}

private sealed trait HeaderLine
private object HeaderLine {
  case object Eof                  extends HeaderLine
  final case class Line(s: String) extends HeaderLine
}

private sealed trait HeaderResult
private object HeaderResult {
  case object EndOfStream                      extends HeaderResult
  final case class Failure(reason: String)     extends HeaderResult
  final case class Headers(contentLength: Int) extends HeaderResult
}

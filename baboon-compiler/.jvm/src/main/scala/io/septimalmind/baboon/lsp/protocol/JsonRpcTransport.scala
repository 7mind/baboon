package io.septimalmind.baboon.lsp.protocol

import io.circe._
import io.circe.parser._

import java.io._
import java.nio.charset.StandardCharsets
import scala.util.Try

/** JVM implementation of LSP transport using java.io streams.
  * Implements JSON-RPC message framing with Content-Length header.
  */
class JsonRpcTransport(in: InputStream, out: OutputStream) extends LspTransport {
  private val reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))
  private val writer = new BufferedOutputStream(out)

  override def readMessage(): Option[Json] = {
    Try {
      // Read headers
      var contentLength = -1
      var line          = reader.readLine()

      while (line != null && line.isEmpty) {
        line = reader.readLine()
      }

      if (line == null) {
        return None
      }

      while (line != null && line.nonEmpty) {
        if (line.startsWith("Content-Length:")) {
          contentLength = line.substring(15).trim.toInt
        }
        line = reader.readLine()
      }

      if (contentLength > 0) {
        val buffer = new Array[Char](contentLength)
        var read   = 0
        while (read < contentLength) {
          val n = reader.read(buffer, read, contentLength - read)
          if (n == -1) throw new EOFException("Unexpected end of stream")
          read += n
        }
        val content = new String(buffer)
        parse(content).toOption
      } else {
        throw new IllegalStateException("Missing Content-Length header")
      }
    }.toOption.flatten
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
    Try(reader.close())
    Try(writer.close())
  }
}

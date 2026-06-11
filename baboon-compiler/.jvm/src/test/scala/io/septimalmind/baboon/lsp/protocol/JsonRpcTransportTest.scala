package io.septimalmind.baboon.lsp.protocol

import io.circe.Json
import io.circe.parser._
import io.septimalmind.baboon.util.BLogger
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets

class JsonRpcTransportTest extends AnyWordSpec with Matchers {

  private def framed(body: String): Array[Byte] = {
    val bytes  = body.getBytes(StandardCharsets.UTF_8)
    val header = s"Content-Length: ${bytes.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8)
    header ++ bytes
  }

  private def transport(bytes: Array[Byte]): JsonRpcTransport = {
    new JsonRpcTransport(
      new ByteArrayInputStream(bytes),
      new ByteArrayOutputStream(),
      BLogger.Noop,
    )
  }

  private def json(s: String): Json = parse(s).fold(throw _, identity)

  "JsonRpcTransport.readMessage" should {

    "frame two consecutive messages where the first body carries multi-byte UTF-8 chars" in {
      // em-dash U+2014 is 3 bytes, the rocket emoji U+1F680 is 4 bytes in UTF-8.
      // A char-counting reader allocates too small a char buffer (chars < bytes)
      // and stops short, leaving trailing body bytes in the stream and corrupting
      // the framing of the SECOND message.
      val body1 = """{"jsonrpc":"2.0","method":"note","params":{"text":"em—dash 🚀"}}"""
      val body2 = """{"jsonrpc":"2.0","method":"ascii","params":{"text":"plain"}}"""

      val stream = framed(body1) ++ framed(body2)
      val t      = transport(stream)

      t.readMessage() shouldBe Some(json(body1))
      t.readMessage() shouldBe Some(json(body2))
    }

    "round-trip a pure-ASCII message" in {
      val body = """{"jsonrpc":"2.0","id":1,"method":"ping"}"""
      val t    = transport(framed(body))
      t.readMessage() shouldBe Some(json(body))
    }

    "ignore extra headers such as Content-Type" in {
      val body  = """{"jsonrpc":"2.0","method":"x"}"""
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      val raw =
        s"Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: ${bytes.length}\r\n\r\n"
          .getBytes(StandardCharsets.UTF_8) ++ bytes
      val t = transport(raw)
      t.readMessage() shouldBe Some(json(body))
    }

    "return None on EOF at stream start (clean disconnect)" in {
      val t = transport(Array.emptyByteArray)
      t.readMessage() shouldBe None
    }

    "return None without throwing when Content-Length header is missing" in {
      val raw = "Content-Type: text/plain\r\n\r\n{}".getBytes(StandardCharsets.UTF_8)
      val t   = transport(raw)
      noException should be thrownBy {
        t.readMessage() shouldBe None
      }
    }

    "return None without throwing on mid-body EOF (truncated message)" in {
      val body  = """{"jsonrpc":"2.0","method":"x"}"""
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      // Declare a longer Content-Length than the body actually provides.
      val raw = s"Content-Length: ${bytes.length + 10}\r\n\r\n".getBytes(StandardCharsets.UTF_8) ++ bytes
      val t   = transport(raw)
      noException should be thrownBy {
        t.readMessage() shouldBe None
      }
    }

    "continue to the next frame after a JSON parse failure" in {
      val garbage = "this is not json"
      val body2   = """{"jsonrpc":"2.0","method":"recovered"}"""
      val stream  = framed(garbage) ++ framed(body2)
      val t       = transport(stream)
      // First frame is correctly framed but not valid JSON -> skip, do NOT disconnect.
      t.readMessage() shouldBe Some(json(body2))
    }
  }

  "JsonRpcTransport.writeMessage -> readMessage" should {
    "round-trip multi-byte content" in {
      val msg = json("""{"jsonrpc":"2.0","method":"note","params":{"text":"em—dash 🚀"}}""")
      val buf = new ByteArrayOutputStream()
      val w   = new JsonRpcTransport(new ByteArrayInputStream(Array.emptyByteArray), buf, BLogger.Noop)
      w.writeMessage(msg)

      val r = transport(buf.toByteArray)
      r.readMessage() shouldBe Some(msg)
    }
  }
}

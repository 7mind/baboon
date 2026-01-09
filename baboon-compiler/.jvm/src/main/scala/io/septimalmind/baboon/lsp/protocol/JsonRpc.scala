package io.septimalmind.baboon.lsp.protocol

import io.circe._
import io.circe.syntax._
import io.circe.parser._

import java.io._
import java.nio.charset.StandardCharsets
import scala.util.Try

/** JSON-RPC 2.0 message types */
sealed trait JsonRpcMessage

object JsonRpcMessage {
  final case class Request(
    id: Either[Int, String],
    method: String,
    params: Option[Json]
  ) extends JsonRpcMessage

  final case class Notification(
    method: String,
    params: Option[Json]
  ) extends JsonRpcMessage

  final case class Response(
    id: Either[Int, String],
    result: Option[Json],
    error: Option[ResponseError]
  ) extends JsonRpcMessage

  final case class ResponseError(
    code: Int,
    message: String,
    data: Option[Json] = None
  )

  object ResponseError {
    implicit val encoder: Encoder[ResponseError] = Encoder.instance { e =>
      Json.obj(
        "code" -> e.code.asJson,
        "message" -> e.message.asJson,
        "data" -> e.data.asJson
      ).dropNullValues
    }
  }

  object ErrorCodes {
    val ParseError: Int = -32700
    val InvalidRequest: Int = -32600
    val MethodNotFound: Int = -32601
    val InvalidParams: Int = -32602
    val InternalError: Int = -32603
  }

  def parseMessage(json: Json): Either[String, JsonRpcMessage] = {
    val cursor = json.hcursor

    val methodOpt = cursor.get[String]("method").toOption
    val idOpt = cursor.get[Int]("id").map(Left(_))
      .orElse(cursor.get[String]("id").map(Right(_)))
      .toOption

    (methodOpt, idOpt) match {
      case (Some(method), Some(id)) =>
        // Request
        Right(Request(id, method, cursor.get[Json]("params").toOption))

      case (Some(method), None) =>
        // Notification
        Right(Notification(method, cursor.get[Json]("params").toOption))

      case (None, Some(id)) =>
        // Response
        val result = cursor.get[Json]("result").toOption
        val error = cursor.downField("error").focus.flatMap { errJson =>
          for {
            code <- errJson.hcursor.get[Int]("code").toOption
            msg <- errJson.hcursor.get[String]("message").toOption
          } yield ResponseError(code, msg, errJson.hcursor.get[Json]("data").toOption)
        }
        Right(Response(id, result, error))

      case (None, None) =>
        Left("Invalid JSON-RPC message: missing both method and id")
    }
  }

  def encodeResponse(id: Either[Int, String], result: Json): Json = {
    val idJson = id.fold(_.asJson, _.asJson)
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> idJson,
      "result" -> result
    )
  }

  def encodeError(id: Either[Int, String], error: ResponseError): Json = {
    val idJson = id.fold(_.asJson, _.asJson)
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "id" -> idJson,
      "error" -> error.asJson
    )
  }

  def encodeNotification(method: String, params: Json): Json = {
    Json.obj(
      "jsonrpc" -> "2.0".asJson,
      "method" -> method.asJson,
      "params" -> params
    )
  }
}

/** JSON-RPC message framing with Content-Length header */
class JsonRpcTransport(in: InputStream, out: OutputStream) {
  private val reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))
  private val writer = new BufferedOutputStream(out)

  def readMessage(): Option[Json] = {
    Try {
      // Read headers
      var contentLength = -1
      var line = reader.readLine()

      while (line != null && line.nonEmpty) {
        if (line.startsWith("Content-Length:")) {
          contentLength = line.substring(15).trim.toInt
        }
        line = reader.readLine()
      }

      if (contentLength > 0) {
        val buffer = new Array[Char](contentLength)
        var read = 0
        while (read < contentLength) {
          val n = reader.read(buffer, read, contentLength - read)
          if (n == -1) throw new EOFException("Unexpected end of stream")
          read += n
        }
        val content = new String(buffer)
        parse(content).toOption
      } else {
        None
      }
    }.toOption.flatten
  }

  def writeMessage(json: Json): Unit = {
    val content = json.noSpaces
    val bytes = content.getBytes(StandardCharsets.UTF_8)
    val header = s"Content-Length: ${bytes.length}\r\n\r\n"

    synchronized {
      writer.write(header.getBytes(StandardCharsets.UTF_8))
      writer.write(bytes)
      writer.flush()
    }
  }

  def close(): Unit = {
    Try(reader.close())
    Try(writer.close())
  }
}

package example

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import java.net.InetSocketAddress

import baboon.runtime.shared.{BaboonCodecContext, BaboonMethodId}
import petstore.api.PetStoreWiring

object PetStoreServer {
  private val impl                    = new PetStoreImpl
  private val ctx: BaboonCodecContext = BaboonCodecContext.Default

  def start(host: String, port: Int): Unit = {
    val server = HttpServer.create(new InetSocketAddress(host, port), 0)

    server.createContext(
      "/health",
      (exchange: HttpExchange) => {
        sendResponse(exchange, 200, "ok")
      },
    )

    server.createContext(
      "/reset",
      (exchange: HttpExchange) => {
        impl.reset()
        sendResponse(exchange, 200, "ok")
      },
    )

    server.createContext(
      "/shutdown",
      (exchange: HttpExchange) => {
        if (exchange.getRequestMethod != "POST") {
          sendResponse(exchange, 405, "Method Not Allowed")
        } else {
          sendResponse(exchange, 200, "ok")
          val stopper = new Thread(
            () => {
              server.stop(0)
              System.exit(0)
            }
          )
          stopper.setDaemon(false)
          stopper.start()
        }
      },
    )

    server.createContext(
      "/",
      (exchange: HttpExchange) => {
        val path  = exchange.getRequestURI.getPath
        val parts = path.split("/").filter(_.nonEmpty)
        if (parts.length == 2) {
          val service     = parts(0)
          val method      = parts(1)
          val contentType = Option(exchange.getRequestHeaders.getFirst("Content-Type")).getOrElse("")
          val rawBody     = exchange.getRequestBody.readAllBytes()
          try {
            val methodId = BaboonMethodId(service, method)
            if (contentType.startsWith("application/octet-stream")) {
              val result = PetStoreWiring.invokeUeba(methodId, rawBody, impl, ctx)
              exchange.getResponseHeaders.set("Content-Type", "application/octet-stream")
              sendResponseBytes(exchange, 200, result)
            } else {
              val body   = new String(rawBody, "UTF-8")
              val result = PetStoreWiring.invokeJson(methodId, body, impl, ctx)
              exchange.getResponseHeaders.set("Content-Type", "application/json")
              sendResponse(exchange, 200, result)
            }
          } catch {
            case e: Exception =>
              sendResponse(exchange, 500, e.getMessage)
          }
        } else {
          sendResponse(exchange, 404, "Not Found")
        }
      },
    )

    server.setExecutor(null)
    server.start()
    println(s"Listening on $host:$port")
  }

  private def sendResponse(exchange: HttpExchange, code: Int, body: String): Unit = {
    sendResponseBytes(exchange, code, body.getBytes("UTF-8"))
  }

  private def sendResponseBytes(exchange: HttpExchange, code: Int, bytes: Array[Byte]): Unit = {
    exchange.sendResponseHeaders(code, bytes.length.toLong)
    val os = exchange.getResponseBody
    os.write(bytes)
    os.close()
  }
}

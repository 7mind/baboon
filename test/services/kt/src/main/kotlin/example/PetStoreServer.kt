package example

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonMethodId
import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer
import petstore.api.PetStoreWiring
import java.net.InetSocketAddress

object PetStoreServer {
    fun start(host: String, port: Int) {
        val impl = PetStoreImpl()
        val ctx = BaboonCodecContext.Default

        val server = HttpServer.create(InetSocketAddress(host, port), 0)

        server.createContext("/health") { exchange ->
            exchange.sendText(200, "ok")
        }

        server.createContext("/reset") { exchange ->
            if (exchange.requestMethod == "POST") {
                impl.reset()
                exchange.sendText(200, "ok")
            } else {
                exchange.sendText(405, "Method Not Allowed")
            }
        }

        server.createContext("/shutdown") { exchange ->
            if (exchange.requestMethod == "POST") {
                exchange.sendText(200, "ok")
                Thread {
                    server.stop(0)
                    System.exit(0)
                }.start()
            } else {
                exchange.sendText(405, "Method Not Allowed")
            }
        }

        server.createContext("/") { exchange ->
            if (exchange.requestMethod != "POST") {
                exchange.sendText(405, "Method Not Allowed")
                return@createContext
            }

            val parts = exchange.requestURI.path.split("/").filter { it.isNotEmpty() }
            if (parts.size != 2) {
                exchange.sendText(404, "Not Found")
                return@createContext
            }

            val serviceName = parts[0]
            val methodName = parts[1]
            val body = exchange.requestBody.bufferedReader().readText()

            try {
                val methodId = BaboonMethodId(serviceName, methodName)
                val result = PetStoreWiring.invokeJson(methodId, body, impl, ctx)
                exchange.responseHeaders.add("Content-Type", "application/json")
                exchange.sendText(200, result)
            } catch (e: Exception) {
                exchange.sendText(500, e.message ?: "Internal Server Error")
            }
        }

        server.executor = null

        Runtime.getRuntime().addShutdownHook(Thread {
            server.stop(0)
        })

        sun.misc.Signal.handle(sun.misc.Signal("TERM")) {
            server.stop(0)
        }

        println("Listening on $host:$port")
        System.out.flush()
        server.start()
    }

    private fun HttpExchange.sendText(code: Int, body: String) {
        val bytes = body.toByteArray(Charsets.UTF_8)
        sendResponseHeaders(code, bytes.size.toLong())
        responseBody.use { it.write(bytes) }
    }
}

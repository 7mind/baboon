package io.septimalmind.baboon.lsp

import io.septimalmind.baboon.lsp.protocol._

import java.net.ServerSocket
import scala.annotation.tailrec

class LspLauncher(
  server: BaboonLanguageServer,
  port: Option[Int]
) {

  def launch(): Unit = {
    port match {
      case Some(p) => launchTcp(p)
      case None    => launchStdio()
    }
  }

  private def launchStdio(): Unit = {
    val transport = new JsonRpcTransport(System.in, System.out)
    server.setTransport(transport)
    runMessageLoop(transport)
  }

  private def launchTcp(port: Int): Unit = {
    val serverSocket = new ServerSocket(port)
    System.err.println(s"Baboon LSP server listening on port $port")

    val socket = serverSocket.accept()
    System.err.println("Client connected")

    val transport = new JsonRpcTransport(socket.getInputStream, socket.getOutputStream)
    server.setTransport(transport)

    try {
      runMessageLoop(transport)
    } finally {
      transport.close()
      socket.close()
      serverSocket.close()
    }
  }

  @tailrec
  private def runMessageLoop(transport: JsonRpcTransport): Unit = {
    transport.readMessage() match {
      case Some(json) =>
        JsonRpcMessage.parseMessage(json) match {
          case Right(msg) =>
            server.handleMessage(msg)
          case Left(error) =>
            System.err.println(s"Failed to parse JSON-RPC message: $error")
        }
        runMessageLoop(transport)

      case None =>
        // End of stream or read error - exit
        System.err.println("Connection closed")
    }
  }
}

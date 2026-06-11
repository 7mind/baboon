package io.septimalmind.baboon.lsp

import io.septimalmind.baboon.lsp.protocol._
import io.septimalmind.baboon.util.BLogger

import java.net.ServerSocket
import scala.annotation.tailrec

class LspLauncher(
  server: BaboonLanguageServer,
  port: Option[Int],
  logger: BLogger,
) {

  def launch(): Unit = {
    port match {
      case Some(p) => launchTcp(p)
      case None    => launchStdio()
    }
  }

  private def launchStdio(): Unit = {
    val transport = new JsonRpcTransport(System.in, System.out, logger)
    server.setTransport(transport)
    runMessageLoop(transport)
  }

  private def launchTcp(port: Int): Unit = {
    val serverSocket = new ServerSocket(port)
    logger.message(LspLogging.Context, s"Baboon LSP server listening on port $port")

    val socket = serverSocket.accept()
    logger.message(LspLogging.Context, "Client connected")

    val transport = new JsonRpcTransport(socket.getInputStream, socket.getOutputStream, logger)
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
  private def runMessageLoop(transport: LspTransport): Unit = {
    transport.readMessage() match {
      case Some(json) =>
        try {
          JsonRpcMessage.parseMessage(json) match {
            case Right(msg) =>
              server.handleMessage(msg)
            case Left(error) =>
              logger.message(LspLogging.Context, s"Failed to parse JSON-RPC message: $error")
          }
        } catch {
          case t: VirtualMachineError => throw t
          case t: Throwable =>
            logger.message(LspLogging.Context, s"Unhandled exception from message dispatch: ${t.getClass.getName}: ${t.getMessage}")
        }
        runMessageLoop(transport)

      case None =>
        // End of stream or read error - exit
        logger.message(LspLogging.Context, "Connection closed")
    }
  }
}

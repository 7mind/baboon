package io.septimalmind.baboon.lsp

import org.eclipse.lsp4j.launch.LSPLauncher

import java.net.ServerSocket

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
    val launcher = LSPLauncher.createServerLauncher(
      server,
      System.in,
      System.out
    )

    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
  }

  private def launchTcp(port: Int): Unit = {
    val serverSocket = new ServerSocket(port)
    System.err.println(s"Baboon LSP server listening on port $port")

    val socket = serverSocket.accept()
    System.err.println("Client connected")

    val launcher = LSPLauncher.createServerLauncher(
      server,
      socket.getInputStream,
      socket.getOutputStream
    )

    server.connect(launcher.getRemoteProxy)
    launcher.startListening().get()
  }
}

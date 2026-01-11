package io.septimalmind.baboon.lsp.protocol

import io.circe.Json

/** Platform-agnostic LSP transport abstraction.
  *
  * Handles JSON-RPC message framing with Content-Length headers.
  * JVM implementation uses java.io streams.
  * JS implementation would use Node.js streams or WebSocket.
  */
trait LspTransport {

  /** Read the next JSON-RPC message, blocking until available.
    * Returns None on end-of-stream or read error.
    */
  def readMessage(): Option[Json]

  /** Write a JSON-RPC message with proper Content-Length framing */
  def writeMessage(json: Json): Unit

  /** Close the transport and release resources */
  def close(): Unit
}

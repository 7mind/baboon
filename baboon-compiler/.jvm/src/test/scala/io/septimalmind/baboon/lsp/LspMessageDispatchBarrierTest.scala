package io.septimalmind.baboon.lsp

import io.circe.Json
import io.circe.syntax._
import io.septimalmind.baboon.lsp.features._
import io.septimalmind.baboon.lsp.protocol._
import io.septimalmind.baboon.lsp.state._
import io.septimalmind.baboon.lsp.util.{JvmPathOps, PositionConverter}
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BLogger
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

/** Verifies that per-message Throwable barriers in [[BaboonLanguageServer.handleMessage]]
  * prevent provider crashes from killing the server process.
  *
  * D15 requirements tested:
  *  1. A request handler throwing AssertionError yields a JSON-RPC error response
  *     with code -32603 (InternalError) carrying the exception class name.
  *  2. The subsequent message is still processed after a throwing request.
  *  3. A notification handler throwing AssertionError produces NO response; the loop continues.
  *  4. At least one thrown value used in the tests is a java.lang.Error (not Exception).
  *  5. The intentional 'exit' notification path is not suppressed.
  */
class LspMessageDispatchBarrierTest extends AnyWordSpec with Matchers {

  // ---------------------------------------------------------------------------
  // Stub transport: feeds scripted messages; records all writes.
  // ---------------------------------------------------------------------------

  private final class ScriptedTransport(messages: List[Json]) extends LspTransport {
    private val queue                          = mutable.Queue.from(messages)
    private val _written: mutable.Buffer[Json] = mutable.Buffer.empty

    def writtenMessages: List[Json] = _written.toList

    override def readMessage(): Option[Json]    = if (queue.nonEmpty) Some(queue.dequeue()) else None
    override def writeMessage(json: Json): Unit = { val _ = _written.addOne(json) }
    override def close(): Unit                  = ()
  }

  // ---------------------------------------------------------------------------
  // Stub LspCompiler: always fails with a harmless IncludeNotFound error.
  // ---------------------------------------------------------------------------

  private final class NoopCompiler extends LspCompiler {
    def reload(inputs: Seq[BaboonParser.Input], previous: Option[BaboonFamily]): Either[NEList[BaboonIssue], BaboonFamily] =
      Left(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound("noop"))))
  }

  // ---------------------------------------------------------------------------
  // Common fixtures
  // ---------------------------------------------------------------------------

  private val pathOps      = JvmPathOps
  private val logger       = BLogger.Noop
  private val posConverter = new PositionConverter(pathOps)

  private val noopInputProvider: InputProvider = new InputProvider {
    def getWorkspaceInputs: Seq[BaboonParser.Input] = Seq.empty
    def pathToUri(path: String): String             = path
    def uriToPath(uri: String): String              = uri
  }

  /** Build a minimal BaboonLanguageServer. The hoverProvider is provided by the caller
    * so tests can rig it to throw.
    */
  private def buildServer(
    hover: HoverProvider,
    doc: DocumentState,
    exitCb: () => Unit = () => (),
  ): BaboonLanguageServer = {
    val ws = new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger)
    new BaboonLanguageServer(
      documentState          = doc,
      workspaceState         = ws,
      diagnosticsProvider    = new DiagnosticsProvider(posConverter),
      definitionProvider     = new DefinitionProvider(doc, ws, posConverter),
      hoverProvider          = hover,
      completionProvider     = new CompletionProvider(doc, ws, logger),
      documentSymbolProvider = new DocumentSymbolProvider(ws, posConverter, pathOps, logger),
      exitCallback           = exitCb,
      logger                 = logger,
    )
  }

  /** Attach a scripted transport and process the given messages through handleMessage. */
  private def runMessages(server: BaboonLanguageServer, messages: List[Json]): List[Json] = {
    val transport = new ScriptedTransport(messages)
    server.setTransport(transport)
    messages.foreach {
      json =>
        JsonRpcMessage.parseMessage(json) match {
          case Right(msg) => server.handleMessage(msg)
          case Left(e)    => fail(s"Scripted message failed to parse: $e")
        }
    }
    transport.writtenMessages
  }

  // ---------------------------------------------------------------------------
  // JSON-RPC message builders
  // ---------------------------------------------------------------------------

  private def hoverRequest(id: Int): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "method"  -> "textDocument/hover".asJson,
    "params" -> Json.obj(
      "textDocument" -> Json.obj("uri" -> "file:///x.baboon".asJson),
      "position"     -> Json.obj("line" -> 0.asJson, "character" -> 0.asJson),
    ),
  )

  private def shutdownRequest(id: Int): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "method"  -> "shutdown".asJson,
  )

  private def didOpenNotification(uri: String = "file:///x.baboon", text: String = "model foo version 0"): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "method"  -> "textDocument/didOpen".asJson,
    "params" -> Json.obj(
      "textDocument" -> Json.obj(
        "uri"        -> uri.asJson,
        "languageId" -> "baboon".asJson,
        "version"    -> 1.asJson,
        "text"       -> text.asJson,
      )
    ),
  )

  private def exitNotification(): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "method"  -> "exit".asJson,
  )

  // ---------------------------------------------------------------------------
  // Assertion helpers
  // ---------------------------------------------------------------------------

  private def assertInternalError(json: Json, expectedId: Int): Unit = {
    json.hcursor.get[String]("jsonrpc").toOption shouldBe Some("2.0")
    json.hcursor.get[Int]("id").toOption shouldBe Some(expectedId)
    json.hcursor.downField("error").get[Int]("code").toOption shouldBe Some(JsonRpcMessage.ErrorCodes.InternalError)
  }

  private def assertSuccessResponse(json: Json, expectedId: Int): Unit = {
    json.hcursor.get[String]("jsonrpc").toOption shouldBe Some("2.0")
    json.hcursor.get[Int]("id").toOption shouldBe Some(expectedId)
    json.hcursor.downField("error").focus shouldBe None
  }

  // ===========================================================================
  // Tests — request barrier
  // ===========================================================================

  "BaboonLanguageServer request barrier (D15)" should {

    "respond with -32603 InternalError when request handler throws AssertionError" in {
      // AssertionError is a java.lang.Error subtype (NOT an Exception).
      // The barrier must catch it and return a JSON-RPC InternalError response.
      val doc = new DocumentState(pathOps)
      val hover = new HoverProvider(doc, new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger), logger) {
        override def getHover(uri: String, pos: Position): Option[Hover] =
          throw new AssertionError("provider crashed — AssertionError is an Error, not an Exception")
      }
      val server  = buildServer(hover, doc)
      val written = runMessages(server, List(hoverRequest(id = 1), shutdownRequest(id = 2)))

      written.size should be >= 2
      assertInternalError(written(0), expectedId = 1)
    }

    "still process the subsequent message after a throwing request" in {
      val doc = new DocumentState(pathOps)
      val hover = new HoverProvider(doc, new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger), logger) {
        override def getHover(uri: String, pos: Position): Option[Hover] =
          throw new AssertionError("blown up")
      }
      val server  = buildServer(hover, doc)
      val written = runMessages(server, List(hoverRequest(id = 1), shutdownRequest(id = 2)))

      val shutdownOk = written.find {
        j =>
          j.hcursor.get[Int]("id").toOption.contains(2) &&
          j.hcursor.downField("error").focus.isEmpty
      }
      assert(shutdownOk.isDefined, s"Expected a successful shutdown response (id=2), got: $written")
    }

    "include the exception class name in the InternalError message field" in {
      val doc = new DocumentState(pathOps)
      val hover = new HoverProvider(doc, new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger), logger) {
        override def getHover(uri: String, pos: Position): Option[Hover] =
          throw new AssertionError("detailed reason")
      }
      val server  = buildServer(hover, doc)
      val written = runMessages(server, List(hoverRequest(id = 1)))

      val errorMsg = written.head.hcursor.downField("error").get[String]("message").toOption
      errorMsg shouldBe defined
      errorMsg.get should include("AssertionError")
    }

    "confirm thrown value is java.lang.Error (not Exception) — barrier must cover Error subtypes" in {
      // Type-hierarchy assertion: AssertionError <: Error, NOT <: Exception.
      val err: Throwable = new AssertionError("type hierarchy check")
      err.isInstanceOf[java.lang.Error] shouldBe true
      err.isInstanceOf[Exception] shouldBe false

      // The barrier catches it (it's not a VirtualMachineError, so it's not rethrown):
      val doc = new DocumentState(pathOps)
      val hover = new HoverProvider(doc, new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger), logger) {
        override def getHover(uri: String, pos: Position): Option[Hover] = throw err
      }
      val server  = buildServer(hover, doc)
      val written = runMessages(server, List(hoverRequest(id = 1)))

      written should have size 1
      assertInternalError(written.head, expectedId = 1)
    }
  }

  // ===========================================================================
  // Tests — notification barrier
  // ===========================================================================

  "BaboonLanguageServer notification barrier (D15)" should {

    "survive a throwing notification handler without writing an error response for the notification" in {
      // JSON-RPC spec §5.1: a notification must NEVER produce a response, even on error.
      // When the handler throws, the barrier must log-and-continue, writing nothing for the notification.
      //
      // We trigger the throw via a DocumentState whose `open` method throws AssertionError.
      // 'textDocument/didOpen' calls documentState.open(uri, text), so it crashes there.

      val throwingDoc = new DocumentState(pathOps) {
        override def open(uri: String, content: String): Unit =
          throw new AssertionError("didOpen handler crash")
      }
      val ws = new WorkspaceState(throwingDoc, new NoopCompiler, noopInputProvider, pathOps, logger)
      val server = new BaboonLanguageServer(
        documentState          = throwingDoc,
        workspaceState         = ws,
        diagnosticsProvider    = new DiagnosticsProvider(posConverter),
        definitionProvider     = new DefinitionProvider(throwingDoc, ws, posConverter),
        hoverProvider          = new HoverProvider(throwingDoc, ws, logger),
        completionProvider     = new CompletionProvider(throwingDoc, ws, logger),
        documentSymbolProvider = new DocumentSymbolProvider(ws, posConverter, pathOps, logger),
        exitCallback           = () => (),
        logger                 = logger,
      )

      // Send: (1) didOpen notification (crashes) + (2) shutdown request (succeeds)
      val written = runMessages(server, List(didOpenNotification(), shutdownRequest(id = 10)))

      // No error response must have been written for the notification.
      // A valid error response has an 'error' field and an 'id'. Notifications have no id,
      // so any error response the server might (incorrectly) write would lack a matching id.
      // Verify: no 'error' object found on any message that is NOT the shutdown response.
      val spuriousError = written.find {
        j =>
          j.hcursor.downField("error").focus.isDefined &&
          !j.hcursor.get[Int]("id").toOption.contains(10)
      }
      spuriousError shouldBe None
    }

    "continue to process the shutdown request after a throwing notification" in {
      val throwingDoc = new DocumentState(pathOps) {
        override def open(uri: String, content: String): Unit =
          throw new AssertionError("didOpen boom")
      }
      val ws = new WorkspaceState(throwingDoc, new NoopCompiler, noopInputProvider, pathOps, logger)
      val server = new BaboonLanguageServer(
        documentState          = throwingDoc,
        workspaceState         = ws,
        diagnosticsProvider    = new DiagnosticsProvider(posConverter),
        definitionProvider     = new DefinitionProvider(throwingDoc, ws, posConverter),
        hoverProvider          = new HoverProvider(throwingDoc, ws, logger),
        completionProvider     = new CompletionProvider(throwingDoc, ws, logger),
        documentSymbolProvider = new DocumentSymbolProvider(ws, posConverter, pathOps, logger),
        exitCallback           = () => (),
        logger                 = logger,
      )

      val written = runMessages(server, List(didOpenNotification(), shutdownRequest(id = 10)))

      val shutdownOk = written.find {
        j =>
          j.hcursor.get[Int]("id").toOption.contains(10) &&
          j.hcursor.downField("error").focus.isEmpty
      }
      assert(shutdownOk.isDefined, s"Expected shutdown response (id=10) after throwing notification, got: $written")
    }
  }

  // ===========================================================================
  // Tests — intentional exit path must not be suppressed
  // ===========================================================================

  "BaboonLanguageServer exit notification" should {

    "invoke exitCallback — the intentional process-exit path is not swallowed by the barrier" in {
      val doc = new DocumentState(pathOps)
      var exitCalled = false
      val hover = new HoverProvider(doc, new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger), logger)
      val server  = buildServer(hover, doc, exitCb = () => { exitCalled = true })
      runMessages(server, List(exitNotification()))

      exitCalled shouldBe true
    }
  }
}

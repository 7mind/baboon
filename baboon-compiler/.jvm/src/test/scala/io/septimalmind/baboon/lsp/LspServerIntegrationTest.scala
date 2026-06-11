package io.septimalmind.baboon.lsp

import io.circe.Json
import io.circe.parser.parse
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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets

/** System-level regression suite (T31): verifies that the byte-accurate LSP framing introduced in
  * T29 (JsonRpcTransport) survives a raw UTF-8 wire sequence end-to-end through the REAL message
  * loop.
  *
  * The pre-T29 server desynchronised after receiving a `textDocument/didOpen` whose body contained
  * multi-byte UTF-8 characters (em-dash U+2014, 3 bytes; and a 4-byte supplementary-plane
  * character). The char-counting reader left trailing bytes in the stream which corrupted the next
  * frame; the subsequent `textDocument/documentSymbol` was never answered — the observed leg-B
  * failure ("Connection closed", exit 0) from the probe.
  *
  * This test:
  *   - Assembles the REAL server in-process using the same fixture pattern as
  *     [[LspMessageDispatchBarrierTest]] and [[io.septimalmind.baboon.lsp.features.LspFeaturesTest]].
  *   - Uses the REAL [[JsonRpcTransport]] over a [[ByteArrayInputStream]] holding the full scripted
  *     wire and a [[ByteArrayOutputStream]] capturing server responses — deterministic, no timing.
  *   - Drives the REAL message loop via `LspLauncher.runMessageLoop` (widened to private[lsp]).
  *   - Asserts that the `documentSymbol` response is present (the pre-fix server would never
  *     answer it), that `initialize` and `shutdown` responses are present, and that the exit
  *     callback is NOT invoked.
  *
  * Worklog note (fail-first reasoning): against pre-T29 code the documentSymbol response would be
  * absent because the char-Reader over-read the didOpen body by (byteCount − charCount) bytes
  * (exactly the surplus produced by the multi-byte characters asserted below), leaving those bytes
  * as phantom header bytes for the next frame and causing a framing failure. T29 replaced the
  * Reader with a raw byte read followed by UTF-8 decode, eliminating the surplus.
  */
class LspServerIntegrationTest extends AnyWordSpec with Matchers {

  // ---------------------------------------------------------------------------
  // Minimal no-op compiler — always fails with IncludeNotFound so the server
  // does not need real .baboon files on disk.
  // ---------------------------------------------------------------------------

  private final class NoopCompiler extends LspCompiler {
    def reload(
      inputs: Seq[BaboonParser.Input],
      previous: Option[BaboonFamily],
    ): Either[NEList[BaboonIssue], BaboonFamily] =
      Left(NEList(BaboonIssue.Parser(ParserIssue.IncludeNotFound("noop"))))
  }

  // ---------------------------------------------------------------------------
  // Common fixtures (same pattern as LspMessageDispatchBarrierTest)
  // ---------------------------------------------------------------------------

  private val pathOps      = JvmPathOps
  private val logger       = BLogger.Noop
  private val posConverter = new PositionConverter(pathOps)

  private val noopInputProvider: InputProvider = new InputProvider {
    def getWorkspaceInputs: Seq[BaboonParser.Input] = Seq.empty
    def pathToUri(path: String): String             = path
  }

  private def buildServer(exitCb: () => Unit): BaboonLanguageServer = {
    val doc = new DocumentState(pathOps)
    val ws  = new WorkspaceState(doc, new NoopCompiler, noopInputProvider, pathOps, logger)
    new BaboonLanguageServer(
      documentState          = doc,
      workspaceState         = ws,
      diagnosticsProvider    = new DiagnosticsProvider(posConverter),
      definitionProvider     = new DefinitionProvider(doc, ws, posConverter),
      hoverProvider          = new HoverProvider(doc, ws, logger),
      completionProvider     = new CompletionProvider(doc, ws, logger),
      documentSymbolProvider = new DocumentSymbolProvider(ws, posConverter, pathOps, logger),
      exitCallback           = exitCb,
      logger                 = logger,
    )
  }

  // ---------------------------------------------------------------------------
  // Wire helpers: produce byte-accurate Content-Length framed messages.
  // Content-Length counts UTF-8 BYTES, not chars.
  // ---------------------------------------------------------------------------

  private def frame(json: Json): Array[Byte] = {
    val body    = json.noSpaces.getBytes(StandardCharsets.UTF_8)
    val header  = s"Content-Length: ${body.length}\r\n\r\n".getBytes(StandardCharsets.UTF_8)
    header ++ body
  }

  // ---------------------------------------------------------------------------
  // Scripted LSP messages (the leg-B probe sequence)
  // ---------------------------------------------------------------------------

  /** The document text used in didOpen. Contains:
    *   - em-dash U+2014 (3 bytes in UTF-8)
    *   - mathematical fraktur A U+1D504 (4 bytes in UTF-8, a supplementary-plane char)
    * so UTF-8 byte count > char count — the test asserts this inequality explicitly.
    */
  private val didOpenText: String = "model foo version 0\nen—dash 𝔄 end"

  private def initializeRequest(id: Int): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "method"  -> "initialize".asJson,
    "params" -> Json.obj(
      "processId"        -> Json.Null,
      "rootUri"          -> Json.Null,
      "workspaceFolders" -> Json.Null,
      "capabilities"     -> Json.obj(),
    ),
  )

  private def initializedNotification(): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "method"  -> "initialized".asJson,
    "params"  -> Json.obj(),
  )

  private def didOpenNotification(uri: String, text: String): Json = Json.obj(
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

  private def documentSymbolRequest(id: Int, uri: String): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "method"  -> "textDocument/documentSymbol".asJson,
    "params" -> Json.obj(
      "textDocument" -> Json.obj("uri" -> uri.asJson)
    ),
  )

  private def shutdownRequest(id: Int): Json = Json.obj(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "method"  -> "shutdown".asJson,
  )

  // ---------------------------------------------------------------------------
  // Wire helper: parse all framed responses from the captured output stream.
  // ---------------------------------------------------------------------------

  private def parseResponses(out: ByteArrayOutputStream): List[Json] = {
    val bytes = out.toByteArray
    val input = new ByteArrayInputStream(bytes)
    val t     = new JsonRpcTransport(input, new ByteArrayOutputStream(), BLogger.Noop)

    @scala.annotation.tailrec
    def loop(acc: List[Json]): List[Json] =
      t.readMessage() match {
        case Some(j) => loop(acc :+ j)
        case None    => acc
      }

    loop(Nil)
  }

  // ---------------------------------------------------------------------------
  // The integration test
  // ---------------------------------------------------------------------------

  "LspServerIntegrationTest (T31) raw-UTF-8 didOpen via real JsonRpcTransport" should {

    "complete the initialize → initialized → didOpen(multi-byte) → documentSymbol → shutdown sequence" in {

      // --- Precondition: didOpen body is multi-byte (bytes > chars) ---
      val docUri                   = "file:///test.baboon"
      val didOpenBodyJson          = didOpenNotification(docUri, didOpenText)
      val didOpenBodyBytes: Array[Byte] = didOpenBodyJson.noSpaces.getBytes(StandardCharsets.UTF_8)
      // char count of the JSON body string (Java chars, including surrogate pairs as 2 chars each)
      val didOpenBodyChars: Int    = didOpenBodyJson.noSpaces.length

      withClue("The didOpen JSON body must have more UTF-8 bytes than Java chars (proves multi-byte encoding)") {
        didOpenBodyBytes.length should be > didOpenBodyChars
      }

      // --- Build the wire: all 5 messages concatenated as raw UTF-8 byte frames ---
      val wire: Array[Byte] =
        frame(initializeRequest(id = 1)) ++
        frame(initializedNotification()) ++
        frame(didOpenNotification(docUri, didOpenText)) ++
        frame(documentSymbolRequest(id = 2, uri = docUri)) ++
        frame(shutdownRequest(id = 3))

      // --- Assemble the real server ---
      var exitCalled = false
      val server     = buildServer(exitCb = () => { exitCalled = true })

      val inStream  = new ByteArrayInputStream(wire)
      val outStream = new ByteArrayOutputStream()

      val transport = new JsonRpcTransport(inStream, outStream, logger)
      server.setTransport(transport)

      // --- Drive the real message loop (LspLauncher.runMessageLoop widened to private[lsp]) ---
      val launcher = new LspLauncher(server, port = None, logger)
      launcher.runMessageLoop(transport)

      // --- Parse all responses from the output stream ---
      val responses = parseResponses(outStream)

      // --- Assertions ---

      // 1. The loop should have ended cleanly at EOF (not prematurely).
      //    We verify this indirectly: if the loop desynchronised after didOpen
      //    (pre-T29 behavior), the documentSymbol response would be missing.

      val initResponse = responses.find(j => j.hcursor.get[Int]("id").toOption.contains(1))
      withClue("initialize response must be present") {
        initResponse shouldBe defined
      }
      withClue("initialize response must have no error") {
        initResponse.get.hcursor.downField("error").focus shouldBe None
      }

      // 2. documentSymbol response MUST be present.
      //    Pre-T29: after the multi-byte didOpen frame the stream was desynchronised,
      //    the documentSymbol frame was corrupt, the server never answered — "Connection closed".
      val docSymbolResponse = responses.find(j => j.hcursor.get[Int]("id").toOption.contains(2))
      withClue(
        "documentSymbol response must be present — its absence is the pre-T29 regression symptom " +
        "(framing desync after multi-byte didOpen leaves the documentSymbol frame unreadable)"
      ) {
        docSymbolResponse shouldBe defined
      }
      withClue("documentSymbol response must have no error") {
        docSymbolResponse.get.hcursor.downField("error").focus shouldBe None
      }

      // 3. shutdown response must be present.
      val shutdownResponse = responses.find(j => j.hcursor.get[Int]("id").toOption.contains(3))
      withClue("shutdown response must be present") {
        shutdownResponse shouldBe defined
      }

      // 4. exit callback must NOT have been invoked — we sent no 'exit' notification.
      withClue("exit callback must not be called") {
        exitCalled shouldBe false
      }

      // 5. Loop ended at stream EOF (deterministic — no blocking reads on empty stream).
      //    Verified by the fact that runMessageLoop returned at all.
    }

    "verify: didOpen body UTF-8 byte count exceeds char count (multi-byte wire invariant)" in {
      // Standalone assertion required by the task spec — proves the wire actually
      // exercises the multi-byte path and is not accidentally ASCII-only.
      val text      = didOpenText
      val asBytes   = text.getBytes(StandardCharsets.UTF_8)
      val charCount = text.length // Java char count (surrogate pairs count as 2)

      withClue(s"text='$text', bytes=${asBytes.length}, chars=$charCount") {
        asBytes.length should be > charCount
      }
    }
  }
}

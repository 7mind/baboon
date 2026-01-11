package io.septimalmind.baboon.lsp

import io.circe._
import io.circe.syntax._
import io.septimalmind.baboon.lsp.features._
import io.septimalmind.baboon.lsp.protocol._
import io.septimalmind.baboon.lsp.state._

class BaboonLanguageServer(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  diagnosticsProvider: DiagnosticsProvider,
  definitionProvider: DefinitionProvider,
  hoverProvider: HoverProvider,
  completionProvider: CompletionProvider,
  documentSymbolProvider: DocumentSymbolProvider,
  exitCallback: () => Unit
) {

  private var transport: LspTransport = _

  def setTransport(t: LspTransport): Unit = {
    transport = t
  }

  def handleMessage(msg: JsonRpcMessage): Unit = msg match {
    case JsonRpcMessage.Request(id, method, params) =>
      val result = handleRequest(method, params)
      result match {
        case Right(json) =>
          transport.writeMessage(JsonRpcMessage.encodeResponse(id, json))
        case Left(error) =>
          transport.writeMessage(JsonRpcMessage.encodeError(id, error))
      }

    case JsonRpcMessage.Notification(method, params) =>
      handleNotification(method, params)

    case _: JsonRpcMessage.Response =>
      // We don't expect responses from the client in this simple implementation
      ()
  }

  private def handleRequest(method: String, params: Option[Json]): Either[JsonRpcMessage.ResponseError, Json] = {
    method match {
      case "initialize" =>
        params.flatMap(_.as[InitializeParams].toOption) match {
          case Some(p) => Right(initialize(p).asJson)
          case None    => Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.InvalidParams, "Invalid initialize params"))
        }

      case "shutdown" =>
        Right(Json.Null)

      case "textDocument/definition" =>
        params.flatMap(_.as[DefinitionParams].toOption) match {
          case Some(p) =>
            val locations = definitionProvider.findDefinition(p.textDocument.uri, p.position)
            Right(locations.asJson)
          case None =>
            Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.InvalidParams, "Invalid definition params"))
        }

      case "textDocument/hover" =>
        params.flatMap(_.as[HoverParams].toOption) match {
          case Some(p) =>
            val hover = hoverProvider.getHover(p.textDocument.uri, p.position)
            Right(hover.asJson)
          case None =>
            Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.InvalidParams, "Invalid hover params"))
        }

      case "textDocument/completion" =>
        params.flatMap(_.as[CompletionParams].toOption) match {
          case Some(p) =>
            val items = completionProvider.getCompletions(p.textDocument.uri, p.position)
            Right(items.asJson)
          case None =>
            Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.InvalidParams, "Invalid completion params"))
        }

      case "textDocument/documentSymbol" =>
        params.flatMap(_.as[DocumentSymbolParams].toOption) match {
          case Some(p) =>
            val symbols = documentSymbolProvider.getSymbols(p.textDocument.uri)
            Right(symbols.asJson)
          case None =>
            Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.InvalidParams, "Invalid documentSymbol params"))
        }

      case _ =>
        Left(JsonRpcMessage.ResponseError(JsonRpcMessage.ErrorCodes.MethodNotFound, s"Method not found: $method"))
    }
  }

  private def handleNotification(method: String, params: Option[Json]): Unit = {
    method match {
      case "initialized" =>
        workspaceState.scanWorkspace()
        publishAllDiagnostics()

      case "exit" =>
        exitCallback()

      case "textDocument/didOpen" =>
        params.flatMap(_.as[DidOpenTextDocumentParams].toOption).foreach { p =>
          documentState.open(p.textDocument.uri, p.textDocument.text)
          recompileAndPublishDiagnostics()
        }

      case "textDocument/didChange" =>
        params.flatMap(_.as[DidChangeTextDocumentParams].toOption).foreach { p =>
          p.contentChanges.lastOption.foreach { change =>
            documentState.update(p.textDocument.uri, change.text)
          }
        }

      case "textDocument/didSave" =>
        params.flatMap(_.as[DidSaveTextDocumentParams].toOption).foreach { p =>
          p.text.foreach(text => documentState.update(p.textDocument.uri, text))
          recompileAndPublishDiagnostics()
        }

      case "textDocument/didClose" =>
        params.flatMap(_.as[DidCloseTextDocumentParams].toOption).foreach { p =>
          documentState.close(p.textDocument.uri)
          publishDiagnostics(p.textDocument.uri, Seq.empty)
        }

      case _ =>
        // Ignore unknown notifications
        ()
    }
  }

  private def initialize(params: InitializeParams): InitializeResult = {
    params.workspaceFolders.foreach { folders =>
      folders.foreach { folder =>
        workspaceState.addWorkspaceFolder(folder.uri)
      }
    }
    params.rootUri.foreach(workspaceState.addWorkspaceFolder)

    val capabilities = ServerCapabilities(
      textDocumentSync = Some(TextDocumentSyncOptions(
        openClose = true,
        change = TextDocumentSyncKind.Full,
        save = Some(SaveOptions(includeText = true))
      )),
      definitionProvider = Some(true),
      hoverProvider = Some(true),
      completionProvider = Some(CompletionOptions(
        triggerCharacters = Some(Seq(".", ":", "[")),
        resolveProvider = Some(false)
      )),
      documentSymbolProvider = Some(true)
    )

    InitializeResult(capabilities)
  }

  private def recompileAndPublishDiagnostics(): Unit = {
    val compilationResult = workspaceState.recompile()
    publishDiagnosticsForResult(compilationResult)
  }

  private def publishAllDiagnostics(): Unit = {
    val compilationResult = workspaceState.getLastCompilationResult
    publishDiagnosticsForResult(compilationResult)
  }

  private def publishDiagnosticsForResult(compilationResult: CompilationResult): Unit = {
    // Publish diagnostics for all files that have issues
    compilationResult.fileIssues.keys.foreach { uri =>
      val diagnostics = diagnosticsProvider.getDiagnostics(uri, compilationResult)
      publishDiagnostics(uri, diagnostics)
    }

    // Clear diagnostics for open documents that no longer have issues
    documentState.getOpenDocuments.foreach { uri =>
      if (!compilationResult.fileIssues.contains(uri)) {
        publishDiagnostics(uri, Seq.empty)
      }
    }
  }

  private def publishDiagnostics(uri: String, diagnostics: Seq[Diagnostic]): Unit = {
    if (transport != null) {
      val params = PublishDiagnosticsParams(uri, diagnostics)
      val notification = JsonRpcMessage.encodeNotification("textDocument/publishDiagnostics", params.asJson)
      transport.writeMessage(notification)
    }
  }
}

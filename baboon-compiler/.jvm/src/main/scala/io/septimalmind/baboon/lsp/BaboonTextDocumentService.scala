package io.septimalmind.baboon.lsp

import io.septimalmind.baboon.lsp.features._
import io.septimalmind.baboon.lsp.state._
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.services.{LanguageClient, TextDocumentService}

import java.util.concurrent.CompletableFuture
import java.util.{List => JList}
import scala.jdk.CollectionConverters._

class BaboonTextDocumentService(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  diagnosticsProvider: DiagnosticsProvider,
  definitionProvider: DefinitionProvider,
  hoverProvider: HoverProvider,
  completionProvider: CompletionProvider,
  documentSymbolProvider: DocumentSymbolProvider
) extends TextDocumentService {

  private var client: LanguageClient = _

  def setClient(client: LanguageClient): Unit = {
    this.client = client
  }

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val uri     = params.getTextDocument.getUri
    val content = params.getTextDocument.getText
    documentState.open(uri, content)
    recompileAndPublishDiagnostics(uri)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    // For Full sync, just take the last change
    params.getContentChanges.asScala.lastOption.foreach { change =>
      documentState.update(uri, change.getText)
    }
    // Don't recompile on every change - wait for save
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    // Update content if included
    Option(params.getText).foreach(text => documentState.update(uri, text))
    recompileAndPublishDiagnostics(uri)
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    documentState.close(uri)
    // Clear diagnostics for closed document
    if (client != null) {
      client.publishDiagnostics(new PublishDiagnosticsParams(uri, java.util.Collections.emptyList()))
    }
  }

  override def definition(
    params: DefinitionParams
  ): CompletableFuture[messages.Either[JList[? <: Location], JList[? <: LocationLink]]] = {
    CompletableFuture.supplyAsync { () =>
      val uri       = params.getTextDocument.getUri
      val position  = params.getPosition
      val locations = definitionProvider.findDefinition(uri, position)
      messages.Either.forLeft(locations.asJava)
    }
  }

  override def hover(params: HoverParams): CompletableFuture[Hover] = {
    CompletableFuture.supplyAsync { () =>
      val uri      = params.getTextDocument.getUri
      val position = params.getPosition
      hoverProvider.getHover(uri, position).orNull
    }
  }

  override def completion(
    params: CompletionParams
  ): CompletableFuture[messages.Either[JList[CompletionItem], CompletionList]] = {
    CompletableFuture.supplyAsync { () =>
      val uri      = params.getTextDocument.getUri
      val position = params.getPosition
      val items    = completionProvider.getCompletions(uri, position)
      messages.Either.forLeft(items.asJava)
    }
  }

  override def documentSymbol(
    params: DocumentSymbolParams
  ): CompletableFuture[JList[messages.Either[SymbolInformation, DocumentSymbol]]] = {
    CompletableFuture.supplyAsync { () =>
      val uri     = params.getTextDocument.getUri
      val symbols = documentSymbolProvider.getSymbols(uri)
      symbols.map(s => messages.Either.forRight[SymbolInformation, DocumentSymbol](s)).asJava
    }
  }

  private def recompileAndPublishDiagnostics(@annotation.unused changedUri: String): Unit = {
    if (client == null) return

    // Recompile all workspace documents
    val compilationResult = workspaceState.recompile()

    // Publish diagnostics for all open documents
    documentState.getOpenDocuments.foreach { uri =>
      val diagnostics = diagnosticsProvider.getDiagnostics(uri, compilationResult)
      client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))
    }
  }

  def publishAllDiagnostics(): Unit = {
    if (client == null) return

    val compilationResult = workspaceState.getLastCompilationResult
    documentState.getOpenDocuments.foreach { uri =>
      val diagnostics = diagnosticsProvider.getDiagnostics(uri, compilationResult)
      client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))
    }
  }
}

package io.septimalmind.baboon.lsp

import io.septimalmind.baboon.lsp.state.WorkspaceState
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

import java.util.concurrent.CompletableFuture

class BaboonLanguageServer(
  textDocumentService: BaboonTextDocumentService,
  workspaceService: BaboonWorkspaceService,
  workspaceState: WorkspaceState
) extends LanguageServer
    with LanguageClientAware {

  private var client: LanguageClient = _

  override def initialize(params: InitializeParams): CompletableFuture[InitializeResult] = {
    CompletableFuture.supplyAsync { () =>
      // Extract workspace folders
      Option(params.getWorkspaceFolders).foreach { folders =>
        folders.forEach { folder =>
          workspaceState.addWorkspaceFolder(folder.getUri)
        }
      }
      Option(params.getRootUri).foreach(workspaceState.addWorkspaceFolder)

      val capabilities = new ServerCapabilities()

      // Text document sync - full sync for simplicity
      val syncOptions = new TextDocumentSyncOptions()
      syncOptions.setOpenClose(true)
      syncOptions.setChange(TextDocumentSyncKind.Full)
      syncOptions.setSave(new SaveOptions(true))
      capabilities.setTextDocumentSync(syncOptions)

      // Feature capabilities
      capabilities.setDefinitionProvider(true)
      capabilities.setHoverProvider(true)
      capabilities.setDocumentSymbolProvider(true)

      // Completion
      val completionOptions = new CompletionOptions()
      completionOptions.setTriggerCharacters(java.util.Arrays.asList(".", ":", "["))
      completionOptions.setResolveProvider(false)
      capabilities.setCompletionProvider(completionOptions)

      new InitializeResult(capabilities)
    }
  }

  override def initialized(params: InitializedParams): Unit = {
    // Perform initial workspace scan
    workspaceState.scanWorkspace()
    // Publish initial diagnostics for all open documents
    textDocumentService.publishAllDiagnostics()
  }

  override def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {
    System.exit(0)
  }

  override def getTextDocumentService: TextDocumentService = textDocumentService

  override def getWorkspaceService: WorkspaceService = workspaceService

  override def connect(client: LanguageClient): Unit = {
    this.client = client
    textDocumentService.setClient(client)
    workspaceService.setClient(client)
  }
}

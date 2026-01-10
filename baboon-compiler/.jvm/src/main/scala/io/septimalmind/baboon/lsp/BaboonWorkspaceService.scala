package io.septimalmind.baboon.lsp

import io.septimalmind.baboon.lsp.state.WorkspaceState
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.{LanguageClient, WorkspaceService}

class BaboonWorkspaceService(
  @annotation.unused workspaceState: WorkspaceState
) extends WorkspaceService {

  @annotation.unused
  private var client: LanguageClient = _

  def setClient(client: LanguageClient): Unit = {
    this.client = client
  }

  override def didChangeConfiguration(params: DidChangeConfigurationParams): Unit = {
    // Handle configuration changes if needed
  }

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams): Unit = {
    // No file watching - rely on document save events only
  }
}

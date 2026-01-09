package io.septimalmind.baboon.lsp.protocol

import io.circe._
import io.circe.generic.semiauto._

// ============ Initialize ============

final case class InitializeParams(
  processId: Option[Int],
  rootUri: Option[String],
  workspaceFolders: Option[Seq[WorkspaceFolder]],
  capabilities: Json
)

object InitializeParams {
  implicit val decoder: Decoder[InitializeParams] = deriveDecoder
}

final case class TextDocumentSyncOptions(
  openClose: Boolean,
  change: Int,
  save: Option[SaveOptions]
)

object TextDocumentSyncOptions {
  implicit val encoder: Encoder[TextDocumentSyncOptions] = deriveEncoder
}

final case class SaveOptions(includeText: Boolean)

object SaveOptions {
  implicit val encoder: Encoder[SaveOptions] = deriveEncoder
}

final case class CompletionOptions(
  triggerCharacters: Option[Seq[String]],
  resolveProvider: Option[Boolean]
)

object CompletionOptions {
  implicit val encoder: Encoder[CompletionOptions] = deriveEncoder
}

final case class ServerCapabilities(
  textDocumentSync: Option[TextDocumentSyncOptions] = None,
  definitionProvider: Option[Boolean] = None,
  hoverProvider: Option[Boolean] = None,
  completionProvider: Option[CompletionOptions] = None,
  documentSymbolProvider: Option[Boolean] = None
)

object ServerCapabilities {
  implicit val encoder: Encoder[ServerCapabilities] = deriveEncoder
}

final case class InitializeResult(capabilities: ServerCapabilities)

object InitializeResult {
  implicit val encoder: Encoder[InitializeResult] = deriveEncoder
}

// ============ Text Document Notifications ============

final case class DidOpenTextDocumentParams(textDocument: TextDocumentItem)

object DidOpenTextDocumentParams {
  implicit val decoder: Decoder[DidOpenTextDocumentParams] = deriveDecoder
}

final case class DidChangeTextDocumentParams(
  textDocument: VersionedTextDocumentIdentifier,
  contentChanges: Seq[TextDocumentContentChangeEvent]
)

object DidChangeTextDocumentParams {
  implicit val decoder: Decoder[DidChangeTextDocumentParams] = deriveDecoder
}

final case class DidSaveTextDocumentParams(
  textDocument: TextDocumentIdentifier,
  text: Option[String]
)

object DidSaveTextDocumentParams {
  implicit val decoder: Decoder[DidSaveTextDocumentParams] = deriveDecoder
}

final case class DidCloseTextDocumentParams(textDocument: TextDocumentIdentifier)

object DidCloseTextDocumentParams {
  implicit val decoder: Decoder[DidCloseTextDocumentParams] = deriveDecoder
}

// ============ Text Document Requests ============

final case class DefinitionParams(
  textDocument: TextDocumentIdentifier,
  position: Position
)

object DefinitionParams {
  implicit val decoder: Decoder[DefinitionParams] = deriveDecoder
}

final case class HoverParams(
  textDocument: TextDocumentIdentifier,
  position: Position
)

object HoverParams {
  implicit val decoder: Decoder[HoverParams] = deriveDecoder
}

final case class CompletionParams(
  textDocument: TextDocumentIdentifier,
  position: Position
)

object CompletionParams {
  implicit val decoder: Decoder[CompletionParams] = deriveDecoder
}

final case class DocumentSymbolParams(textDocument: TextDocumentIdentifier)

object DocumentSymbolParams {
  implicit val decoder: Decoder[DocumentSymbolParams] = deriveDecoder
}

// ============ Diagnostics ============

final case class PublishDiagnosticsParams(
  uri: String,
  diagnostics: Seq[Diagnostic]
)

object PublishDiagnosticsParams {
  implicit val encoder: Encoder[PublishDiagnosticsParams] = deriveEncoder
}

// ============ TextDocumentSyncKind ============

object TextDocumentSyncKind {
  val None: Int = 0
  val Full: Int = 1
  val Incremental: Int = 2
}

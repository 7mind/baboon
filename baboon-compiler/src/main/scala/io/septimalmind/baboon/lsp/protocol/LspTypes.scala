package io.septimalmind.baboon.lsp.protocol

import io.circe._
import io.circe.generic.semiauto._

/** LSP Position - 0-based line and character */
final case class Position(line: Int, character: Int)

object Position {
  implicit val encoder: Encoder[Position] = deriveEncoder
  implicit val decoder: Decoder[Position] = deriveDecoder
}

/** LSP Range */
final case class Range(start: Position, end: Position)

object Range {
  implicit val encoder: Encoder[Range] = deriveEncoder
  implicit val decoder: Decoder[Range] = deriveDecoder

  def zero: Range = Range(Position(0, 0), Position(0, 0))
}

/** LSP Location */
final case class Location(uri: String, range: Range)

object Location {
  implicit val encoder: Encoder[Location] = deriveEncoder
  implicit val decoder: Decoder[Location] = deriveDecoder
}

/** LSP Diagnostic Severity */
object DiagnosticSeverity {
  val Error: Int       = 1
  val Warning: Int     = 2
  val Information: Int = 3
  val Hint: Int        = 4
}

/** LSP Diagnostic */
final case class Diagnostic(
  range: Range,
  message: String,
  severity: Option[Int]  = None,
  source: Option[String] = None,
)

object Diagnostic {
  implicit val encoder: Encoder[Diagnostic] = deriveEncoder
  implicit val decoder: Decoder[Diagnostic] = deriveDecoder
}

/** LSP MarkupKind */
object MarkupKind {
  val PlainText: String = "plaintext"
  val Markdown: String  = "markdown"
}

/** LSP MarkupContent */
final case class MarkupContent(kind: String, value: String)

object MarkupContent {
  implicit val encoder: Encoder[MarkupContent] = deriveEncoder
  implicit val decoder: Decoder[MarkupContent] = deriveDecoder
}

/** LSP Hover */
final case class Hover(contents: MarkupContent, range: Option[Range] = None)

object Hover {
  implicit val encoder: Encoder[Hover] = deriveEncoder
  implicit val decoder: Decoder[Hover] = deriveDecoder
}

/** LSP CompletionItemKind */
object CompletionItemKind {
  val Text: Int          = 1
  val Method: Int        = 2
  val Function: Int      = 3
  val Constructor: Int   = 4
  val Field: Int         = 5
  val Variable: Int      = 6
  val Class: Int         = 7
  val Interface: Int     = 8
  val Module: Int        = 9
  val Property: Int      = 10
  val Unit: Int          = 11
  val Value: Int         = 12
  val Enum: Int          = 13
  val Keyword: Int       = 14
  val Snippet: Int       = 15
  val Color: Int         = 16
  val File: Int          = 17
  val Reference: Int     = 18
  val Folder: Int        = 19
  val EnumMember: Int    = 20
  val Constant: Int      = 21
  val Struct: Int        = 22
  val Event: Int         = 23
  val Operator: Int      = 24
  val TypeParameter: Int = 25
}

/** LSP CompletionItem */
final case class CompletionItem(
  label: String,
  kind: Option[Int]                    = None,
  detail: Option[String]               = None,
  documentation: Option[MarkupContent] = None,
  insertText: Option[String]           = None,
  sortText: Option[String]             = None,
  filterText: Option[String]           = None,
)

object CompletionItem {
  implicit val encoder: Encoder[CompletionItem] = deriveEncoder
  implicit val decoder: Decoder[CompletionItem] = deriveDecoder
}

/** LSP SymbolKind */
object SymbolKind {
  val File: Int          = 1
  val Module: Int        = 2
  val Namespace: Int     = 3
  val Package: Int       = 4
  val Class: Int         = 5
  val Method: Int        = 6
  val Property: Int      = 7
  val Field: Int         = 8
  val Constructor: Int   = 9
  val Enum: Int          = 10
  val Interface: Int     = 11
  val Function: Int      = 12
  val Variable: Int      = 13
  val Constant: Int      = 14
  val String: Int        = 15
  val Number: Int        = 16
  val Boolean: Int       = 17
  val Array: Int         = 18
  val Object: Int        = 19
  val Key: Int           = 20
  val Null: Int          = 21
  val EnumMember: Int    = 22
  val Struct: Int        = 23
  val Event: Int         = 24
  val Operator: Int      = 25
  val TypeParameter: Int = 26
}

/** LSP DocumentSymbol */
final case class DocumentSymbol(
  name: String,
  kind: Int,
  range: Range,
  selectionRange: Range,
  children: Option[Seq[DocumentSymbol]] = None,
)

object DocumentSymbol {
  // Custom encoder that omits null children instead of serializing as null
  implicit val encoder: Encoder[DocumentSymbol] = Encoder.instance {
    ds =>
      val base = Json.obj(
        "name"           -> Json.fromString(ds.name),
        "kind"           -> Json.fromInt(ds.kind),
        "range"          -> Encoder[Range].apply(ds.range),
        "selectionRange" -> Encoder[Range].apply(ds.selectionRange),
      )
      ds.children match {
        case Some(children) => base.deepMerge(Json.obj("children" -> Encoder[Seq[DocumentSymbol]].apply(children)))
        case None           => base
      }
  }
  implicit val decoder: Decoder[DocumentSymbol] = deriveDecoder
}

/** LSP TextDocumentIdentifier */
final case class TextDocumentIdentifier(uri: String)

object TextDocumentIdentifier {
  implicit val encoder: Encoder[TextDocumentIdentifier] = deriveEncoder
  implicit val decoder: Decoder[TextDocumentIdentifier] = deriveDecoder
}

/** LSP TextDocumentItem */
final case class TextDocumentItem(
  uri: String,
  languageId: String,
  version: Int,
  text: String,
)

object TextDocumentItem {
  implicit val encoder: Encoder[TextDocumentItem] = deriveEncoder
  implicit val decoder: Decoder[TextDocumentItem] = deriveDecoder
}

/** LSP VersionedTextDocumentIdentifier */
final case class VersionedTextDocumentIdentifier(uri: String, version: Int)

object VersionedTextDocumentIdentifier {
  implicit val encoder: Encoder[VersionedTextDocumentIdentifier] = deriveEncoder
  implicit val decoder: Decoder[VersionedTextDocumentIdentifier] = deriveDecoder
}

/** LSP TextDocumentContentChangeEvent */
final case class TextDocumentContentChangeEvent(text: String)

object TextDocumentContentChangeEvent {
  implicit val encoder: Encoder[TextDocumentContentChangeEvent] = deriveEncoder
  implicit val decoder: Decoder[TextDocumentContentChangeEvent] = deriveDecoder
}

/** LSP TextDocumentPositionParams */
final case class TextDocumentPositionParams(
  textDocument: TextDocumentIdentifier,
  position: Position,
)

object TextDocumentPositionParams {
  implicit val encoder: Encoder[TextDocumentPositionParams] = deriveEncoder
  implicit val decoder: Decoder[TextDocumentPositionParams] = deriveDecoder
}

/** LSP WorkspaceFolder */
final case class WorkspaceFolder(uri: String, name: String)

object WorkspaceFolder {
  implicit val encoder: Encoder[WorkspaceFolder] = deriveEncoder
  implicit val decoder: Decoder[WorkspaceFolder] = deriveDecoder
}

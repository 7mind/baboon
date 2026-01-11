package io.septimalmind.baboon.lsp.util

import io.septimalmind.baboon.lsp.protocol.{Position, Range}
import io.septimalmind.baboon.parser.model.{InputOffset, InputPointer, RawNodeMeta}

class PositionConverter(pathOps: PathOps) {

  /** InputOffset uses 1-based line/column, LSP uses 0-based */
  def fromInputOffset(offset: InputOffset): Position = {
    Position(offset.line - 1, offset.column - 1)
  }

  def fromInputPointer(full: InputPointer.Full): Range = {
    Range(fromInputOffset(full.start), fromInputOffset(full.stop))
  }

  def fromStartOffset(offset: InputPointer.Offset): Range = {
    val start = fromInputOffset(offset.start)
    Range(start, Position(start.line, start.character + 1))
  }

  def fromMeta(meta: RawNodeMeta): Range = {
    meta.pos match {
      case full: InputPointer.Full     => fromInputPointer(full)
      case offset: InputPointer.Offset => fromStartOffset(offset)
      case _: InputPointer.JustFile    => defaultRange
      case InputPointer.Undefined      => defaultRange
    }
  }

  def pathToUri(path: String): String = {
    pathOps.pathToUri(path)
  }

  def uriToPath(uri: String): String = {
    pathOps.uriToPath(uri)
  }

  def defaultRange: Range = Range(Position(0, 0), Position(0, 1))
}

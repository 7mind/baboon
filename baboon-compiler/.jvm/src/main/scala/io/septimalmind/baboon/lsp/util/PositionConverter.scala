package io.septimalmind.baboon.lsp.util

import io.septimalmind.baboon.parser.model.{InputOffset, InputPointer, RawNodeMeta}
import org.eclipse.lsp4j.{Position, Range}

import java.net.URI
import java.nio.file.Paths

class PositionConverter {

  /** InputOffset uses 1-based line/column, LSP uses 0-based */
  def fromInputOffset(offset: InputOffset): Position = {
    new Position(offset.line - 1, offset.column - 1)
  }

  def fromInputPointer(full: InputPointer.Full): Range = {
    new Range(fromInputOffset(full.start), fromInputOffset(full.stop))
  }

  def fromStartOffset(offset: InputPointer.Offset): Range = {
    val start = fromInputOffset(offset.start)
    // For offset-only, create a small range
    new Range(start, new Position(start.getLine, start.getCharacter + 1))
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
    Paths.get(path).toUri.toString
  }

  def uriToPath(uri: String): String = {
    new URI(uri).getPath
  }

  def defaultRange: Range = new Range(new Position(0, 0), new Position(0, 1))
}

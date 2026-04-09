package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.protocol.{Location, Position, Range}
import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.lsp.util.PositionConverter
import io.septimalmind.baboon.parser.model.InputPointer
import io.septimalmind.baboon.typer.model._

class DefinitionProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  positionConverter: PositionConverter,
) {

  def findDefinition(uri: String, position: Position): Seq[Location] = {
    val wordAtCursor = getWordAtPosition(uri, position)

    wordAtCursor.flatMap {
      typeName =>
        findTypeDefinition(typeName)
    }.toSeq
  }

  private def getWordAtPosition(uri: String, position: Position): Option[String] = {
    documentState.getContent(uri).flatMap {
      content =>
        val lines = content.split("\n", -1)
        if (position.line < lines.length) {
          val line = lines(position.line)
          val col  = position.character

          if (col <= line.length) {
            val wordChars = (c: Char) => c.isLetterOrDigit || c == '_'

            var start = col
            while (start > 0 && wordChars(line.charAt(start - 1))) start -= 1

            var end = col
            while (end < line.length && wordChars(line.charAt(end))) end += 1

            if (start < end) Some(line.substring(start, end)) else None
          } else None
        } else None
    }
  }

  private def locationFromPos(pos: InputPointer): Option[Location] = {
    pos match {
      case full: InputPointer.Full =>
        Some(Location(positionConverter.pathToUri(full.file.asString), positionConverter.fromInputPointer(full)))
      case offset: InputPointer.Offset =>
        Some(Location(positionConverter.pathToUri(offset.file.asString), positionConverter.fromStartOffset(offset)))
      case file: InputPointer.JustFile =>
        Some(Location(positionConverter.pathToUri(file.file.asString), Range.zero))
      case InputPointer.Undefined =>
        None
    }
  }

  private def findTypeDefinition(typeName: String): Option[Location] = {
    workspaceState.getFamily.flatMap {
      family =>
        val fromMembers = family.domains.toMap.values.flatMap {
          lineage =>
            lineage.versions.toMap.values.flatMap {
              domain =>
                domain.defs.meta.nodes.values.collectFirst {
                  case u: DomainMember.User if u.id.name.name == typeName => u.meta.pos
                }.flatMap(locationFromPos)
            }
        }.headOption

        fromMembers.orElse {
          family.domains.toMap.values.flatMap {
            lineage =>
              lineage.versions.toMap.values.flatMap {
                domain =>
                  domain.aliases.collectFirst {
                    case a if a.name.name == typeName => a.meta.pos
                  }.flatMap(locationFromPos)
              }
          }.headOption
        }
    }
  }
}

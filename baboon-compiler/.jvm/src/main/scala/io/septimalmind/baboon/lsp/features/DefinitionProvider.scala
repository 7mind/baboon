package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.lsp.util.PositionConverter
import io.septimalmind.baboon.parser.model.InputPointer
import io.septimalmind.baboon.typer.model._
import org.eclipse.lsp4j._

class DefinitionProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  positionConverter: PositionConverter
) {

  def findDefinition(uri: String, position: Position): Seq[Location] = {
    val wordAtCursor = getWordAtPosition(uri, position)

    wordAtCursor.flatMap { typeName =>
      findTypeDefinition(typeName)
    }.toSeq
  }

  private def getWordAtPosition(uri: String, position: Position): Option[String] = {
    documentState.getContent(uri).flatMap { content =>
      val lines = content.split("\n", -1)
      if (position.getLine < lines.length) {
        val line = lines(position.getLine)
        val col  = position.getCharacter

        if (col <= line.length) {
          // Extract word at cursor (type names are alphanumeric + underscore)
          val wordChars = (c: Char) => c.isLetterOrDigit || c == '_'

          // Find word boundaries
          var start = col
          while (start > 0 && wordChars(line.charAt(start - 1))) start -= 1

          var end = col
          while (end < line.length && wordChars(line.charAt(end))) end += 1

          if (start < end) Some(line.substring(start, end)) else None
        } else None
      } else None
    }
  }

  private def findTypeDefinition(typeName: String): Option[Location] = {
    workspaceState.getFamily.flatMap { family =>
      // Search all domains for a type with this name
      family.domains.toMap.values.flatMap { lineage =>
        lineage.versions.toMap.values.flatMap { domain =>
          domain.defs.meta.nodes.values.collectFirst {
            case u: DomainMember.User if u.id.name.name == typeName =>
              u.meta.pos match {
                case full: InputPointer.Full =>
                  val range = positionConverter.fromInputPointer(full)
                  new Location(positionConverter.pathToUri(full.file.asString), range)
                case offset: InputPointer.Offset =>
                  val range = positionConverter.fromStartOffset(offset)
                  new Location(positionConverter.pathToUri(offset.file.asString), range)
                case file: InputPointer.JustFile =>
                  new Location(
                    positionConverter.pathToUri(file.file.asString),
                    new Range(new Position(0, 0), new Position(0, 0))
                  )
                case InputPointer.Undefined =>
                  null
              }
          }.filter(_ != null)
        }
      }.headOption
    }
  }
}

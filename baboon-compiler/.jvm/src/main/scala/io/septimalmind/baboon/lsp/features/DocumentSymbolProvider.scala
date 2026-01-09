package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.state.WorkspaceState
import io.septimalmind.baboon.lsp.util.PositionConverter
import io.septimalmind.baboon.parser.model.InputPointer
import io.septimalmind.baboon.typer.model._
import org.eclipse.lsp4j._

import scala.jdk.CollectionConverters._

class DocumentSymbolProvider(
  workspaceState: WorkspaceState,
  positionConverter: PositionConverter
) {

  def getSymbols(uri: String): Seq[DocumentSymbol] = {
    val filePath = positionConverter.uriToPath(uri)

    workspaceState.getFamily
      .map { family =>
        family.domains.toMap.values.flatMap { lineage =>
          lineage.versions.toMap.values.flatMap { domain =>
            domain.defs.meta.nodes.values.collect {
              case u: DomainMember.User if isInFile(u.meta.pos, filePath) =>
                createSymbol(u, domain)
            }
          }
        }.toSeq
      }
      .getOrElse(Seq.empty)
  }

  private def isInFile(pos: InputPointer, filePath: String): Boolean = {
    pos match {
      case fk: InputPointer.FileKnown => fk.file.asString == filePath
      case InputPointer.Undefined     => false
    }
  }

  private def createSymbol(member: DomainMember.User, domain: Domain): DocumentSymbol = {
    val name = member.id.name.name
    val kind = member.defn match {
      case _: Typedef.Dto      => SymbolKind.Class
      case _: Typedef.Adt      => SymbolKind.Interface
      case _: Typedef.Enum     => SymbolKind.Enum
      case _: Typedef.Foreign  => SymbolKind.TypeParameter
      case _: Typedef.Contract => SymbolKind.Interface
      case _: Typedef.Service  => SymbolKind.Module
    }

    val range = member.meta.pos match {
      case full: InputPointer.Full     => positionConverter.fromInputPointer(full)
      case offset: InputPointer.Offset => positionConverter.fromStartOffset(offset)
      case _                           => new Range(new Position(0, 0), new Position(0, 0))
    }

    val symbol = new DocumentSymbol(name, kind, range, range)

    // Add children for ADTs and Enums
    val children: Seq[DocumentSymbol] = member.defn match {
      case adt: Typedef.Adt =>
        adt.members.toList.flatMap { branchId =>
          domain.defs.meta.nodes.get(branchId).collect { case u: DomainMember.User =>
            val childRange = u.meta.pos match {
              case full: InputPointer.Full     => positionConverter.fromInputPointer(full)
              case offset: InputPointer.Offset => positionConverter.fromStartOffset(offset)
              case _                           => new Range(new Position(0, 0), new Position(0, 0))
            }
            new DocumentSymbol(branchId.name.name, SymbolKind.Class, childRange, childRange)
          }
        }

      case enum: Typedef.Enum =>
        enum.members.toList.map { m =>
          // Enum members don't have individual positions, use parent range
          new DocumentSymbol(m.name, SymbolKind.EnumMember, range, range)
        }

      case dto: Typedef.Dto =>
        dto.fields.map { f =>
          // Fields don't have individual positions either
          new DocumentSymbol(f.name.name, SymbolKind.Field, range, range)
        }

      case _ => Seq.empty
    }

    if (children.nonEmpty) {
      symbol.setChildren(children.asJava)
    }

    symbol
  }
}

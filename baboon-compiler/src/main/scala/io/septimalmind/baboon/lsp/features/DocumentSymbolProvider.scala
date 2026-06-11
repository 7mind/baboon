package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.LspLogging
import io.septimalmind.baboon.lsp.protocol.{DocumentSymbol, Range, SymbolKind}
import io.septimalmind.baboon.lsp.state.WorkspaceState
import io.septimalmind.baboon.lsp.util.{PathOps, PositionConverter}
import io.septimalmind.baboon.parser.model.{InputPointer, RawNodeMeta}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.BLogger

import scala.util.Try

class DocumentSymbolProvider(
  workspaceState: WorkspaceState,
  positionConverter: PositionConverter,
  pathOps: PathOps,
  logger: BLogger,
) {

  def getSymbols(uri: String): Seq[DocumentSymbol] = {
    val filePathOpt = pathOps.uriToPathSafe(uri)
    if (filePathOpt.isEmpty) {
      logger.message(LspLogging.Context, s"getSymbols: unconvertible URI (malformed or non-file scheme), returning empty: uri=$uri")
      return Seq.empty
    }
    val filePath = filePathOpt.get
    logger.message(LspLogging.Context, s"getSymbols: uri=$uri, filePath=$filePath")

    workspaceState.getFamily match {
      case None =>
        logger.message(LspLogging.Context, "getSymbols: NO FAMILY - compilation failed or not run")
        Seq.empty
      case Some(family) =>
        // Carry the owning domain alongside each member so createSymbol can look up
        // ADT branch nodes in the correct domain (D15/T35: the old code passed .head
        // for every member, routing branch lookups to the wrong domain's node map).
        val allTypes = family.domains.toMap.values.flatMap {
          lineage =>
            lineage.versions.toMap.values.flatMap {
              domain =>
                domain.defs.meta.nodes.values.collect { case u: DomainMember.User => (domain, u) }
            }
        }.toSeq
        logger.message(LspLogging.Context, s"getSymbols: found ${allTypes.size} total types in family")

        val inFile = allTypes.filter { case (_, u) => isInFile(u.meta.pos, filePath) }
        logger.message(LspLogging.Context, s"getSymbols: ${inFile.size} types in file $filePath")

        if (inFile.isEmpty && allTypes.nonEmpty) {
          // Debug: show first few type paths
          allTypes.take(3).foreach {
            case (_, u) =>
              u.meta.pos match {
                case fk: InputPointer.FileKnown =>
                  logger.message(LspLogging.Context, s"getSymbols: type ${u.id.name.name} is in ${fk.file.asString}")
                case _ =>
              }
          }
        }

        val aliasSymbols = family.domains.toMap.values.flatMap {
          lineage =>
            lineage.versions.toMap.values.flatMap {
              domain =>
                domain.aliases.filter(a => isInFile(a.meta.pos, filePath)).map {
                  a =>
                    val range = a.meta.pos match {
                      case full: InputPointer.Full     => positionConverter.fromInputPointer(full)
                      case offset: InputPointer.Offset => positionConverter.fromStartOffset(offset)
                      case _                           => Range.zero
                    }
                    DocumentSymbol(a.name.name, SymbolKind.TypeParameter, range, range)
                }
            }
        }.toSeq

        val templateSymbols = family.domains.toMap.values.flatMap {
          lineage =>
            lineage.versions.toMap.values.flatMap {
              domain =>
                domain.templateRegistry.templates.collect {
                  case ((_, _, name), body) if isInFile(templateBodyMeta(body).pos, filePath) =>
                    val meta  = templateBodyMeta(body)
                    val range = meta.pos match {
                      case full: InputPointer.Full     => positionConverter.fromInputPointer(full)
                      case offset: InputPointer.Offset => positionConverter.fromStartOffset(offset)
                      case _                           => Range.zero
                    }
                    DocumentSymbol(name.name, SymbolKind.Class, range, range)
                }
            }
        }.toSeq

        val typeSymbols = inFile.map { case (domain, u) => createSymbol(u, domain) }
        (typeSymbols ++ aliasSymbols ++ templateSymbols).sortBy(_.name)
    }
  }

  private def templateBodyMeta(body: TemplateBody): RawNodeMeta = body.rawDefn match {
    case RawTemplateDefn.Dto(raw)        => raw.meta
    case RawTemplateDefn.Identifier(raw) => raw.meta
    case RawTemplateDefn.Adt(raw)        => raw.meta
    case RawTemplateDefn.Contract(raw)   => raw.meta
    case RawTemplateDefn.Service(raw)    => raw.meta
  }

  private def isInFile(pos: InputPointer, filePath: String): Boolean = {
    pos match {
      case fk: InputPointer.FileKnown =>
        val posPath = Try(pathOps.normalizePath(fk.file.asString)).getOrElse(fk.file.asString)
        val reqPath = Try(pathOps.normalizePath(filePath)).getOrElse(filePath)
        posPath == reqPath
      case InputPointer.Undefined => false
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
      case _                           => Range.zero
    }

    val children: Seq[DocumentSymbol] = member.defn match {
      case adt: Typedef.Adt =>
        adt.members.toList.flatMap {
          branchId =>
            domain.defs.meta.nodes.get(branchId).collect {
              case u: DomainMember.User =>
                val childRange = u.meta.pos match {
                  case full: InputPointer.Full     => positionConverter.fromInputPointer(full)
                  case offset: InputPointer.Offset => positionConverter.fromStartOffset(offset)
                  case _                           => Range.zero
                }
                DocumentSymbol(branchId.name.name, SymbolKind.Class, childRange, childRange)
            }
        }

      case enum: Typedef.Enum =>
        enum.members.toList.map {
          m =>
            DocumentSymbol(m.name, SymbolKind.EnumMember, range, range)
        }

      case dto: Typedef.Dto =>
        dto.fields.map {
          f =>
            DocumentSymbol(f.name.name, SymbolKind.Field, range, range)
        }

      case _ => Seq.empty
    }

    val symbol = DocumentSymbol(name, kind, range, range, if (children.nonEmpty) Some(children) else None)
    logger.message(LspLogging.Context, s"createSymbol: $name, range=$range, children=${children.size}")
    symbol
  }
}

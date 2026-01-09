package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.{FSPath, InputPointer}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.platform.files.IzFiles

import java.nio.file.{Files, Path, Paths}
import scala.collection.concurrent.TrieMap

class WorkspaceState(
  documentState: DocumentState,
  loader: BaboonLoader[Lambda[(`+e`, `+a`) => Either[e, a]]],
  cliModelDirs: Set[Path]
) {
  private val workspaceFolders = TrieMap.empty[String, Path]
  @volatile private var lastCompilationResult: CompilationResult = CompilationResult.empty

  // Initialize with CLI model directories - these are the ONLY source of .baboon files
  cliModelDirs.foreach { path =>
    val normalizedKey = path.toAbsolutePath.normalize().toString
    workspaceFolders.put(normalizedKey, path)
  }

  /** VSCode workspace folders are ignored - model dirs must be specified via CLI */
  def addWorkspaceFolder(uri: String): Unit = {
    // Intentionally ignored - we only use CLI --model-dir
    ()
  }

  def scanWorkspace(): Unit = {
    recompile()
    ()
  }

  def recompile(): CompilationResult = {
    System.err.println(s"[LSP] recompile: workspaceFolders=${workspaceFolders.keys.mkString(", ")}")

    // Collect all .baboon files from workspace
    val workspaceFiles = workspaceFolders.values.flatMap { folder =>
      if (Files.exists(folder)) {
        IzFiles.walk(folder.toFile).filter(_.toFile.getName.endsWith(".baboon"))
      } else {
        Seq.empty
      }
    }.toSeq

    System.err.println(s"[LSP] recompile: found ${workspaceFiles.size} .baboon files")

    // Build file paths for compilation
    val inputPaths = workspaceFiles.map(_.toAbsolutePath).toList

    // Compile using the loader
    val result = if (inputPaths.nonEmpty) {
      loader.load(inputPaths) match {
        case Right(family) =>
          System.err.println(s"[LSP] recompile: SUCCESS - ${family.domains.size} domains")
          CompilationResult(Some(family), Seq.empty, Map.empty)
        case Left(issues) =>
          System.err.println(s"[LSP] recompile: FAILED - ${issues.size} issues")
          issues.toList.take(5).foreach { issue =>
            val formatted = formatIssue(issue)
            System.err.println(s"[LSP] recompile: $formatted")
          }
          val fileIssues = groupIssuesByFile(issues.toList)
          CompilationResult(None, issues.toList, fileIssues)
      }
    } else {
      System.err.println(s"[LSP] recompile: NO FILES TO COMPILE")
      CompilationResult.empty
    }

    lastCompilationResult = result
    result
  }

  def getLastCompilationResult: CompilationResult = lastCompilationResult

  def getFamily: Option[BaboonFamily] = lastCompilationResult.family

  private def groupIssuesByFile(issues: Seq[BaboonIssue]): Map[String, Seq[BaboonIssue]] = {
    issues.groupBy(extractFileUri).collect {
      case (Some(uri), issueList) => uri -> issueList
    }
  }

  private def extractFileUri(issue: BaboonIssue): Option[String] = {
    extractInputPointer(issue).flatMap {
      case fk: InputPointer.FileKnown =>
        Some(Paths.get(fk.file.asString).toUri.toString)
      case InputPointer.Undefined =>
        None
    }
  }

  private def extractInputPointer(issue: BaboonIssue): Option[InputPointer] = {
    issue match {
      case BaboonIssue.Parser(pi) =>
        pi match {
          case io.septimalmind.baboon.parser.model.issues.ParserIssue.ParserFailed(_, path) =>
            Some(InputPointer.JustFile(path))
          case io.septimalmind.baboon.parser.model.issues.ParserIssue.IncludeNotFound(path) =>
            Some(InputPointer.JustFile(FSPath.parse(izumi.fundamentals.collections.nonempty.NEString.unsafeFrom(path))))
        }
      case BaboonIssue.Typer(ti) =>
        extractTyperIssuePointer(ti)
      case BaboonIssue.Verification(vi) =>
        extractVerificationIssuePointer(vi)
      case BaboonIssue.Evolution(_) =>
        None
      case BaboonIssue.IO(_) =>
        None
      case BaboonIssue.Translation(_) =>
        None
      case BaboonIssue.RuntimeCodec(_) =>
        None
    }
  }

  private def extractTyperIssuePointer(issue: io.septimalmind.baboon.parser.model.issues.TyperIssue): Option[InputPointer] = {
    import io.septimalmind.baboon.parser.model.issues.TyperIssue._
    issue match {
      case GenericTyperIssue(_, meta)                   => Some(meta.pos)
      case NameNotFound(_, _, meta)                     => Some(meta.pos)
      case MissingTypeId(_, _, meta)                    => Some(meta.pos)
      case NonUniqueFields(_, _, meta)                  => Some(meta.pos)
      case EmptyEnum(_, meta)                           => Some(meta.pos)
      case EmptyAdt(_, meta)                            => Some(meta.pos)
      case BadTypeName(_, meta)                         => Some(meta.pos)
      case BadFieldName(_, meta)                        => Some(meta.pos)
      case BadEnumName(_, meta)                         => Some(meta.pos)
      case CircularInheritance(_, meta)                 => Some(meta.pos)
      case NonUniqueEnumBranches(_, _, meta)            => Some(meta.pos)
      case NonUniqueForeignEntries(_, _, meta)          => Some(meta.pos)
      case EmptyGenericArgs(_, meta)                    => Some(meta.pos)
      case NonUniqueTypedefs(_, meta)                   => Some(meta.pos)
      case NonUniqueScope(_, meta)                      => Some(meta.pos)
      case UnexpectedScoping(_, meta)                   => Some(meta.pos)
      case UnexpectedBuiltin(_, _, meta)                => Some(meta.pos)
      case UnexpectedNonBuiltin(_, _, _, meta)          => Some(meta.pos)
      case ScopedRefToNamespacedGeneric(_, meta)        => Some(meta.pos)
      case UnexpectedScopeLookup(_, meta)               => Some(meta.pos)
      case NamSeqeNotFound(_, _, meta)                  => Some(meta.pos)
      case DuplicatedTypes(_, meta)                     => Some(meta.pos)
      case WrongParent(_, _, meta)                      => Some(meta.pos)
      case MissingContractFields(_, _, meta)            => Some(meta.pos)
      case BadInheritance(_, meta)                      => Some(meta.pos)
      case NonUniqueMethodNames(_, _, meta)             => Some(meta.pos)
      case ServiceMissingOutput(_, _, meta)             => Some(meta.pos)
      case ServiceMultipleOutputs(_, _, _, meta)        => Some(meta.pos)
      case ServiceMultipleErrors(_, _, _, meta)         => Some(meta.pos)
      case DagError(_, meta)                            => Some(meta.pos)
      case ScalarExpected(_, meta)                      => Some(meta.pos)
      case CollectionExpected(_, meta)                  => Some(meta.pos)
      case ScopeCannotBeEmpty(member)                   => Some(member.meta.pos)
      case EmptyPackageId(header)                       => Some(header.meta.pos)
      case DuplicatedTypedefs(model, _)                 => Some(model.header.meta.pos)
      case _: NonUniqueDomainVersions                   => None
      case _: EmptyDomainFamily                         => None
      case _: NonUniqueLineages                         => None
      case _: NonUniqueRawDomainVersion                 => None
      case _: EmptyFamily                               => None
      case _: TodoTyperIssue                            => None
    }
  }

  private def extractVerificationIssuePointer(issue: io.septimalmind.baboon.parser.model.issues.VerificationIssue): Option[InputPointer] = {
    import io.septimalmind.baboon.parser.model.issues.VerificationIssue._
    issue match {
      case ConflictingDtoFields(_, _, meta)                   => Some(meta.pos)
      case ConflictingEnumBranches(_, _, meta)                => Some(meta.pos)
      case ConflictingAdtBranches(_, _, meta)                 => Some(meta.pos)
      case BadFieldNames(_, _, meta)                          => Some(meta.pos)
      case EmptyEnumDef(_, meta)                              => Some(meta.pos)
      case EitherAllOrNoneEnumMembersMustHaveConstants(_, meta) => Some(meta.pos)
      case WrongEnumConstant(_, meta)                         => Some(meta.pos)
      case EmptyAdtDef(_, meta)                               => Some(meta.pos)
      case UnderscoredDefinitionRetained(_, meta)             => Some(meta.pos)
      case PathologicGenerics(_, _, meta)                     => Some(meta.pos)
      case SetsCantContainGenerics(_, _, meta)                => Some(meta.pos)
      case MapKeysShouldNotBeGeneric(_, _, meta)              => Some(meta.pos)
      case _: LockedVersionModified                           => None
      case _: MissingTypeDef                                  => None
      case _: ReferentialCyclesFound                          => None
      case _: IncorrectRootFound                              => None
      case _: ConflictingTypeIds                              => None
      case _: MissingEvoDiff                                  => None
      case _: MissingEvoConversion                            => None
      case _: BrokenConversion                                => None
      case _: IncorrectConversionApplication                  => None
    }
  }

  private def formatIssue(issue: BaboonIssue): String = {
    import io.septimalmind.baboon.parser.model.issues.TyperIssue._
    import io.septimalmind.baboon.parser.model.issues.VerificationIssue._
    import io.septimalmind.baboon.parser.model.issues.ParserIssue._

    val (location, message) = issue match {
      case BaboonIssue.Parser(ParserFailed(error, path)) =>
        (Some(path.asString), s"Parse error: ${error.msg}")
      case BaboonIssue.Parser(IncludeNotFound(path)) =>
        (None, s"Include not found: $path")

      case BaboonIssue.Typer(ti) => ti match {
        case GenericTyperIssue(msg, meta)               => (extractLocation(meta.pos), msg)
        case NameNotFound(_, name, meta)                => (extractLocation(meta.pos), s"Type not found: ${name.path.head.name}")
        case MissingTypeId(_, missing, meta)            => (extractLocation(meta.pos), s"Missing types: ${missing.map(_.name.name).mkString(", ")}")
        case NonUniqueFields(id, dupes, meta)           => (extractLocation(meta.pos), s"Duplicate fields in ${id.name.name}: ${dupes.keys.mkString(", ")}")
        case EmptyEnum(id, meta)                        => (extractLocation(meta.pos), s"Empty enum: ${id.name.name}")
        case EmptyAdt(id, meta)                         => (extractLocation(meta.pos), s"Empty ADT: ${id.name.name}")
        case BadTypeName(name, meta)                    => (extractLocation(meta.pos), s"Invalid type name: $name")
        case BadFieldName(name, meta)                   => (extractLocation(meta.pos), s"Invalid field name: $name")
        case BadEnumName(name, meta)                    => (extractLocation(meta.pos), s"Invalid enum member name: $name")
        case CircularInheritance(error, meta)           => (extractLocation(meta.pos), s"Circular inheritance: $error")
        case UnexpectedBuiltin(id, _, meta)             => (extractLocation(meta.pos), s"Unexpected builtin: ${id.name.name}")
        case UnexpectedNonBuiltin(name, _, _, meta)     => (extractLocation(meta.pos), s"Type not found: ${name.name}")
        case NonUniqueDomainVersions(dupes)             => (None, s"Duplicate domain versions: ${dupes.keys.mkString(", ")}")
        case NonUniqueRawDomainVersion(conflicts)       => (None, s"Duplicate domain versions: ${conflicts.keys.map(k => s"${k.id}@${k.version}").mkString(", ")}")
        case _                                          => (None, ti.toString.take(100))
      }

      case BaboonIssue.Verification(vi) => vi match {
        case ConflictingDtoFields(dto, _, meta)         => (extractLocation(meta.pos), s"Conflicting fields in ${dto.name.name}")
        case EmptyEnumDef(e, meta)                      => (extractLocation(meta.pos), s"Empty enum: ${e.id.name.name}")
        case EmptyAdtDef(a, meta)                       => (extractLocation(meta.pos), s"Empty ADT: ${a.id.name.name}")
        case _                                          => (None, vi.toString.take(100))
      }

      case BaboonIssue.Evolution(ei)     => (None, s"Evolution: ${ei.toString.take(80)}")
      case BaboonIssue.IO(ioi)           => (None, s"IO: ${ioi.toString.take(80)}")
      case BaboonIssue.Translation(ti)   => (None, s"Translation: ${ti.toString.take(80)}")
      case BaboonIssue.RuntimeCodec(ri)  => (None, s"Runtime codec: ${ri.toString.take(80)}")
    }

    location match {
      case Some(loc) => s"$loc: $message"
      case None      => message
    }
  }

  private def extractLocation(pos: InputPointer): Option[String] = pos match {
    case full: InputPointer.Full     => Some(s"${full.file.name.theString}:${full.start.line}:${full.start.column}")
    case offset: InputPointer.Offset => Some(s"${offset.file.name.theString}:${offset.start.line}:${offset.start.column}")
    case file: InputPointer.JustFile => Some(file.file.name.theString)
    case InputPointer.Undefined      => None
  }
}

case class CompilationResult(
  family: Option[BaboonFamily],
  issues: Seq[BaboonIssue],
  fileIssues: Map[String, Seq[BaboonIssue]]
)

object CompilationResult {
  def empty: CompilationResult = CompilationResult(None, Seq.empty, Map.empty)
}

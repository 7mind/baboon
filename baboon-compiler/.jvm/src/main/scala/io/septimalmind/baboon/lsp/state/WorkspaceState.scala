package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.{FSPath, InputPointer}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.files.IzFiles

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import scala.collection.concurrent.TrieMap

class WorkspaceState(
  documentState: DocumentState,
  loader: BaboonLoader[Lambda[(`+e`, `+a`) => Either[e, a]]]
) {
  private val workspaceFolders = TrieMap.empty[String, Path]
  @volatile private var lastCompilationResult: CompilationResult = CompilationResult.empty

  def addWorkspaceFolder(uri: String): Unit = {
    val path = Paths.get(new URI(uri))
    workspaceFolders.put(uri, path)
  }

  def scanWorkspace(): Unit = {
    recompile()
    ()
  }

  def recompile(): CompilationResult = {
    // Collect all .baboon files from workspace
    val workspaceFiles = workspaceFolders.values.flatMap { folder =>
      if (Files.exists(folder)) {
        IzFiles.walk(folder.toFile).filter(_.toFile.getName.endsWith(".baboon"))
      } else {
        Seq.empty
      }
    }.toSeq

    // Build file paths for compilation
    val inputPaths = workspaceFiles.map(_.toAbsolutePath).toList

    // Compile using the loader
    val result = if (inputPaths.nonEmpty) {
      loader.load(inputPaths) match {
        case Right(family) =>
          CompilationResult(Some(family), Seq.empty, Map.empty)
        case Left(issues) =>
          val fileIssues = groupIssuesByFile(issues.toList)
          CompilationResult(None, issues.toList, fileIssues)
      }
    } else {
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
}

case class CompilationResult(
  family: Option[BaboonFamily],
  issues: Seq[BaboonIssue],
  fileIssues: Map[String, Seq[BaboonIssue]]
)

object CompilationResult {
  def empty: CompilationResult = CompilationResult(None, Seq.empty, Map.empty)
}

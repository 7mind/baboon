package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.protocol.{Diagnostic, DiagnosticSeverity, Position, Range}
import io.septimalmind.baboon.lsp.state.CompilationResult
import io.septimalmind.baboon.lsp.util.PositionConverter
import io.septimalmind.baboon.parser.model.{InputOffset, RawNodeMeta}
import io.septimalmind.baboon.parser.model.issues._

class DiagnosticsProvider(positionConverter: PositionConverter) {

  def getDiagnostics(uri: String, result: CompilationResult): Seq[Diagnostic] = {
    result.fileIssues.getOrElse(uri, Seq.empty).flatMap { issue =>
      convertToDiagnostic(issue)
    }
  }

  private def convertToDiagnostic(issue: BaboonIssue): Option[Diagnostic] = {
    val (range, message, severity) = issue match {
      case BaboonIssue.Parser(pi) =>
        pi match {
          case ParserIssue.ParserFailed(error, _) =>
            (parseErrorRange(error), s"Parse error: ${error.msg}", DiagnosticSeverity.Error)
          case ParserIssue.IncludeNotFound(path) =>
            (positionConverter.defaultRange, s"Include not found: $path", DiagnosticSeverity.Error)
        }

      case BaboonIssue.Typer(ti) =>
        val (meta, msg) = extractTyperIssueInfo(ti)
        val range       = meta.map(m => positionConverter.fromMeta(m)).getOrElse(positionConverter.defaultRange)
        (range, msg, DiagnosticSeverity.Error)

      case BaboonIssue.Verification(vi) =>
        val (meta, msg) = extractVerificationIssueInfo(vi)
        val range       = meta.map(m => positionConverter.fromMeta(m)).getOrElse(positionConverter.defaultRange)
        (range, msg, DiagnosticSeverity.Error)

      case BaboonIssue.Evolution(ei) =>
        (positionConverter.defaultRange, s"Evolution issue: $ei", DiagnosticSeverity.Warning)

      case BaboonIssue.IO(ioi) =>
        (positionConverter.defaultRange, s"IO issue: $ioi", DiagnosticSeverity.Error)

      case BaboonIssue.Translation(ti) =>
        (positionConverter.defaultRange, s"Translation issue: $ti", DiagnosticSeverity.Error)

      case BaboonIssue.RuntimeCodec(ri) =>
        (positionConverter.defaultRange, s"Runtime codec issue: $ri", DiagnosticSeverity.Error)
    }

    Some(Diagnostic(range, message, Some(severity), Some("baboon")))
  }

  private def parseErrorRange(error: fastparse.Parsed.Failure): Range = {
    val pretty = error.extra.input.prettyIndex(error.index)
    val parts  = pretty.split(":")
    require(parts.length >= 2, s"Unexpected parser error position format: $pretty")

    val line   = parts(0).toInt
    val column = parts(1).toInt
    val offset = InputOffset(error.index, line, column)
    val start  = positionConverter.fromInputOffset(offset)
    Range(start, Position(start.line, start.character + 1))
  }

  private def extractTyperIssueInfo(issue: TyperIssue): (Option[RawNodeMeta], String) = {
    import TyperIssue._
    issue match {
      case GenericTyperIssue(message, meta)               => (Some(meta), message)
      case NameNotFound(_, name, meta)                    => (Some(meta), s"Type not found: ${name.path.head.name}")
      case MissingTypeId(_, missing, meta)                => (Some(meta), s"Missing types: ${missing.map(_.name.name).mkString(", ")}")
      case NonUniqueFields(id, dupes, meta)               => (Some(meta), s"Duplicate fields in ${id.name.name}: ${dupes.keys.mkString(", ")}")
      case EmptyEnum(id, meta)                            => (Some(meta), s"Empty enum: ${id.name.name}")
      case EmptyAdt(id, meta)                             => (Some(meta), s"Empty ADT: ${id.name.name}")
      case BadTypeName(name, meta)                        => (Some(meta), s"Invalid type name: $name")
      case BadFieldName(name, meta)                       => (Some(meta), s"Invalid field name: $name")
      case BadEnumName(name, meta)                        => (Some(meta), s"Invalid enum member name: $name")
      case CircularInheritance(error, meta)               => (Some(meta), s"Circular inheritance: $error")
      case NonUniqueEnumBranches(dupes, id, meta)         => (Some(meta), s"Duplicate enum branches in ${id.name.name}: ${dupes.keys.mkString(", ")}")
      case NonUniqueForeignEntries(dupes, id, meta)       => (Some(meta), s"Duplicate foreign entries in ${id.name.name}: ${dupes.keys.mkString(", ")}")
      case EmptyGenericArgs(id, meta)                     => (Some(meta), s"Empty generic args for ${id.name.name}")
      case NonUniqueTypedefs(dupes, meta)                 => (Some(meta), s"Duplicate typedefs: ${dupes.keys.map(_.name.name).mkString(", ")}")
      case NonUniqueScope(dupes, meta)                    => (Some(meta), s"Duplicate scopes: ${dupes.keys.mkString(", ")}")
      case UnexpectedScoping(scopes, meta)                => (Some(meta), s"Unexpected scoping: ${scopes.size} scopes")
      case UnexpectedBuiltin(id, _, meta)                 => (Some(meta), s"Unexpected builtin: ${id.name.name}")
      case UnexpectedNonBuiltin(name, _, _, meta)         => (Some(meta), s"Unexpected non-builtin: ${name.name}")
      case ScopedRefToNamespacedGeneric(prefix, meta)     => (Some(meta), s"Scoped ref to namespaced generic: ${prefix.map(_.name).mkString(".")}")
      case UnexpectedScopeLookup(_, meta)                 => (Some(meta), "Unexpected scope lookup")
      case NamSeqeNotFound(names, _, meta)                => (Some(meta), s"Names not found: ${names.map(_.name).mkString(".")}")
      case DuplicatedTypes(dupes, meta)                   => (Some(meta), s"Duplicate types: ${dupes.map(_.name.name).mkString(", ")}")
      case WrongParent(id, parent, meta)                  => (Some(meta), s"Wrong parent for ${id.name.name}: ${parent.name.name}")
      case MissingContractFields(id, fields, meta)        => (Some(meta), s"Missing contract fields in ${id.name.name}: ${fields.map(_.name.name).mkString(", ")}")
      case BadInheritance(bad, meta)                      => (Some(meta), s"Bad inheritance: ${bad.keys.map(_.name.name).mkString(", ")}")
      case NonUniqueMethodNames(svc, dupes, meta)         => (Some(meta), s"Duplicate methods in $svc: ${dupes.keys.mkString(", ")}")
      case ServiceMissingOutput(svc, method, meta)        => (Some(meta), s"Missing output in $svc.$method")
      case ServiceMultipleOutputs(svc, method, _, meta)   => (Some(meta), s"Multiple outputs in $svc.$method")
      case ServiceMultipleErrors(svc, method, _, meta)    => (Some(meta), s"Multiple errors in $svc.$method")
      case DagError(e, meta)                              => (Some(meta), s"DAG error: $e")
      case ScalarExpected(id, meta)                       => (Some(meta), s"Scalar expected: ${id.name.name}")
      case CollectionExpected(id, meta)                   => (Some(meta), s"Collection expected: ${id.name.name}")
      case ScopeCannotBeEmpty(member)                     => (Some(member.meta), s"Scope cannot be empty: ${member.name.name}")
      case EmptyPackageId(header)                         => (Some(header.meta), "Empty package ID")
      case DuplicatedTypedefs(model, dupes)               => (Some(model.header.meta), s"Duplicate typedefs: ${dupes.keys.map(_.name.name).mkString(", ")}")
      case NonUniqueDomainVersions(dupes)                 => (None, s"Non-unique domain versions: ${dupes.keys.mkString(", ")}")
      case EmptyDomainFamily(pkg)                         => (None, s"Empty domain family: $pkg")
      case NonUniqueLineages(lineages)                    => (None, s"Non-unique lineages: ${lineages.keys.mkString(", ")}")
      case NonUniqueRawDomainVersion(conflicts)           => (None, s"Non-unique raw domain versions: ${conflicts.keys.mkString(", ")}")
      case EmptyFamily(_)                                 => (None, "Empty family")
      case EmptyFamilyReload(_)                           => (None, "Empty family")
      case TodoTyperIssue(descr)                          => (None, s"TODO: $descr")
    }
  }

  private def extractVerificationIssueInfo(issue: VerificationIssue): (Option[RawNodeMeta], String) = {
    import VerificationIssue._
    issue match {
      case ConflictingDtoFields(dto, dupes, meta) =>
        (Some(meta), s"Conflicting fields in ${dto.name.name}: ${dupes.map { case (k, vs) => s"$k: ${vs.mkString(", ")}" }.mkString("; ")}")
      case ConflictingEnumBranches(uEnum, dupes, meta) =>
        (Some(meta), s"Conflicting enum branches in ${uEnum.id.name.name}: ${dupes.keys.mkString(", ")}")
      case ConflictingAdtBranches(adt, dupes, meta) =>
        (Some(meta), s"Conflicting ADT branches in ${adt.id.name.name}: ${dupes.keys.mkString(", ")}")
      case BadFieldNames(e, bad, meta) =>
        (Some(meta), s"Bad field names in ${e.name.name}: ${bad.mkString(", ")}")
      case EmptyEnumDef(e, meta) =>
        (Some(meta), s"Empty enum definition: ${e.id.name.name}")
      case EitherAllOrNoneEnumMembersMustHaveConstants(e, meta) =>
        (Some(meta), s"Either all or none of enum members must have constants: ${e.id.name.name}")
      case WrongEnumConstant(e, meta) =>
        (Some(meta), s"Enum constant out of range: ${e.id.name.name}")
      case EmptyAdtDef(a, meta) =>
        (Some(meta), s"Empty ADT definition: ${a.id.name.name}")
      case UnderscoredDefinitionRetained(s, meta) =>
        (Some(meta), s"Underscored definition retained: ${s.id.name.name}")
      case PathologicGenerics(dto, fields, meta) =>
        (Some(meta), s"Pathologic generics in ${dto.id.name.name}: ${fields.map(_.name.name).mkString(", ")}")
      case SetsCantContainGenerics(dto, fields, meta) =>
        (Some(meta), s"Sets can't contain generics in ${dto.id.name.name}: ${fields.map(_.name.name).mkString(", ")}")
      case MapKeysShouldNotBeGeneric(dto, fields, meta) =>
        (Some(meta), s"Map keys should not be generic in ${dto.id.name.name}: ${fields.map(_.name.name).mkString(", ")}")
      case LockedVersionModified(pkg, version) =>
        (None, s"Locked version modified: $pkg@$version")
      case MissingTypeDef(domain, missing) =>
        (None, s"Missing type definitions in ${domain.id}: ${missing.map(_.name.name).mkString(", ")}")
      case ReferentialCyclesFound(domain, _) =>
        (None, s"Referential cycles found in ${domain.id}")
      case IncorrectRootFound(domain, badRoots) =>
        (None, s"Incorrect root found in ${domain.id}: ${badRoots.map(_.name.name).mkString(", ")}")
      case ConflictingTypeIds(domain, dupes) =>
        (None, s"Conflicting type IDs in ${domain.id}: ${dupes.keys.mkString(", ")}")
      case MissingEvoDiff(prev, next, _) =>
        (None, s"Missing evolution diff: ${prev.id} -> ${next.id}")
      case MissingEvoConversion(prev, next, _, _) =>
        (None, s"Missing evolution conversion: ${prev.id} -> ${next.id}")
      case BrokenConversion(c) =>
        (None, s"Broken conversion: ${c.sourceTpe}")
      case IncorrectConversionApplication(c, _, _, convIssue) =>
        (None, s"Incorrect conversion application: ${c.sourceTpe} - $convIssue")
    }
  }
}

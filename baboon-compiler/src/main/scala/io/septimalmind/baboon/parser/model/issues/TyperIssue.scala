package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.model.DomainKey
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Typedef.ForeignEntry
import io.septimalmind.baboon.typer.model.BaboonLang
import izumi.fundamentals.graphs.{DAGError, ToposortError}
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait TyperIssue extends IssueGroup

object TyperIssue {
  implicit def wrap(issue: TyperIssue): BaboonIssue = BaboonIssue.Typer(issue)

  case class GenericTyperIssue(message: String, meta: RawNodeMeta) extends TyperIssue

  case class ScalarExpected(id: TypeId, meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class CollectionExpected(id: TypeId, meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class NonUniqueDomainVersions(
    duplicateDomainVersion: Map[Version, List[Domain]]
  ) extends TyperIssue

  case class EmptyDomainFamily(pkg: Pkg) extends TyperIssue with BaboonBug

  case class NonUniqueLineages(nonUniqueLineages: Map[Pkg, List[BaboonLineage]]) extends TyperIssue

  case class NonUniqueRawDomainVersion(conflicts: Map[DomainKey, List[RawDomain]]) extends TyperIssue

  case class EmptyFamily(input: List[BaboonParser.Input]) extends TyperIssue with BaboonBug
  case class EmptyFamilyReload(input: List[BaboonParser.ReloadInput]) extends TyperIssue with BaboonBug

  case class UnexpectedBuiltin(id: TypeId.Builtin, pkg: Pkg, meta: RawNodeMeta) extends TyperIssue

  case class UnexpectedNonBuiltin(name: TypeName, pkg: Pkg, path: Scope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class MissingTypeId(domain: Pkg, missing: Set[TypeId], meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueEnumBranches(
    duplicateEnumMembers: Map[String, List[EnumMember]],
    id: TypeId.User,
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class NonUniqueForeignEntries(
    duplicateForeignEntries: Map[BaboonLang, List[ForeignEntry]],
    id: TypeId.User,
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class UnknownForeignLang(lang: String, id: TypeId.User, meta: RawNodeMeta) extends TyperIssue

  case class InvalidRtMapping(id: TypeId.User, reason: String, meta: RawNodeMeta) extends TyperIssue

  case class EmptyEnum(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueFields(id: TypeId.User, duplicateFields: Map[String, List[Field]], meta: RawNodeMeta) extends TyperIssue

  case class MissingContractFields(id: TypeId.User, missingFields: Seq[Field], meta: RawNodeMeta) extends TyperIssue

  case class EmptyAdt(id: TypeId.User, meta: RawNodeMeta) extends TyperIssue

  case class EmptyGenericArgs(id: TypeId, meta: RawNodeMeta) extends TyperIssue

  case class DuplicatedTypedefs(
    model: RawDomain,
    duplicateTypeDefs: Map[TypeId, List[DomainMember]],
  ) extends TyperIssue

  case class EmptyPackageId(header: RawHeader) extends TyperIssue

  case class ScopedRefToNamespacedGeneric(prefix: Seq[RawTypeName], meta: RawNodeMeta) extends TyperIssue

  case class NonUniqueTypedefs(
    duplicateTypedefs: Map[TypeId, List[DomainMember]],
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class NonUniqueScope(
    nonUniqueScopes: Map[Scope.ScopeName, List[Scope.NestedScope[FullRawDefn]]],
    meta: RawNodeMeta,
  ) extends TyperIssue

  case class UnexpectedScoping(e: List[Scope[ExtendedRawDefn]], meta: RawNodeMeta) extends TyperIssue with BaboonBug

  case class TodoTyperIssue(descr: String) extends TyperIssue

  case class NonUniqueMethodNames(serviceName: String, duplicateMethods: Map[String, List[String]], meta: RawNodeMeta) extends TyperIssue

  case class ServiceMissingOutput(serviceName: String, methodName: String, meta: RawNodeMeta) extends TyperIssue

  case class ServiceMultipleOutputs(serviceName: String, methodName: String, count: Int, meta: RawNodeMeta) extends TyperIssue

  case class ServiceMultipleInputs(serviceName: String, methodName: String, count: Int, meta: RawNodeMeta) extends TyperIssue

  case class ServiceMultipleErrors(serviceName: String, methodName: String, count: Int, meta: RawNodeMeta) extends TyperIssue

  case class ScopeCannotBeEmpty(member: RawDefn) extends TyperIssue

  case class BadEnumName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadFieldName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadTypeName(name: String, meta: RawNodeMeta) extends TyperIssue

  case class BadInheritance(
    bad: Map[TypeId.User, List[(Set[TypeId.User], Scope[ExtendedRawDefn])]],
    meta: RawNodeMeta,
  ) extends TyperIssue
    with BaboonBug

  case class CircularInheritance(error: ToposortError[TypeId.User], meta: RawNodeMeta) extends TyperIssue

  case class NameNotFound(pkg: Pkg, name: ScopedRef, meta: RawNodeMeta) extends TyperIssue

  case class UnexpectedScopeLookup(b: Scope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class NamSeqeNotFound(names: Seq[RawTypeName], scope: Scope.SubScope[ExtendedRawDefn], meta: RawNodeMeta) extends TyperIssue

  case class DuplicatedTypes(dupes: Set[TypeId], meta: RawNodeMeta) extends TyperIssue

  case class CircularAlias(name: RawTypeName, meta: RawNodeMeta) extends TyperIssue

  case class WrongParent(id: TypeId.User, id1: TypeId, meta: RawNodeMeta) extends TyperIssue

  /** Two or more branches in an ADT's expanded branch set share the same name (Q-M20-2).
    * Carries the offending branch name and the source `TypeId`s contributing it.
    */
  case class DuplicatedAdtBranches(
    adtId: TypeId.User,
    branchName: String,
    sources: List[TypeId],
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** An `+ X` / `- X` / `^ X` ref in an ADT body does not resolve to an ADT (or to a branch of an
    * ADT). Carries the offending ref and a human-readable reason.
    */
  case class WrongAdtInclusion(
    adtId: TypeId.User,
    includeRef: ScopedRef,
    reason: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** An `+ X` / `- X` / `^ X` ref in an ADT body resolves to a type in a different package
    * (cross-version inclusion). Forbidden because cross-version branch sharing would leak the
    * source version into wire format.
    */
  case class CrossVersionAdtInclusion(
    adtId: TypeId.User,
    includeRef: ScopedRef,
    includeId: TypeId,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** A template declaration has two or more type parameters with the same name (negative-test
    * matrix #4: `data X[T, T] { … }`).
    */
  case class DuplicateTypeParam(name: String, ownerName: String, meta: RawNodeMeta) extends TyperIssue

  /** Two sibling templates at the same namespace level share the same name (PR-29.4-D04).
    * `name` is the template name; `ownerName` is `"<toplevel>"` or the dot-joined namespace path.
    * `meta` is the meta of the second (dropped) definition.
    */
  case class DuplicateTemplateName(name: String, ownerName: String, meta: RawNodeMeta) extends TyperIssue

  /** Arity mismatch at a template instantiation site (negative-test matrix #3):
    * the number of type arguments does not equal the number of type parameters
    * declared on the template.
    *
    * Example: `data X[T] { a: lst[T] }; type XInt = X[i32, str]`
    * produces `TemplateArityMismatch("X", "XInt", expected=1, actual=2, …)`.
    */
  case class TemplateArityMismatch(
    templateName: String,
    aliasName: String,
    expected: Int,
    actual: Int,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** A template instantiation appears in a position where it is forbidden
    * (spec §4, matrix #1 / locked decision #3; or spec §2.5.2, matrix #2).
    *
    * Matrix #1 — instantiation inside a template body field:
    *   `data X[T] { rec: X[T] }` — self-reference in body is forbidden.
    *   `data Tree[T] { children: lst[Tree[T]] }` — container-mediated self-ref.
    *
    * Matrix #2 — instantiation nested inside alias RHS type arguments:
    *   `type Y = X[Z[i32]]` where both X and Z are templates — the inner
    *   constructor `Z[…]` is a template instantiation in alias-arg position,
    *   which is forbidden. Templates may only be instantiated as the right-hand
    *   side of a top-level `type` alias (matrix #1 / decision #3).
    *
    * The rejection happens at substitution time in `TemplateInstantiator`.
    *
    * `containingTemplateName` — the template (or alias) whose body / args
    *   contain the forbidden instantiation.
    * `instantiatedName` — the name of the template being illegally instantiated.
    */
  case class TemplateInstantiationInForbiddenPosition(
    containingTemplateName: String,
    instantiatedName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** An alias whose RHS is a bare (non-instantiated) reference to a template
    * (negative-test matrix #7, spec §2.5.7).
    *
    * Example: `data X[T] { f: T }; type Y = X`
    *
    * Templates must be instantiated with type arguments; a bare reference without
    * brackets is rejected. Detected in `TemplateInstantiator` when a
    * `RawTypeRef.Simple` alias target names a registered template.
    *
    * `templateName` — the template being referenced without instantiation.
    * `aliasName`    — the alias whose RHS carries the bare reference.
    */
  case class TemplateNotInstantiated(
    templateName: String,
    aliasName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** A constructor expression `H[arg]` whose head `H` is neither a builtin
    * collection nor a registered template (negative-test matrix #8, spec §2.5.8).
    *
    * Example: `type Y = i32[X]`
    *
    * Detected in `TemplateInstantiator` when a `RawTypeRef.Constructor` alias
    * target has a head that resolves to neither a template in the registry nor
    * one of the builtin collections (`lst`, `set`, `opt`, `map`).
    *
    * `head`      — the name used in head position of the constructor.
    * `aliasName` — the alias whose RHS carries the constructor.
    */
  case class NotATemplate(
    head: String,
    aliasName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** A template body carries a `: derived[…]` annotation, which is forbidden by
    * locked decision #6 (spec §5.3). Derivation must be written on the alias, not
    * on the template itself. Detected at registry-build time in
    * `TemplateRegistryBuilder` (before the template body is ever instantiated).
    *
    * `templateName` — the template that illegally carries a `derived` annotation.
    */
  case class TemplateBodyCarriesDerived(
    templateName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** PR-33.2 (M33) [PR-33.2-D02]: a `- Template[Args]` or `^ Template[Args]` arm was
    * substituted, but the resulting member list contains a non-FieldDef member (e.g. a
    * concrete `+ ParentRef`, ContractRef, etc.). The `-` and `^` operators only have a
    * defined semantics over a flat field list (field-by-field equality / intersection); a
    * non-FieldDef contributor would be silently dropped, masking the omission. Restrict
    * template bodies used in `-`/`^` position to flat field lists.
    *
    * `templateName`        — the template whose body was substituted.
    * `receivingName`       — the DTO/Contract receiving the `-`/`^` arm.
    * `kind`                — `"minus"` for `-`, `"caret"` for `^`.
    * `offendingMemberKind` — the simple-class-name of the offending RawDtoMember subtype, OR the
    *                         sentinel `"empty body"` when the substituted body is empty (PR-33.4).
    */
  case class TemplateBodyNotFlatForRemoval(
    templateName: String,
    receivingName: String,
    kind: String,
    offendingMemberKind: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** T38 (auto-extracted-contracts): a `has (mirror|contract) <Name>` extraction clause appears on
    * an invalid host. Valid hosts are TEMPLATED `data`, TEMPLATED `id`, and TEMPLATED `adt`
    * (ADT-level) ONLY. A `has` clause on a non-templated `id`, on any other non-template
    * declaration, in a `contract` body, or on an ADT branch is rejected.
    *
    * `hostName`        — the declaration carrying the offending `has` clause.
    * `hostDescription` — a short phrase describing why the host is invalid (e.g.
    *                     `"a non-templated id"`, `"a contract body"`, `"an ADT branch"`).
    */
  case class ExtractionHostInvalid(
    hostName: String,
    hostDescription: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** T38 (auto-extracted-contracts): the name of a synthesized extraction contract collides with an
    * existing type, a template name, or another extraction's name. There is no structural merging;
    * a name collision is always an error.
    *
    * `extractionName` — the `<Name>` requested by the `has` clause.
    * `hostName`       — the templated host that declared the extraction.
    */
  case class ExtractionNameCollision(
    extractionName: String,
    hostName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  /** T38 (auto-extracted-contracts): the RESOLVED field set of a synthesized extraction contract is
    * empty. The emptiness check runs post-translation because raw-level emptiness is insufficient:
    * a structural member like `+ EmptyDto` could resolve to an empty field set.
    *
    * `extractionName` — the name of the empty synthesized contract.
    * `hostName`       — the templated host that declared the extraction.
    */
  case class ExtractionEmpty(
    extractionName: String,
    hostName: String,
    meta: RawNodeMeta,
  ) extends TyperIssue

  implicit val todoPrinter: IssuePrinter[TodoTyperIssue] =
    (issue: TodoTyperIssue) => {
      issue.descr
    }

  implicit val dagErrorPrinter: IssuePrinter[DagError] =
    (issue: DagError) => {
      val repr = issue.e match {
        case DAGError.LoopBreakerFailed(loopMember) => s"Loop at (${loopMember.id}, ${loopMember.version})"
        case DAGError.UnexpectedLoops()             => "Unexpected loops (BUG)"
      }

      s"""${extractLocation(issue.meta)}
         |Import loops found: $repr
         |""".stripMargin
    }

  implicit val scalaExpectedPrinter: IssuePrinter[ScalarExpected] =
    (bug: ScalarExpected) => {
      s"""${extractLocation(bug.meta)}
         |Scalar type is expected, instead provided: ${bug.id.toString}
         |""".stripMargin
    }

  implicit val collectionExpectedPrinter: IssuePrinter[CollectionExpected] =
    (bug: CollectionExpected) => {
      s"""${extractLocation(bug.meta)}
         |One of collection types expected: map, opt, list, set
         |Instead provided: ${bug.id.toString}
         |""".stripMargin
    }

  implicit val nonUniqueDomainVersionsPrinter: IssuePrinter[NonUniqueDomainVersions] =
    (issue: NonUniqueDomainVersions) => {
      val stringProblems = issue.duplicateDomainVersion.map {
        case (version, domains) =>
          s"""Version: $version => models: ${domains
              .map(_.id.toString)
              .mkString(", ")}
             |""".stripMargin
      }
        .mkString("\n")
      s"""Duplicate domain versions have been found
         |$stringProblems
         |""".stripMargin
    }

  implicit val emptyDomainFamilyPrinter: IssuePrinter[EmptyDomainFamily] =
    (bug: EmptyDomainFamily) => {
      s"Empty model: ${bug.pkg.toString}"
    }

  implicit val nonUniqueLineagesPrinter: IssuePrinter[NonUniqueLineages] =
    (issue: NonUniqueLineages) => {
      s"Non unique models found: ${issue.nonUniqueLineages.keys.toList.mkString(", ")}"
    }

  implicit val emptyFamilyPrinter: IssuePrinter[EmptyFamily] =
    (bug: EmptyFamily) => {
      s"Empty models in files:${bug.input.niceList()}"
    }

  implicit val emptyFamilyReloadPrinter: IssuePrinter[EmptyFamilyReload] =
    (bug: EmptyFamilyReload) => {
      val inputs = bug.input.map(_.path).niceList()
      s"Empty models in files:$inputs"
    }

  implicit val nonUniqueRawDomainVersionPrinter: IssuePrinter[NonUniqueRawDomainVersion] =
    (issue: NonUniqueRawDomainVersion) => {
      val stringProblems = issue.conflicts.map {
        case (key, _) =>
          s"""Version: ${key.version} in ${key.id}""".stripMargin
      }
        .mkString("\n")
      s"""Domains with duplicated versions were found (import resolution phase)
         |$stringProblems
         |""".stripMargin
    }

  implicit val unexpectedBuiltinPrinter: IssuePrinter[UnexpectedBuiltin] =
    (issue: UnexpectedBuiltin) => {
      s"""${extractLocation(issue.meta)}
         |In Model ${issue.pkg.toString}, the type ${issue.id.toString} - is a built in type it cannot be used as a user defined type
         |""".stripMargin
    }

  implicit val unexpectedNonBuiltin: IssuePrinter[UnexpectedNonBuiltin] =
    (issue: UnexpectedNonBuiltin) => {
      s"""${extractLocation(issue.meta)}
         |Model: ${issue.pkg.toString}
         |The type ${issue.name.name} takes a builtin position but there is no such builtin type""".stripMargin
    }

  implicit val missingTypeIdPrinter: IssuePrinter[MissingTypeId] =
    (issue: MissingTypeId) => {
      s"""${extractLocation(issue.meta)}
         |Missing type ids: ${issue.missing.toList.map(_.name.name).niceList()}
         |""".stripMargin
    }

  implicit val nonUniqueEnumBranchesPrinter: IssuePrinter[NonUniqueEnumBranches] = (issue: NonUniqueEnumBranches) => {
    val stringProblems = issue.duplicateEnumMembers.values
      .map(_.map(_.name).niceList())
      .mkString("\n")
    s"""${extractLocation(issue.meta)}
       |Enum ${issue.id.toString} has members with identical names:$stringProblems
       |""".stripMargin
  }

  implicit val nonUniqueForeignEntriesPrinter: IssuePrinter[NonUniqueForeignEntries] =
    (issue: NonUniqueForeignEntries) => {
      val stringProblems = issue.duplicateForeignEntries.values
        .map(_.map(_.lang.asString).niceList())
        .mkString("\n")
      s"""${extractLocation(issue.meta)}
         |Foreign type ${issue.id.toString} has members with identical names:$stringProblems
         |""".stripMargin
    }

  implicit val unknownForeignLangPrinter: IssuePrinter[UnknownForeignLang] =
    (issue: UnknownForeignLang) => {
      s"""${extractLocation(issue.meta)}
         |Unknown foreign language identifier '${issue.lang}' in foreign type ${issue.id.toString}
         |Valid identifiers: ${BaboonLang.all.map(_.asString).mkString(", ")}, rt
         |""".stripMargin
    }

  implicit val invalidRtMappingPrinter: IssuePrinter[InvalidRtMapping] =
    (issue: InvalidRtMapping) => {
      s"""${extractLocation(issue.meta)}
         |Invalid rt mapping in foreign type ${issue.id.toString}: ${issue.reason}
         |""".stripMargin
    }

  implicit val emptyEnumPrinter: IssuePrinter[EmptyEnum] =
    (issue: EmptyEnum) => {
      s"""${extractLocation(issue.meta)}
         |Found empty enum: ${issue.id.toString}
         |""".stripMargin
    }

  implicit val nonUniqueFieldsPrinter: IssuePrinter[NonUniqueFields] =
    (issue: NonUniqueFields) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.id.toString} has fields with identical names:${issue.duplicateFields.values.flatten
          .niceList()}
         |""".stripMargin
    }

  implicit val emptyADTPrinter: IssuePrinter[EmptyAdt] = (issue: EmptyAdt) => {
    s"""${extractLocation(issue.meta)}
       |Found empty ADT: ${issue.id.toString}
       |""".stripMargin
  }

  implicit val emptyGenericArgsPrinter: IssuePrinter[EmptyGenericArgs] =
    (issue: EmptyGenericArgs) => {
      s"""${extractLocation(issue.meta)}
         |The type ${issue.id.name.name} is expected to be used with type argument
         |""".stripMargin
    }

  implicit val duplicatedTypedefsPrinter: IssuePrinter[DuplicatedTypedefs] =
    (issue: DuplicatedTypedefs) => {
      val duplicateTypesString = issue.duplicateTypeDefs.map {
        case (typeId, members) =>
          s"type: $typeId => members: ${members.map(_.id.name.name).mkString(", ")}"
      }
      s"""${extractLocation(issue.model.header.meta)}
         |Duplicate type definitions have been found:${duplicateTypesString
          .niceList()}
         |""".stripMargin
    }

  implicit val emptyPackageIdPrinter: IssuePrinter[EmptyPackageId] =
    (issue: EmptyPackageId) => {
      s"""${extractLocation(issue.header.meta)}
         |There is an empty package name
         |""".stripMargin
    }

  implicit val nonUniqueTypedefsPrinter: IssuePrinter[NonUniqueTypedefs] =
    (issue: NonUniqueTypedefs) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate type members found: ${issue.duplicateTypedefs.values.flatten
          .map(_.id.name.name)
          .mkString}
         |""".stripMargin
    }

  implicit val nonUniqueScopePrinter: IssuePrinter[NonUniqueScope] =
    (issue: NonUniqueScope) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate scopes have been found:${issue.nonUniqueScopes
          .map(_._2.map(_.name.name).mkString(", "))
          .niceList()}
         |""".stripMargin
    }

  implicit val unexpectedScopingPrinter: IssuePrinter[UnexpectedScoping] =
    (bug: UnexpectedScoping) => {
      s"""${extractLocation(bug.meta)}
         |Unexpected scoping: ${bug.e.niceList()}
         |""".stripMargin
    }

  implicit val scopeCannotBeEmptyPrinter: IssuePrinter[ScopeCannotBeEmpty] =
    (issue: ScopeCannotBeEmpty) => {
      val memberType = issue.member match {
        case _: RawDto        => "DTO"
        case _: RawIdentifier => "Id"
        case _: RawEnum       => "Enum"
        case _: RawAdt        => "ADT"
        case _: RawForeign    => "Foreign"
        case _: RawContract   => "Contract"
        case _: RawNamespace  => "Namespace"
        case _: RawService    => "Service"
        case _: RawAlias      => "Alias"
      }
      s"""${extractLocation(issue.member.meta)}
         |Found an empty $memberType: ${issue.member.name.name}
         |""".stripMargin
    }

  implicit val badEnumNamePrinter: IssuePrinter[BadEnumName] =
    (issue: BadEnumName) => {
      s"""${extractLocation(issue.meta)}
         |Bad enum name: ${issue.name}
         |""".stripMargin
    }

  implicit val badFieldNamePrinter: IssuePrinter[BadFieldName] =
    (issue: BadFieldName) => {
      s"""${extractLocation(issue.meta)}
         |Bad field name: ${issue.name}
         |""".stripMargin
    }

  implicit val badTypeNamePrinter: IssuePrinter[BadTypeName] =
    (issue: BadTypeName) => {
      s"""${extractLocation(issue.meta)}
         |Bad type name: ${issue.name}
         |""".stripMargin
    }

  implicit val badInheritancePrinter: IssuePrinter[BadInheritance] =
    (bug: BadInheritance) => {
      val badStr = bug.bad.map {
        case (id, deps) =>
          val depsStr = deps.map {
            case (deps, scope) =>
              s"""Scope: ${scope.toString}
                 |Dependencies: ${deps.map(_.toString).mkString(", ")}
                 |""".stripMargin
          }
            .niceList()

          s"""Type: ${id.toString}
             |$depsStr
             |""".stripMargin
      }
        .niceList("\t")
      s"""${extractLocation(bug.meta)}
         |Bad inheritance:
         |$badStr
         |""".stripMargin
    }

  implicit val circularInheritancePrinter: IssuePrinter[CircularInheritance] =
    (issue: CircularInheritance) => {
      val errorString = issue.error match {
        case ToposortError.UnexpectedLoop(_, matrix) =>
          matrix.links.map {
            case (typeId, children) =>
              s"type: ${typeId.name.name} => children: ${children.toList.map(_.name.name).mkString(", ")}"
          }
            .niceList()
        case _ => ""
      }
      s"""${extractLocation(issue.meta)}
         |Circular inheritance have been found:$errorString
         |""".stripMargin
    }

  implicit val nameNotFoundPrinter: IssuePrinter[NameNotFound] =
    (issue: NameNotFound) => {
      s"""${extractLocation(issue.meta)}
         |Type not found: ${issue.name.path.head.name}
         |""".stripMargin
    }

  implicit val unexpectedScopeLookupPrinter: IssuePrinter[UnexpectedScopeLookup] = (issue: UnexpectedScopeLookup) => {
    s"""${extractLocation(issue.meta)}
       |Unexpected scope: ${issue.b.toString}
       |""".stripMargin
  }

  implicit val namSeqeNotFoundPrinter: IssuePrinter[NamSeqeNotFound] =
    (issue: NamSeqeNotFound) => {
      s"""${extractLocation(issue.meta)}
         |Members: ${issue.names
          .map(_.name)
          .mkString} are not found in scope: ${issue.scope.name.name}
         |""".stripMargin
    }

  implicit val circularAliasPrinter: IssuePrinter[CircularAlias] =
    (issue: CircularAlias) => {
      s"""${extractLocation(issue.meta)}
         |Circular type alias detected for: ${issue.name.name}
         |""".stripMargin
    }

  implicit val duplicatedTypesPrinter: IssuePrinter[DuplicatedTypes] =
    (issue: DuplicatedTypes) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate types have been found:${issue.dupes
          .map(_.name.name)
          .niceList()}
         |""".stripMargin
    }

  implicit val wrongParentPrinter: IssuePrinter[WrongParent] =
    (issue: WrongParent) => {
      s"""${extractLocation(issue.meta)}
         |DTO parent is expected instead provided => name:${issue.id.toString} type:${issue.id1.name.name}
         |""".stripMargin
    }

  implicit val duplicatedAdtBranchesPrinter: IssuePrinter[DuplicatedAdtBranches] =
    (issue: DuplicatedAdtBranches) => {
      val sourcesStr = issue.sources.map(_.toString).mkString(", ")
      s"""${extractLocation(issue.meta)}
         |ADT ${issue.adtId.toString} has duplicate branches named '${issue.branchName}' contributed by: $sourcesStr
         |""".stripMargin
    }

  implicit val wrongAdtInclusionPrinter: IssuePrinter[WrongAdtInclusion] =
    (issue: WrongAdtInclusion) => {
      val refStr = issue.includeRef.path.toList.map(_.name).mkString(".")
      s"""${extractLocation(issue.meta)}
         |ADT ${issue.adtId.toString} has an invalid inheritance arm referencing '$refStr': ${issue.reason}
         |""".stripMargin
    }

  implicit val crossVersionAdtInclusionPrinter: IssuePrinter[CrossVersionAdtInclusion] =
    (issue: CrossVersionAdtInclusion) => {
      val refStr = issue.includeRef.path.toList.map(_.name).mkString(".")
      s"""${extractLocation(issue.meta)}
         |ADT ${issue.adtId.toString} cannot include '$refStr' (resolved to ${issue.includeId.toString}) — cross-version ADT inclusion is forbidden
         |""".stripMargin
    }

  implicit val missingContractFields: IssuePrinter[MissingContractFields] =
    (issue: MissingContractFields) => {
      s"""${extractLocation(issue.meta)}
         |Contract fields were removed => name:${issue.id.toString} type:${issue.id}, missing: ${issue.missingFields
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val scopedRefToNamespacedGeneric: IssuePrinter[ScopedRefToNamespacedGeneric] =
    (issue: ScopedRefToNamespacedGeneric) => {
      s"""${extractLocation(issue.meta)}
         |A reference to user-defined generic, which is not supported, prefix: ${issue.prefix
          .map(_.name)
          .mkString(".")}
         |""".stripMargin
    }

  implicit val nonUniqueMethodNamesPrinter: IssuePrinter[NonUniqueMethodNames] = (issue: NonUniqueMethodNames) => {
    val duplicatesString = issue.duplicateMethods.map {
      case (methodName, occurrences) =>
        s"method: $methodName => occurrences: ${occurrences.mkString(", ")}"
    }.mkString("\n")
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName} has methods with identical names:
       |$duplicatesString
       |""".stripMargin
  }

  implicit val serviceMissingOutputPrinter: IssuePrinter[ServiceMissingOutput] = (issue: ServiceMissingOutput) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: missing output type definition
       |""".stripMargin
  }

  implicit val serviceMultipleOutputsPrinter: IssuePrinter[ServiceMultipleOutputs] = (issue: ServiceMultipleOutputs) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: has ${issue.count} output type definitions, expected exactly 1
       |""".stripMargin
  }

  implicit val serviceMultipleInputsPrinter: IssuePrinter[ServiceMultipleInputs] = (issue: ServiceMultipleInputs) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: has ${issue.count} input type definitions, expected at most 1
       |""".stripMargin
  }

  implicit val serviceMultipleErrorsPrinter: IssuePrinter[ServiceMultipleErrors] = (issue: ServiceMultipleErrors) => {
    s"""${extractLocation(issue.meta)}
       |Service ${issue.serviceName}, method ${issue.methodName}: has ${issue.count} error type definitions, expected at most 1
       |""".stripMargin
  }

  implicit val genericTyperIssue: IssuePrinter[GenericTyperIssue] = (issue: GenericTyperIssue) => {
    s"""${extractLocation(issue.meta)}: ${issue.message}""".stripMargin
  }

  implicit val duplicateTypeParamPrinter: IssuePrinter[DuplicateTypeParam] =
    (issue: DuplicateTypeParam) => {
      s"""${extractLocation(issue.meta)}
         |Template '${issue.ownerName}' declares duplicate type parameter '${issue.name}'
         |""".stripMargin
    }

  implicit val duplicateTemplateNamePrinter: IssuePrinter[DuplicateTemplateName] =
    (issue: DuplicateTemplateName) => {
      s"""${extractLocation(issue.meta)}
         |Duplicate template name '${issue.name}' at owner '${issue.ownerName}'
         |""".stripMargin
    }

  implicit val templateArityMismatchPrinter: IssuePrinter[TemplateArityMismatch] =
    (issue: TemplateArityMismatch) => {
      s"""${extractLocation(issue.meta)}
         |Template '${issue.templateName}' instantiated with wrong arity in alias '${issue.aliasName}': expected ${issue.expected} type argument(s), got ${issue.actual}
         |""".stripMargin
    }

  implicit val templateInstantiationInForbiddenPositionPrinter: IssuePrinter[TemplateInstantiationInForbiddenPosition] =
    (issue: TemplateInstantiationInForbiddenPosition) => {
      s"""${extractLocation(issue.meta)}
         |Template instantiation `${issue.instantiatedName}` is forbidden here — templates may only be instantiated as the right-hand side of a top-level `type` alias (matrix #1 / decision #3). Containing context: '${issue.containingTemplateName}'
         |""".stripMargin
    }

  implicit val templateNotInstantiatedPrinter: IssuePrinter[TemplateNotInstantiated] =
    (issue: TemplateNotInstantiated) => {
      s"""${extractLocation(issue.meta)}
         |Alias '${issue.aliasName}' references template '${issue.templateName}' without type arguments — templates must be instantiated with brackets, e.g. '${issue.templateName}[T]' (spec §2.5.7, matrix #7)
         |""".stripMargin
    }

  implicit val notATemplatePrinter: IssuePrinter[NotATemplate] =
    (issue: NotATemplate) => {
      s"""${extractLocation(issue.meta)}
         |Alias '${issue.aliasName}' uses '${issue.head}' as a generic constructor head, but '${issue.head}' is not a template — only user-defined templates and builtin collections (lst, set, opt, map) may be used with type arguments (spec §2.5.8, matrix #8)
         |""".stripMargin
    }

  implicit val templateBodyCarriesDerivedPrinter: IssuePrinter[TemplateBodyCarriesDerived] =
    (issue: TemplateBodyCarriesDerived) => {
      s"""${extractLocation(issue.meta)}
         |Template '${issue.templateName}' carries a ':derived[…]' annotation, which is forbidden on template bodies — write the annotation on the instantiating alias instead (spec §5.3, locked decision #6)
         |""".stripMargin
    }

  implicit val templateBodyNotFlatForRemovalPrinter: IssuePrinter[TemplateBodyNotFlatForRemoval] =
    (issue: TemplateBodyNotFlatForRemoval) => {
      val opGloss = issue.kind match {
        case "minus" => "'-'"
        case "caret" => "'^'"
        case other   => s"'$other'"
      }
      // PR-33.4-D03: the sentinel "empty body" (produced when the substituted body is empty under `^`
      // or `-`) requires a distinct, operator-aware message — "contains a non-field member (empty body)"
      // would be paradoxical. PR-33.4-D01 extends the sentinel to the `minus` operator as well.
      val detail = if (issue.offendingMemberKind == "empty body") {
        val opContext = issue.kind match {
          case "caret" => "intersection over an empty field set would be a silent no-op"
          case "minus" => "removal of an empty field set would be a silent no-op"
          case other   => s"applying $other over an empty field set would be a silent no-op"
        }
        s"the substituted body is empty ($opContext; caught at lowering time to surface the likely mistake)"
      } else {
        s"the substituted body contains a non-field member (${issue.offendingMemberKind}). The $opGloss operator only operates on a flat field list — restrict the template body to FieldDefs when it is intended for use under '-' or '^'"
      }
      s"""${extractLocation(issue.meta)}
         |Template '${issue.templateName}' cannot be used under the $opGloss operator on '${issue.receivingName}': $detail.
         |""".stripMargin
    }

  implicit val extractionHostInvalidPrinter: IssuePrinter[ExtractionHostInvalid] =
    (issue: ExtractionHostInvalid) => {
      s"""${extractLocation(issue.meta)}
         |Invalid extraction host '${issue.hostName}': a `has (mirror|contract) …` clause is only allowed on a templated `data`, templated `id`, or templated `adt`, but '${issue.hostName}' is ${issue.hostDescription}.
         |""".stripMargin
    }

  implicit val extractionNameCollisionPrinter: IssuePrinter[ExtractionNameCollision] =
    (issue: ExtractionNameCollision) => {
      s"""${extractLocation(issue.meta)}
         |Extraction '${issue.extractionName}' on host '${issue.hostName}' collides with an existing type, template, or another extraction of the same name — extraction names must be pairwise-distinct and must not shadow any existing declaration.
         |""".stripMargin
    }

  implicit val extractionEmptyPrinter: IssuePrinter[ExtractionEmpty] =
    (issue: ExtractionEmpty) => {
      s"""${extractLocation(issue.meta)}
         |Extraction '${issue.extractionName}' on host '${issue.hostName}' resolves to an empty field set — a `has (mirror|contract) …` extraction must contribute at least one param-free field.
         |""".stripMargin
    }

  private def extractLocation(meta: RawNodeMeta): String = {
    meta.pos match {
      case full: InputPointer.Full =>
        s"""In model: ${full.file.name.theString}
           |  line=${full.start.line} position=${full.start.column}
           |  line=${full.stop.line}  position=${full.stop.column}""".stripMargin
      case offset: InputPointer.Offset =>
        s"""In model: ${offset.file.name.theString}
           |  line=${offset.start.line} position=${offset.start.column}""".stripMargin
      case file: InputPointer.JustFile =>
        s"In model: ${file.file.name.theString}"
      case InputPointer.Undefined => ""
    }
  }

  case class DagError(e: DAGError[DomainKey], meta: RawNodeMeta) extends TyperIssue
}

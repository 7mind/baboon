package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.graphs.tools.cycles.LoopDetector
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait VerificationIssue extends IssueGroup

object VerificationIssue {
  implicit def wrap(issue: VerificationIssue): BaboonIssue = BaboonIssue.Verification(issue)

  case class LockedVersionModified(pkg: Pkg, version: Version) extends VerificationIssue

  case class MissingTypeDef(domain: Domain, missing: Set[TypeId]) extends VerificationIssue

  case class ReferentialCyclesFound(domain: Domain, loops: Set[LoopDetector.Cycles[TypeId]]) extends VerificationIssue

  case class IncorrectRootFound(domain: Domain, badRoots: Seq[TypeId.User]) extends VerificationIssue

  case class ConflictingDtoFields(dto: TypeId.User, dupes: Map[String, List[Field]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingEnumBranches(uEnum: Typedef.Enum, dupes: Map[String, NEList[EnumMember]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingAdtBranches(adt: Typedef.Adt, dupes: Map[String, NEList[TypeId.User]], meta: RawNodeMeta) extends VerificationIssue

  case class ConflictingTypeIds(domain: Domain, dupes: Map[String, Iterable[DomainMember]]) extends VerificationIssue

  case class BadFieldNames(e: TypeId.User, bad: Seq[String], meta: RawNodeMeta) extends VerificationIssue

  case class EmptyEnumDef(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class EitherAllOrNoneEnumMembersMustHaveConstants(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class WrongEnumConstant(e: Typedef.Enum, meta: RawNodeMeta) extends VerificationIssue

  case class EmptyAdtDef(a: Typedef.Adt, meta: RawNodeMeta) extends VerificationIssue

  case class UnderscoredDefinitionRetained(s: DomainMember.User, meta: RawNodeMeta) extends VerificationIssue

  case class MissingEvoDiff(prev: Domain, next: Domain, orphanDiffs: Set[TypeId]) extends VerificationIssue with BaboonBug

  case class MissingEvoConversion(prev: Domain, next: Domain, missingConversions: Set[TypeId], extraConversions: Set[TypeId]) extends VerificationIssue with BaboonBug

  case class BrokenConversion(c: Conversion) extends VerificationIssue with BaboonBug

  sealed trait ConversionIssue
  object ConversionIssue {
    case object TypeMismatch extends ConversionIssue
    case object RemovedEnumBranches extends ConversionIssue
    case object RemovedAdtBranches extends ConversionIssue
    case object RemovedDtoFields extends ConversionIssue
    case object IncorrectFields extends ConversionIssue
    case object ConflictingFields extends ConversionIssue
  }
  case class IncorrectConversionApplication(c: Conversion, o: DomainMember, n: DomainMember, issue: ConversionIssue) extends VerificationIssue with BaboonBug

  case class PathologicGenerics(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  case class SetsCantContainGenerics(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  case class MapKeysShouldNotBeGeneric(dto: Typedef.Dto, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `any[T]` rule: T must be a user-defined DTO/ADT/Enum. Builtin scalars, collections,
  // nested `any`, and foreign types are rejected. The owner is any `Typedef.User`
  // — DTOs, Contracts, ADTs (own fields), and Services (method sig/out/err) all
  // participate. For Service methods, badFields synthesises a Field whose name is
  // `<methodName>.{sig|out|err}` and whose tpe is the offending TypeRef.
  case class AnyUnderlyingNotUserType(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `any[T]` rule: T must carry `derived[ueba]`. Checked via `Domain.derivationRequests`.
  // TODO (M2+): also require `derived[json]` when JSON codegen is enabled for the containing type.
  case class AnyUnderlyingLacksUebaDerivation(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `any` cannot appear as a map key (no stable hash/equality).
  case class AnyAsMapKey(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `any` cannot appear as a set element (no stable hash/equality).
  case class AnyAsSetElement(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `id` field-type rules: float types (f32/f64/f128) are not allowed in identifiers
  // because float toString is locale-sensitive and not bit-stable across languages.
  case class IdentifierFieldFloatType(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `id` field-type rules: collections (lst/set/map/opt) are not allowed in identifiers;
  // only primitive or nested-identifier scalar fields are permitted.
  case class IdentifierFieldCollection(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // `id` field-type rules: a user-defined type referenced from an `id` field must itself
  // be an `id` type. Regular `data`, `adt`, `enum`, `foreign`, `service`, and `contract`
  // references are rejected. Carries the offending TypeId for the diagnostic message.
  case class IdentifierFieldUserNotIdentifier(owner: Typedef.User, badFields: List[(Field, TypeId)], meta: RawNodeMeta) extends VerificationIssue

  // `any` is not allowed in identifiers. This is the SOLE rejection path for a
  // plain `any` scalar field in an `id` DTO — `checkAnyFields` only rejects
  // `any[X]` underlyings, `any` as map key, and `any` as set element.
  case class IdentifierFieldAny(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta) extends VerificationIssue

  // M19/BAB-A02: a user type used as a JSON map key fails the wrapper-eligibility
  // procedure (single-primitive grounding, no contracts, no opt/collection field, etc.).
  // `reason` carries the structured cause so the printer can surface an actionable error.
  sealed trait IneligibleMapKeyReason
  object IneligibleMapKeyReason {
    case class MultiFieldNonIdWrapper(typeId: TypeId) extends IneligibleMapKeyReason
    case class CollectionField(typeId: TypeId) extends IneligibleMapKeyReason
    case class OptionField(typeId: TypeId) extends IneligibleMapKeyReason
    case class WrapperWithContracts(typeId: TypeId) extends IneligibleMapKeyReason
    case class IneligibleUserType(typeId: TypeId) extends IneligibleMapKeyReason
    case class CyclicWrapper(typeId: TypeId) extends IneligibleMapKeyReason
    case class FloatWrapper(typeId: TypeId) extends IneligibleMapKeyReason
  }

  // M19/BAB-A02: a `map[K, V]` field in `owner` whose `K` is a user type that fails the
  // eligibility procedure (see `BaboonValidator.checkUserMapKeysEligibility`). Multi-field
  // non-id DTOs, collection-/opt-wrapped wrappers, ADTs/contracts/services, cyclic wrappers,
  // and float wrappers (Q-M19-2 asymmetric override) are rejected.
  case class IneligibleUserMapKey(owner: Typedef.User, badField: Field, reason: IneligibleMapKeyReason, meta: RawNodeMeta) extends VerificationIssue

  // M19/BAB-A02 / Q-FU-1: a `map[K, V]` field whose owner derives `json` (or `ueba`) and whose
  // user-typed key K does not derive the same. `missing` is "json" or "ueba".
  case class MapKeyMissingDerivation(owner: Typedef.User, badField: Field, keyType: TypeId, missing: String, meta: RawNodeMeta) extends VerificationIssue

  // D2: a plain field (or a collection element) whose resolved user type is a
  // `NonDataTypedef` (a `contract` or `service`). Contracts/services are not data
  // types — they cannot be carried as a field value or codec'd — yet the typer's
  // `convertTpe` admits any `TypeId.User` (which extends `TypeId.Scalar`) as a
  // field type with only a `ScalarExpected` guard. `badFields` pairs each offending
  // field with the offending `TypeId.User` it resolved to.
  case class DataTypeExpectedField(owner: Typedef.User, badFields: List[(Field, TypeId.User)], meta: RawNodeMeta) extends VerificationIssue

  implicit val lockedVersionModifiedPrinter: IssuePrinter[LockedVersionModified] =
    (issue: LockedVersionModified) => {
      s"""Model ${issue.pkg.toString}@${issue.version} was modified but it's not the latest version so it's locked with the lockfile""".stripMargin
    }

  implicit val missingTypeDefPrinter: IssuePrinter[MissingTypeDef] =
    (issue: MissingTypeDef) => {
      s"""Model: ${issue.domain.id.toString}
         |Has missing type definitions: ${issue.missing.map(_.name.name).mkString}
         |""".stripMargin
    }

  implicit val referentialCyclesFoundPrinter: IssuePrinter[ReferentialCyclesFound] =
    (issue: ReferentialCyclesFound) => {
      val stringLoops = issue.loops.toList
        .map(
          cycle =>
            s"type: ${cycle.node.name.name} => loop: ${cycle.loops
                .flatMap(_.loop.map(_.name.name))
                .mkString("->")}"
        )
        .niceList()
      s"""Model: ${issue.domain.id.toString}
         |Referential cycles have been found:$stringLoops
         |""".stripMargin
    }

  implicit val conflictingDtoFieldsPrinter: IssuePrinter[ConflictingDtoFields] =
    (issue: ConflictingDtoFields) => {
      s"""${extractLocation(issue.meta)}
         |In DTO: ${issue.dto.name.name}
         |Conflicting fields have been found:${issue.dupes.values.flatten
          .niceList()}
         |""".stripMargin
    }

  implicit val conflictingEnumBranchesPrinter: IssuePrinter[ConflictingEnumBranches] =
    (issue: ConflictingEnumBranches) => {
      s"""${extractLocation(issue.meta)}
         |In Enum: ${issue.uEnum.id.name.name}
         |Conflicting members have been found: ${issue.dupes.values.flatten
          .map(_.name)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val conflictingAdtBranchesPrinter: IssuePrinter[ConflictingAdtBranches] =
    (issue: ConflictingAdtBranches) => {
      s"""${extractLocation(issue.meta)}
         |In ADT: ${issue.adt.id.name.name}
         |Conflicting branches have been found: ${issue.dupes.values.flatten
          .map(_.name.name)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val conflictingTypeIdsPrinter: IssuePrinter[ConflictingTypeIds] =
    (issue: ConflictingTypeIds) => {
      s"""In model: ${issue.domain.id.toString}
         |Conflicting types have been found: ${issue.dupes.values.flatten
          .map(_.id.toString)
          .mkString(", ")}
         |""".stripMargin
    }

  implicit val emptyEnumDefPrinter: IssuePrinter[EmptyEnumDef] =
    (issue: EmptyEnumDef) => {
      s"""${extractLocation(issue.meta)}
         |Empty enum: ${issue.e.id.toString}
         |""".stripMargin
    }

  implicit val emptyAdtDefPrinter: IssuePrinter[EmptyAdtDef] =
    (issue: EmptyAdtDef) => {
      s"""${extractLocation(issue.meta)}
         |Empty ADT: ${issue.a.id.toString}
         |""".stripMargin
    }

  implicit val missingEvoDiffPrinter: IssuePrinter[MissingEvoDiff] =
    new BugPrinter[MissingEvoDiff] {
      override def errorMessage(bug: MissingEvoDiff): String = {
        s"""Orphan differences (entries pointing at types absent from both versions):${bug.orphanDiffs.niceList()}
           |In model: ${bug.prev.id}
           |""".stripMargin
      }
    }

  implicit val missingEvoConversionPrinter: IssuePrinter[MissingEvoConversion] =
    new BugPrinter[MissingEvoConversion] {
      override def errorMessage(bug: MissingEvoConversion): String = {
        s"""Missing evolution conversion
           |Missing conversions:${(bug.missingConversions ++ bug.extraConversions)
            .niceList()}
           |In model:
           |   Previous version: ${bug.prev.id.toString}
           |   Next version:     ${bug.next.id.toString}
           |""".stripMargin
      }
    }

  implicit val brokenConversionPrinter: IssuePrinter[BrokenConversion] =
    new BugPrinter[BrokenConversion] {
      override def errorMessage(bug: BrokenConversion): String = {
        s"""Removed type: ${bug.c.sourceTpe.toString}
           |Is not present as removed type in conversion:
           |${bug.c.toString}
           |""".stripMargin
      }
    }

  implicit val incorrectConversionApplicationPrinter: IssuePrinter[IncorrectConversionApplication] =
    new BugPrinter[IncorrectConversionApplication] {
      override def errorMessage(bug: IncorrectConversionApplication): String = {
        val location = (bug.o, bug.n) match {
          case (old: DomainMember.User, newMember: DomainMember.User) =>
            s"""Old member location:
               |   ${extractLocation(old.meta)}
               |
               |New member location:
               |   ${extractLocation(newMember.meta)}
               |""".stripMargin
          case _ => ""
        }
        location +
        s"""Incorrect conversion: ${bug.c.toString}
           |Member:
           |   Old version: ${bug.o.id.toString}
           |   New version: ${bug.n.id.toString}
           |Issue: ${bug.issue.toString}
           |""".stripMargin
      }
    }

  implicit val pathologicGenericsPrinter: IssuePrinter[PathologicGenerics] =
    (issue: PathologicGenerics) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has nested option fields:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val setsCantContainGenericsPrinter: IssuePrinter[SetsCantContainGenerics] =
    (issue: SetsCantContainGenerics) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has set fields with generic type parameter:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val mapKeysShouldNotBeGenericPrinter: IssuePrinter[MapKeysShouldNotBeGeneric] =
    (issue: MapKeysShouldNotBeGeneric) => {
      s"""${extractLocation(issue.meta)}
         |DTO: ${issue.dto.id.toString}
         |Has map fields with generic key:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val anyUnderlyingNotUserTypePrinter: IssuePrinter[AnyUnderlyingNotUserType] =
    (issue: AnyUnderlyingNotUserType) => {
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |`any[T]` requires T to be a user-defined DTO/ADT/Enum.
         |Primitive types, collections, and foreign types are not supported as `any` payloads yet.
         |Offending fields:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val anyUnderlyingLacksUebaDerivationPrinter: IssuePrinter[AnyUnderlyingLacksUebaDerivation] =
    (issue: AnyUnderlyingLacksUebaDerivation) => {
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Has `any[T]` fields whose T lacks `: derived[ueba]`:${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val anyAsMapKeyPrinter: IssuePrinter[AnyAsMapKey] =
    (issue: AnyAsMapKey) => {
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Has map fields with `any` key (disallowed: `any` has no stable hash/equality):${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val anyAsSetElementPrinter: IssuePrinter[AnyAsSetElement] =
    (issue: AnyAsSetElement) => {
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Has set fields with `any` element (disallowed: `any` has no stable hash/equality):${issue.badFields.niceList()}
         |""".stripMargin
    }

  implicit val incorrectRootFoundPrinter: IssuePrinter[IncorrectRootFound] =
    (issue: IncorrectRootFound) => {
      s"""Incorrect root found in model: ${issue.domain.id.toString} ${issue.badRoots
          .map(_.name.name)
          .niceList()}
         |""".stripMargin
    }

  implicit val badFieldNamesPrinter: IssuePrinter[BadFieldNames] = issue => {
    s"""Bad field names: ${issue.bad.niceList()}""".stripMargin
  }

  implicit val eitherAllOrNoneEnumMembersMustHaveConstantsPrinter: IssuePrinter[EitherAllOrNoneEnumMembersMustHaveConstants] = issue => {
    s"""Either all or none of enum members must have associated constants in ${issue.e.id.toString}""".stripMargin
  }

  implicit val underscoredDefinitionRetainedPrinter: IssuePrinter[UnderscoredDefinitionRetained] = issue => {
    s"""Underscored definitions were retained in ${issue.s.id.toString}""".stripMargin
  }

  implicit val wrongEnumConstantPrinter: IssuePrinter[WrongEnumConstant] = issue => {
    s"""Enum constants must be within Int.MinValue..Int.MaxValue range ${issue.e.id.toString}""".stripMargin
  }

  implicit val identifierFieldFloatTypePrinter: IssuePrinter[IdentifierFieldFloatType] =
    (issue: IdentifierFieldFloatType) => {
      val badDesc = issue.badFields.map(f => s"'${f.name.name}' (type: ${f.tpe})").mkString(", ")
      s"""${extractLocation(issue.meta)}
         |identifier '${issue.owner.id.name.name}': field $badDesc has float type; floats are not allowed in identifiers
         |""".stripMargin
    }

  implicit val identifierFieldCollectionPrinter: IssuePrinter[IdentifierFieldCollection] =
    (issue: IdentifierFieldCollection) => {
      val badDesc = issue.badFields.map(f => s"'${f.name.name}'").mkString(", ")
      s"""${extractLocation(issue.meta)}
         |identifier '${issue.owner.id.name.name}': field $badDesc is a collection; identifiers may only contain primitive or nested-identifier scalar fields
         |""".stripMargin
    }

  implicit val identifierFieldUserNotIdentifierPrinter: IssuePrinter[IdentifierFieldUserNotIdentifier] =
    (issue: IdentifierFieldUserNotIdentifier) => {
      val badDesc = issue.badFields.map { case (f, t) => s"'${f.name.name}' references user type '${t.name.name}', which is not an `id`" }.mkString("; ")
      s"""${extractLocation(issue.meta)}
         |identifier '${issue.owner.id.name.name}': $badDesc
         |""".stripMargin
    }

  implicit val identifierFieldAnyPrinter: IssuePrinter[IdentifierFieldAny] =
    (issue: IdentifierFieldAny) => {
      val badDesc = issue.badFields.map(f => s"'${f.name.name}'").mkString(", ")
      s"""${extractLocation(issue.meta)}
         |identifier '${issue.owner.id.name.name}': field $badDesc has type 'any'; 'any' fields are not allowed in identifiers
         |""".stripMargin
    }

  implicit val ineligibleUserMapKeyPrinter: IssuePrinter[IneligibleUserMapKey] =
    (issue: IneligibleUserMapKey) => {
      val reasonText = issue.reason match {
        case IneligibleMapKeyReason.MultiFieldNonIdWrapper(t) =>
          s"'${t.name.name}' is a multi-field DTO (only single-primitive-field wrappers, `id` types, enums, and foreign types are allowed)"
        case IneligibleMapKeyReason.CollectionField(t) =>
          s"'${t.name.name}' wraps a collection field (lst/set/map are not allowed in map keys)"
        case IneligibleMapKeyReason.OptionField(t) =>
          s"'${t.name.name}' wraps an `opt[_]` field (no stable string form for the absent case)"
        case IneligibleMapKeyReason.WrapperWithContracts(t) =>
          s"'${t.name.name}' is a DTO with contracts (only standalone single-primitive-field wrappers are allowed)"
        case IneligibleMapKeyReason.IneligibleUserType(t) =>
          s"'${t.name.name}' is not a valid map-key user type (must be a wrapper DTO, identifier, enum, or foreign type)"
        case IneligibleMapKeyReason.CyclicWrapper(t) =>
          s"'${t.name.name}' participates in a wrapper cycle"
        case IneligibleMapKeyReason.FloatWrapper(t) =>
          s"'${t.name.name}' grounds in a float type (f32/f64/f128 are rejected for wrappers; floats are still permitted as direct builtin keys)"
      }
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Map key field '${issue.badField.name.name}' is ineligible: $reasonText
         |""".stripMargin
    }

  implicit val mapKeyMissingDerivationPrinter: IssuePrinter[MapKeyMissingDerivation] =
    (issue: MapKeyMissingDerivation) => {
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Map key field '${issue.badField.name.name}' uses key type '${issue.keyType.name.name}' which is missing `: derived[${issue.missing}]`.
         |The owning type derives '${issue.missing}', so the key type must too.
         |""".stripMargin
    }

  implicit val dataTypeExpectedFieldPrinter: IssuePrinter[DataTypeExpectedField] =
    (issue: DataTypeExpectedField) => {
      val fieldText = issue.badFields.map { case (f, t) => s"'${f.name.name}' → '${t.name.name}'" }
        .mkString(", ")
      s"""${extractLocation(issue.meta)}
         |In: ${issue.owner.id.toString}
         |Field(s) reference a contract/service where a data type is expected: $fieldText
         |Contracts and services are not data types — they cannot be used as a field value type. Use a `data`, `adt`, `enum`, or `foreign` type, or inherit the contract via `is`.
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
}

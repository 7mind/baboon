package io.septimalmind.baboon.parser.model.issues

import io.circe.Json
import io.septimalmind.baboon.typer.model.*

sealed trait RuntimeCodecIssue extends IssueGroup

object RuntimeCodecIssue {
  implicit def wrap(issue: RuntimeCodecIssue): BaboonIssue = BaboonIssue.RuntimeCodec(issue)

  case class ExpectedJsonObject(typeId: String, actual: Json) extends RuntimeCodecIssue
  case class ExpectedJsonString(typeId: String, actual: Json) extends RuntimeCodecIssue
  case class ExpectedJsonArray(typeId: String, actual: Json) extends RuntimeCodecIssue
  case class ExpectedJsonBoolean(actual: Json) extends RuntimeCodecIssue
  case class ExpectedJsonNumber(expectedType: String, actual: Json) extends RuntimeCodecIssue

  case class UnknownEnumValue(enumId: TypeId, value: String, validValues: List[String]) extends RuntimeCodecIssue
  case class InvalidEnumIndex(enumId: TypeId, index: Int, maxIndex: Int) extends RuntimeCodecIssue

  case class InvalidAdtStructure(adtId: TypeId, branches: List[String]) extends RuntimeCodecIssue
  case class UnknownAdtBranch(adtId: TypeId, branchName: String, validBranches: List[String]) extends RuntimeCodecIssue
  case class InvalidAdtDiscriminator(adtId: TypeId, discriminator: Int, maxDiscriminator: Int) extends RuntimeCodecIssue

  case class UnsupportedBuiltinScalar(scalarType: TypeId.BuiltinScalar) extends RuntimeCodecIssue
  case class UnsupportedCollectionType(collectionType: TypeId.BuiltinCollection) extends RuntimeCodecIssue
  case class UnsupportedMapKeyType(keyType: TypeRef) extends RuntimeCodecIssue

  case class CannotEncodeType(typeId: TypeId, reason: String) extends RuntimeCodecIssue
  case class CannotDecodeType(typeId: TypeId, reason: String) extends RuntimeCodecIssue

  case class InvalidUuidString(value: String) extends RuntimeCodecIssue
  case class InvalidTimestampString(value: String) extends RuntimeCodecIssue

  case class UnexpectedDomainMemberType(typeId: TypeId, reason: String) extends RuntimeCodecIssue

  // Printers
  implicit val expectedJsonObjectPrinter: IssuePrinter[ExpectedJsonObject] =
    (issue: ExpectedJsonObject) => s"Expected JSON object for ${issue.typeId}, got: ${issue.actual}"

  implicit val expectedJsonStringPrinter: IssuePrinter[ExpectedJsonString] =
    (issue: ExpectedJsonString) => s"Expected JSON string for ${issue.typeId}, got: ${issue.actual}"

  implicit val expectedJsonArrayPrinter: IssuePrinter[ExpectedJsonArray] =
    (issue: ExpectedJsonArray) => s"Expected JSON array for ${issue.typeId}, got: ${issue.actual}"

  implicit val expectedJsonBooleanPrinter: IssuePrinter[ExpectedJsonBoolean] =
    (issue: ExpectedJsonBoolean) => s"Expected JSON boolean, got: ${issue.actual}"

  implicit val expectedJsonNumberPrinter: IssuePrinter[ExpectedJsonNumber] =
    (issue: ExpectedJsonNumber) => s"Expected ${issue.expectedType}, got: ${issue.actual}"

  implicit val unknownEnumValuePrinter: IssuePrinter[UnknownEnumValue] =
    (issue: UnknownEnumValue) =>
      s"Unknown enum value '${issue.value}' for ${issue.enumId}. Valid values: ${issue.validValues.mkString(", ")}"

  implicit val invalidEnumIndexPrinter: IssuePrinter[InvalidEnumIndex] =
    (issue: InvalidEnumIndex) =>
      s"Invalid enum index ${issue.index} for ${issue.enumId}, max is ${issue.maxIndex}"

  implicit val invalidAdtStructurePrinter: IssuePrinter[InvalidAdtStructure] =
    (issue: InvalidAdtStructure) =>
      s"Expected exactly one branch for ADT ${issue.adtId}, got: ${issue.branches.mkString(", ")}"

  implicit val unknownAdtBranchPrinter: IssuePrinter[UnknownAdtBranch] =
    (issue: UnknownAdtBranch) =>
      s"Unknown ADT branch '${issue.branchName}' for ${issue.adtId}. Valid branches: ${issue.validBranches.mkString(", ")}"

  implicit val invalidAdtDiscriminatorPrinter: IssuePrinter[InvalidAdtDiscriminator] =
    (issue: InvalidAdtDiscriminator) =>
      s"Invalid ADT discriminator ${issue.discriminator} for ${issue.adtId}, max is ${issue.maxDiscriminator}"

  implicit val unsupportedBuiltinScalarPrinter: IssuePrinter[UnsupportedBuiltinScalar] =
    (issue: UnsupportedBuiltinScalar) => s"Unsupported builtin scalar: ${issue.scalarType}"

  implicit val unsupportedCollectionTypePrinter: IssuePrinter[UnsupportedCollectionType] =
    (issue: UnsupportedCollectionType) => s"Unsupported collection type: ${issue.collectionType}"

  implicit val unsupportedMapKeyTypePrinter: IssuePrinter[UnsupportedMapKeyType] =
    (issue: UnsupportedMapKeyType) => s"Unsupported map key type: ${issue.keyType}"

  implicit val cannotEncodeTypePrinter: IssuePrinter[CannotEncodeType] =
    (issue: CannotEncodeType) => s"Cannot encode ${issue.typeId}: ${issue.reason}"

  implicit val cannotDecodeTypePrinter: IssuePrinter[CannotDecodeType] =
    (issue: CannotDecodeType) => s"Cannot decode ${issue.typeId}: ${issue.reason}"

  implicit val invalidUuidStringPrinter: IssuePrinter[InvalidUuidString] =
    (issue: InvalidUuidString) => s"Invalid UUID string: ${issue.value}"

  implicit val invalidTimestampStringPrinter: IssuePrinter[InvalidTimestampString] =
    (issue: InvalidTimestampString) => s"Invalid timestamp string: ${issue.value}"

  implicit val unexpectedDomainMemberTypePrinter: IssuePrinter[UnexpectedDomainMemberType] =
    (issue: UnexpectedDomainMemberType) => s"Unexpected domain member type ${issue.typeId}: ${issue.reason}"
}

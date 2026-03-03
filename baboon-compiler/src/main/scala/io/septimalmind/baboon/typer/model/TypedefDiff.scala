package io.septimalmind.baboon.typer.model

sealed trait TypedefDiff {
  def ops: List[AbstractOp]
}

object TypedefDiff {
  case class EnumDiff(ops: List[EnumOp]) extends TypedefDiff
  case class DtoDiff(ops: List[DtoOp]) extends TypedefDiff
  case class AdtDiff(ops: List[AdtOp]) extends TypedefDiff
  case class ServiceDiff(ops: List[ServiceOp]) extends TypedefDiff
  case class ContractDiff(ops: List[ContractOp]) extends TypedefDiff
}

sealed trait AbstractOp
sealed trait EnumOp extends AbstractOp
object EnumOp {
  case class AddBranch(m: EnumMember) extends EnumOp
  case class RemoveBranch(m: EnumMember) extends EnumOp
  case class KeepBranch(m: EnumMember) extends EnumOp
}

sealed trait DtoOp extends AbstractOp
object DtoOp {
  case class AddField(f: Field) extends DtoOp
  case class RemoveField(f: Field) extends DtoOp
  case class ChangeField(f: Field, newType: TypeRef) extends DtoOp
  case class KeepField(f: Field, modification: RefModification) extends DtoOp
  case class RenameField(oldField: Field, newField: Field, modification: RefModification) extends DtoOp
}

sealed trait RefModification
object RefModification {
  case object Unchanged extends RefModification
  case object Shallow extends RefModification
  case object Deep extends RefModification
  case object Full extends RefModification
}

sealed trait AdtOp extends AbstractOp
object AdtOp {
  case class AddBranch(id: TypeId.User) extends AdtOp
  case class RemoveBranch(id: TypeId.User) extends AdtOp
  case class KeepBranch(id: TypeId.User, modification: RefModification) extends AdtOp
}

sealed trait ServiceOp extends AbstractOp
object ServiceOp {
  case class AddMethod(m: Typedef.MethodDef) extends ServiceOp
  case class RemoveMethod(m: Typedef.MethodDef) extends ServiceOp
  case class KeepMethod(m: Typedef.MethodDef) extends ServiceOp
}

sealed trait ContractOp extends AbstractOp
object ContractOp {
  case class AddField(f: Field) extends ContractOp
  case class RemoveField(f: Field) extends ContractOp
  case class KeepField(f: Field) extends ContractOp
  case class AddContract(id: TypeId.User) extends ContractOp
  case class RemoveContract(id: TypeId.User) extends ContractOp
  case class KeepContract(id: TypeId.User) extends ContractOp
}

package io.septimalmind.baboon.typer.model

import izumi.fundamentals.platform.strings.IzString.*

case class BaboonEvolution(pkg: Pkg,
                           latest: Version,
                           diffs: Map[Version, BaboonDiff]) {
  override def toString: String = {
    diffs
      .map {
        case (v, diff) =>
          s"""$v => $latest:
             |${diff.changes.toString.shift(2)}
             |Modifications: ${diff.diffs
               .map({ case (id, d) => s"$id = ${d.ops.niceList().shift(2)}" })
               .niceList()
               .shift(2)}""".stripMargin
      }
      .niceList()
  }
}

case class BaboonDiff(changes: BaboonChanges, diffs: Map[TypeId, TypedefDiff])

case class BaboonChanges(added: Set[TypeId],
                         removed: Set[TypeId],
                         unmodified: Set[TypeId],
                         shallowModified: Set[TypeId],
                         deepModified: Set[TypeId],
                         fullyModified: Set[TypeId],
) {
  def changed: Set[TypeId] = shallowModified ++ deepModified ++ fullyModified

  override def toString: String =
    List(
      s"Added: ${added.niceList()}",
      s"Removed: ${removed.niceList()}",
      s"Unmodified: ${unmodified.niceList()}",
      s"Modified (shallow): ${shallowModified.niceList()}",
      s"Modified (deep): ${deepModified.niceList()}",
      s"Modified (full): ${fullyModified.niceList()}",
    ).mkString("\n")
}

sealed trait TypedefDiff {
  def ops: List[AbstractOp]
}

object TypedefDiff {
  case class EnumDiff(ops: List[EnumOp]) extends TypedefDiff
  case class DtoDiff(ops: List[DtoOp]) extends TypedefDiff
  case class AdtDiff(ops: List[AdtOp]) extends TypedefDiff
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
  case class KeepField(f: Field, modification: FieldModification) extends DtoOp
}

sealed trait FieldModification
object FieldModification {
  case object Unchanged extends FieldModification
  case object Shallow extends FieldModification
  case object Deep extends FieldModification
  case object Full extends FieldModification
}

sealed trait AdtOp extends AbstractOp

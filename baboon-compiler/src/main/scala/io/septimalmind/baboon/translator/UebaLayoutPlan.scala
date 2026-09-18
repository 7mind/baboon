package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.*

final class UebaLayoutPlan(domain: Domain) {
  import UebaLayoutPlan.FieldLayout

  def fields(dto: Typedef.Dto): List[FieldLayout] =
    dto.fields.map(field => FieldLayout(field, domain.refMeta(field.tpe).len))

  def indexedFields(dto: Typedef.Dto): List[Field] =
    fields(dto).collect { case FieldLayout(field, _: BinReprLen.Variable) => field }

  def adtBranchIndex(adtId: TypeId.User, branchId: TypeId.User): Int = {
    val adt = domain.defs.meta.nodes(adtId) match {
      case DomainMember.User(_, value: Typedef.Adt, _, _) => value
      case other                                          => throw new IllegalStateException(s"BUG: expected ADT $adtId while planning UEBA branch $branchId, got $other")
    }
    adt.dataMembers(domain).zipWithIndex.find(_._1 == branchId).get._2
  }
}

object UebaLayoutPlan {
  val MaxIndexValue: Int = Int.MaxValue
  final case class FieldLayout(field: Field, length: BinReprLen)

  sealed trait LengthCheck
  object LengthCheck {
    final case class EqualTo(bytes: Int) extends LengthCheck
    final case class AtLeast(bytes: Int) extends LengthCheck
    final case class AtMost(bytes: Int) extends LengthCheck
    final case class OneOf(bytes: List[Int]) extends LengthCheck
  }

  def lengthChecks(length: BinReprLen): List[LengthCheck] = {
    import LengthCheck.*
    length match {
      case BinReprLen.Fixed(bytes)        => List(EqualTo(bytes))
      case BinReprLen.Unknown()           => List(AtLeast(1), AtMost(MaxIndexValue))
      case BinReprLen.Alternatives(bytes) => List(AtLeast(1), OneOf(bytes.toList))
      case BinReprLen.Range(min, max)     => List(AtLeast(math.max(1, min)), AtMost(max.getOrElse(MaxIndexValue)))
    }
  }
}

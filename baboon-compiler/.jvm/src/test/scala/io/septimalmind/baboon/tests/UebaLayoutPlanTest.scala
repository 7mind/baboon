package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model.{InputPointer, RawNodeMeta}
import io.septimalmind.baboon.translator.UebaLayoutPlan
import io.septimalmind.baboon.translator.UebaLayoutPlan.FieldLayout
import io.septimalmind.baboon.typer.model.*
import izumi.fundamentals.collections.nonempty.{NEList, NESet}
import izumi.fundamentals.graphs.{DG, GraphMeta}
import izumi.fundamentals.graphs.struct.AdjacencyPredList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UebaLayoutPlanTest extends AnyFlatSpec with Matchers {
  private val pkg                           = Pkg(NEList("layout"))
  private def id(name: String): TypeId.User = TypeId.User(pkg, Owner.Toplevel, TypeName(name))
  private def field(name: String): Field    = Field(FieldName(name), TypeRef.Scalar(id(name)), None)

  private def domain(definitions: List[Typedef.User], lengths: Map[TypeRef, BinReprLen]): Domain = {
    val members: Map[TypeId, DomainMember] = definitions.map {
      definition =>
        definition.id -> DomainMember.User(false, definition, Set.empty, RawNodeMeta(InputPointer.Undefined))
    }.toMap
    Domain(
      pkg,
      Version.parse("1.0.0"),
      DG.fromPred(AdjacencyPredList(members.keys.map(_ -> Set.empty[TypeId]).toMap), GraphMeta(members)),
      Set.empty,
      Map.empty,
      Set.empty,
      lengths.map { case (ref, length) => ref -> RefMeta(length) },
      Map.empty,
      Set.empty,
      Map.empty,
      Map.empty,
      Nil,
    )
  }

  "UEBA layout planning" should "preserve field order and every length constraint" in {
    val expected = List(
      FieldLayout(field("zFixed"), BinReprLen.Fixed(4)),
      FieldLayout(field("unknown"), BinReprLen.Unknown()),
      FieldLayout(field("alternatives"), BinReprLen.Alternatives(NESet(1, 5, 9))),
      FieldLayout(field("bounded"), BinReprLen.Range(1, Some(17))),
      FieldLayout(field("unbounded"), BinReprLen.Range(1, None)),
      FieldLayout(field("aFixed"), BinReprLen.Fixed(0)),
    )
    val dto  = Typedef.Dto(id("Record"), expected.map(_.field), Nil)
    val plan = new UebaLayoutPlan(domain(List(dto), expected.map(f => f.field.tpe -> f.length).toMap))
    plan.fields(dto) shouldBe expected
    plan.indexedFields(dto) shouldBe expected.slice(1, 5).map(_.field)
  }

  it should "produce no index entries for empty or fixed-only records" in {
    val empty = Typedef.Dto(id("Empty"), Nil, Nil)
    val fixed = Typedef.Dto(id("Fixed"), List(field("value")), Nil)
    val plan  = new UebaLayoutPlan(domain(List(empty, fixed), Map(fixed.fields.head.tpe -> BinReprLen.Fixed(4))))
    plan.fields(empty) shouldBe Nil
    plan.indexedFields(empty) shouldBe Nil
    plan.indexedFields(fixed) shouldBe Nil
  }

  it should "prepare the same strict length checks for every backend" in {
    import UebaLayoutPlan.LengthCheck.*
    UebaLayoutPlan.lengthChecks(BinReprLen.Fixed(0)) shouldBe List(EqualTo(0))
    UebaLayoutPlan.lengthChecks(BinReprLen.Fixed(4)) shouldBe List(EqualTo(4))
    UebaLayoutPlan.lengthChecks(BinReprLen.Unknown()) shouldBe List(AtLeast(1), AtMost(Int.MaxValue))
    UebaLayoutPlan.lengthChecks(BinReprLen.Range(4, None)) shouldBe List(AtLeast(4), AtMost(Int.MaxValue))
    UebaLayoutPlan.lengthChecks(BinReprLen.Range(1, Some(17))) shouldBe List(AtLeast(1), AtMost(17))
    UebaLayoutPlan.lengthChecks(BinReprLen.Range(0, Some(17))) shouldBe List(AtLeast(1), AtMost(17))
    val alternatives = NESet(1, 5, 9)
    UebaLayoutPlan.lengthChecks(BinReprLen.Alternatives(alternatives)) shouldBe List(AtLeast(1), OneOf(alternatives.toList))
  }

  it should "fail when required field metadata is missing" in {
    val dto = Typedef.Dto(id("Missing"), List(field("value")), Nil)
    intercept[NoSuchElementException](new UebaLayoutPlan(domain(List(dto), Map.empty)).fields(dto))
  }

  it should "retain declaration ordinals while excluding contracts and services" in {
    val parent   = id("Choice")
    val first    = Typedef.Dto(TypeId.User(pkg, Owner.Adt(parent), TypeName("Z")), Nil, Nil)
    val last     = Typedef.Dto(TypeId.User(pkg, Owner.Adt(parent), TypeName("A")), Nil, Nil)
    val contract = Typedef.Contract(id("Contract"), Nil, Nil)
    val service  = Typedef.Service(id("Service"), Nil)
    val adt      = Typedef.Adt(parent, NEList(contract.id, first.id, service.id, last.id), Nil, Nil)
    val plan     = new UebaLayoutPlan(domain(List(adt, first, last, contract, service), Map.empty))
    plan.adtBranchIndex(parent, first.id) shouldBe 0
    plan.adtBranchIndex(parent, last.id) shouldBe 1
    intercept[NoSuchElementException](plan.adtBranchIndex(parent, contract.id))
    intercept[NoSuchElementException](plan.adtBranchIndex(parent, id("Absent")))
  }

  it should "reject a branch owner that is not an ADT" in {
    val dto = Typedef.Dto(id("Record"), Nil, Nil)
    intercept[IllegalStateException](new UebaLayoutPlan(domain(List(dto), Map.empty)).adtBranchIndex(dto.id, id("Branch")))
  }
}

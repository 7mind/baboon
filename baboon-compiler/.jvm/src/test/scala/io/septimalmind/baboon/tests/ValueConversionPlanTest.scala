package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.ValueConversionPlan
import io.septimalmind.baboon.translator.ValueConversionPlan.*
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeName, TypeRef}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ValueConversionPlanTest extends AnyFlatSpec with Matchers {
  private val narrow = TypeRef.Scalar(TypeId.Builtins.i16)
  private val wide   = TypeRef.Scalar(TypeId.Builtins.i64)
  private val pkg    = Pkg(NEList("conversion"))
  private val oldTpe = TypeRef.Scalar(TypeId.User(pkg, Owner.Toplevel, TypeName("Old")))
  private val newTpe = TypeRef.Scalar(TypeId.User(pkg, Owner.Toplevel, TypeName("New")))

  private def unary(id: TypeId.BuiltinCollection, element: TypeRef): TypeRef.Constructor = TypeRef.Constructor(id, NEList(element))
  private def dictionary(key: TypeRef, value: TypeRef): TypeRef.Constructor = TypeRef.Constructor(TypeId.Builtins.map, NEList(key, value))

  "Value conversion planning" should "retain both scalar identifiers without choosing a backend numeric conversion" in {
    ValueConversionPlan(narrow, wide) shouldBe Scalar(narrow, wide)
    ValueConversionPlan(oldTpe, newTpe) shouldBe Scalar(oldTpe, newTpe)
  }

  it should "retain scalar conversion hooks even when the type identifier is unchanged" in {
    ValueConversionPlan(oldTpe, oldTpe) shouldBe Scalar(oldTpe, oldTpe)
    ValueConversionPlan(unary(TypeId.Builtins.lst, oldTpe), unary(TypeId.Builtins.lst, oldTpe)) shouldBe MapList(Scalar(oldTpe, oldTpe))
  }

  it should "plan scalar wrapping independently of the backend collection representation" in {
    ValueConversionPlan(narrow, unary(TypeId.Builtins.opt, wide)) shouldBe WrapOptional(Scalar(narrow, wide))
    ValueConversionPlan(narrow, unary(TypeId.Builtins.lst, wide)) shouldBe WrapList(Scalar(narrow, wide))
    ValueConversionPlan(narrow, unary(TypeId.Builtins.set, wide)) shouldBe WrapSet(Scalar(narrow, wide))
  }

  it should "map optional, list and set elements recursively" in {
    ValueConversionPlan(unary(TypeId.Builtins.opt, narrow), unary(TypeId.Builtins.opt, wide)) shouldBe MapOptional(Scalar(narrow, wide))
    ValueConversionPlan(unary(TypeId.Builtins.set, narrow), unary(TypeId.Builtins.set, wide)) shouldBe MapSet(Scalar(narrow, wide))
    ValueConversionPlan(unary(TypeId.Builtins.lst, narrow), unary(TypeId.Builtins.lst, unary(TypeId.Builtins.opt, wide))) shouldBe
      MapList(WrapOptional(Scalar(narrow, wide)))
  }

  it should "plan map keys and nested values using their separate source and target types" in {
    val source = dictionary(narrow, unary(TypeId.Builtins.lst, unary(TypeId.Builtins.opt, oldTpe)))
    val target = dictionary(wide, unary(TypeId.Builtins.lst, unary(TypeId.Builtins.opt, newTpe)))
    ValueConversionPlan(source, target) shouldBe MapEntries(Scalar(narrow, wide), MapList(MapOptional(Scalar(oldTpe, newTpe))))
  }

  it should "copy opaque values only when their variant and underlying type are unchanged" in {
    for {
      variant    <- List(TypeRef.AnyVariant.Global, TypeRef.AnyVariant.ThisDom, TypeRef.AnyVariant.Current)
      underlying <- List(None, Some(oldTpe))
    } {
      val opaque = TypeRef.Any(variant, underlying)
      ValueConversionPlan(opaque, opaque) shouldBe CopyOpaque
      ValueConversionPlan(unary(TypeId.Builtins.lst, opaque), unary(TypeId.Builtins.lst, opaque)) shouldBe MapList(CopyOpaque)
    }
  }

  it should "reject opaque variant, underlying and nonopaque transitions" in {
    val opaque = TypeRef.Any(TypeRef.AnyVariant.Global, Some(oldTpe))
    for ((source, target) <- List(
        (opaque, opaque.copy(variant = TypeRef.AnyVariant.Current)),
        (opaque, opaque.copy(underlying = Some(newTpe))),
        (opaque, narrow),
        (narrow, opaque),
        (opaque, unary(TypeId.Builtins.opt, opaque)),
      )) {
      intercept[IllegalStateException](ValueConversionPlan(source, target))
    }
  }

  it should "retain invariant failures for unsupported constructor transitions" in {
    intercept[IllegalStateException](ValueConversionPlan(unary(TypeId.Builtins.lst, narrow), narrow))
    intercept[IllegalStateException](ValueConversionPlan(narrow, dictionary(narrow, wide)))
    val unknown = unary(TypeId.BuiltinCollection(TypeName("unknown")), narrow)
    intercept[IllegalStateException](ValueConversionPlan(narrow, unknown))
    intercept[IllegalStateException](ValueConversionPlan(unary(TypeId.Builtins.lst, narrow), unknown))
  }
}

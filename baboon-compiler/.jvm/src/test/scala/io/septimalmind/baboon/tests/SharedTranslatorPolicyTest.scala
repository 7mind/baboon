package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.IdentifierFieldKind
import io.septimalmind.baboon.translator.kotlin.KtTreeTools
import io.septimalmind.baboon.translator.scl.ScTreeTools
import io.septimalmind.baboon.translator.typescript.TsTreeTools
import io.septimalmind.baboon.typer.model.{DocComment, Docs, Owner, Pkg, TypeId, TypeName, TypeRef}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SharedTranslatorPolicyTest extends AnyFlatSpec with Matchers {
  "Identifier classification" should "retain every supported scalar category" in {
    import TypeId.Builtins.*
    val cases = List(
      bit   -> IdentifierFieldKind.Bit,
      i08   -> IdentifierFieldKind.SignedInt,
      i16   -> IdentifierFieldKind.SignedInt,
      i32   -> IdentifierFieldKind.SignedInt,
      i64   -> IdentifierFieldKind.SignedLong,
      u08   -> IdentifierFieldKind.UnsignedSmallInt,
      u16   -> IdentifierFieldKind.UnsignedSmallInt,
      u32   -> IdentifierFieldKind.UnsignedSmallInt,
      u64   -> IdentifierFieldKind.UnsignedLong,
      str   -> IdentifierFieldKind.Str,
      uid   -> IdentifierFieldKind.Uid,
      tsu   -> IdentifierFieldKind.Tsu,
      tso   -> IdentifierFieldKind.Tso,
      bytes -> IdentifierFieldKind.Bytes,
    )
    cases.foreach { case (scalar, expected) => IdentifierFieldKind.classify(TypeRef.Scalar(scalar)) shouldBe expected }
  }

  it should "preserve nested identifier identity" in {
    val id = TypeId.User(Pkg(NEList("example")), Owner.Toplevel, TypeName("Nested"))
    IdentifierFieldKind.classify(TypeRef.Scalar(id)) shouldBe IdentifierFieldKind.NestedId(id)
  }

  it should "reject unsupported scalars, collections and opaque values at the invariant boundary" in {
    import TypeId.Builtins.*
    List(f32, f64, f128).foreach {
      scalar =>
        val error = intercept[IllegalStateException](IdentifierFieldKind.classify(TypeRef.Scalar(scalar)))
        error.getMessage shouldBe s"Identifier field has unsupported scalar $scalar; validator should have rejected this."
    }
    val invalid = List(
      TypeRef.Constructor(lst, NEList(TypeRef.Scalar(str))),
      TypeRef.Any(TypeRef.AnyVariant.Global, None),
    )
    invalid.foreach {
      ref =>
        val error = intercept[IllegalStateException](IdentifierFieldKind.classify(ref))
        error.getMessage shouldBe s"Identifier field has unsupported TypeRef $ref; validator should have rejected this."
    }
  }

  "Javadoc-style backend renderers" should "preserve exact comments through TextTree rendering" in {
    val renderers: List[(Docs, String) => String] = List(
      new KtTreeTools.KtTreeToolsImpl().renderDocs _,
      new ScTreeTools.ScTreeToolsImpl().renderDocs _,
      new TsTreeTools.TsTreeToolsImpl().renderDocs _,
    )
    def docs(prefix: Option[String], suffix: Option[String]): Docs =
      Docs(prefix.map(s => DocComment(s, s)), suffix.map(s => DocComment(s, s)))

    val cases = List(
      (docs(None, None), ""),
      (docs(Some(""), None), "  /**  */\n"),
      (docs(Some("one"), None), "  /** one */\n"),
      (docs(None, Some("suffix")), "  /** suffix */\n"),
      (docs(Some("one\ntwo"), Some("tail")), "  /**\n   * one\n   * two\n   *\n   * tail\n   */\n"),
      (docs(Some("line\n"), None), "  /**\n   * line\n   *\n   */\n"),
      (docs(Some("λ C:\\Windows */"), None), "  /** λ C:\\Windows */ */\n"),
    )
    renderers.foreach {
      render =>
        cases.foreach {
          case (input, expected) =>
            val rendered                = render(input, "  ")
            val tree: TextTree[Nothing] = q"$rendered"
            tree.dump shouldBe expected
        }
    }
  }
}

package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.{ResolvedServiceResult, ServiceMethodPlan}
import io.septimalmind.baboon.translator.csharp.CSTypes
import io.septimalmind.baboon.translator.java.JvTypeTranslator
import io.septimalmind.baboon.translator.kotlin.KtTypeTranslator
import io.septimalmind.baboon.typer.model.{TypeId, TypeRef, Typedef}
import io.septimalmind.baboon.typer.model.Typedef.MethodName
import izumi.fundamentals.platform.strings.TextTree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ServiceMethodPlanTest extends AnyFlatSpec with Matchers {
  private val input  = TypeRef.Scalar(TypeId.Builtins.str)
  private val output = TypeRef.Scalar(TypeId.Builtins.i32)
  private val error  = TypeRef.Scalar(TypeId.Builtins.bytes)
  private val method = Typedef.MethodDef(MethodName("when"), input, Some(output), Some(error))

  "Kotlin method facts" should "retain keyword escaping and defer type resolution until its output is used" in {
    var resolvedTypes = List.empty[TypeRef]
    val plan = new ServiceMethodPlan(
      method,
      tpe => { resolvedTypes = resolvedTypes :+ tpe; q"Type" },
      ResolvedServiceResult(noErrors = false, Some("Either"), Some("<$error, $success>"), None),
      KtTypeTranslator.escapeKtKeyword(method.name.name),
    )
    plan.methodName shouldBe "`when`"
    plan.hasError shouldBe true
    resolvedTypes shouldBe Nil
    plan.input.dump shouldBe "Type"
    plan.input.dump shouldBe "Type"
    resolvedTypes shouldBe List(input)
    plan.output.map(_.dump) shouldBe Some("Type")
    resolvedTypes shouldBe List(input, output)
    plan.error.map(_.dump) shouldBe Some("Type")
    resolvedTypes shouldBe List(input, output, error)
  }

  "Scala method facts" should "retain raw method spelling and configured error suppression" in {
    var resolvedTypes = List.empty[TypeRef]
    val plan = new ServiceMethodPlan(
      method,
      tpe => { resolvedTypes = resolvedTypes :+ tpe; q"Type" },
      ResolvedServiceResult(noErrors = true, Some("Either"), Some("[$error, $success]"), None),
      method.name.name,
    )
    plan.methodName shouldBe "when"
    plan.hasError shouldBe false
    resolvedTypes shouldBe Nil
    plan.input.dump shouldBe "Type"
    plan.output.map(_.dump) shouldBe Some("Type")
    resolvedTypes shouldBe List(input, output)
  }

  it should "keep absent output and error types unresolved" in {
    val empty  = method.copy(out = None, err = None)
    val result = ResolvedServiceResult(noErrors = false, Some("Either"), Some("[$error, $success]"), None)
    val sc     = new ServiceMethodPlan(empty, _ => throw new AssertionError("unused type resolved"), result, empty.name.name)
    val kt     = new ServiceMethodPlan(empty, _ => throw new AssertionError("unused type resolved"), result, KtTypeTranslator.escapeKtKeyword(empty.name.name))
    sc.output shouldBe None
    sc.error shouldBe None
    sc.hasError shouldBe false
    kt.output shouldBe None
    kt.error shouldBe None
    kt.hasError shouldBe false
  }

  "Java method facts" should "escape declarations and resolve requested types once" in {
    var resolvedTypes = List.empty[TypeRef]
    val plan = new ServiceMethodPlan(
      method.copy(name = MethodName("class")),
      tpe => { resolvedTypes = resolvedTypes :+ tpe; q"Type" },
      ResolvedServiceResult(noErrors = false, Some("Either"), Some("<$error, $success>"), None),
      JvTypeTranslator.escapeJvKeyword("class"),
    )
    plan.methodName shouldBe "class_"
    plan.hasError shouldBe true
    plan.hasOutput shouldBe true
    resolvedTypes shouldBe Nil
    plan.input.dump shouldBe "Type"
    plan.input.dump shouldBe "Type"
    plan.output.map(_.dump) shouldBe Some("Type")
    plan.error.map(_.dump) shouldBe Some("Type")
    resolvedTypes shouldBe List(input, output, error)
  }

  "C# method facts" should "retain PascalCase and resolve each requested type once" in {
    var resolvedTypes = List.empty[TypeRef]
    val plan = new ServiceMethodPlan(
      method,
      tpe => { resolvedTypes = resolvedTypes :+ tpe; q"Type" },
      ResolvedServiceResult(noErrors = false, Some("Either"), Some("<$error, $success>"), None),
      CSTypes.escapeCsKeyword(method.name.name.capitalize),
    )
    plan.methodName shouldBe "When"
    plan.hasError shouldBe true
    resolvedTypes shouldBe Nil
    plan.input.dump shouldBe "Type"
    plan.input.dump shouldBe "Type"
    plan.output.map(_.dump) shouldBe Some("Type")
    plan.error.map(_.dump) shouldBe Some("Type")
    resolvedTypes shouldBe List(input, output, error)
  }
}

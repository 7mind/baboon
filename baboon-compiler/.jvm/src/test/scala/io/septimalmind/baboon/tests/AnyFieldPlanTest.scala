package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.AnyFieldPlan
import io.septimalmind.baboon.typer.model.{DomainMember, TypeRef}
import izumi.functional.bio.Error2
import izumi.reflect.TagKK

final class AnyFieldPlanTest extends AnyFieldPlanTestBase[Either]

abstract class AnyFieldPlanTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "Any field planning" should {
    "retain all six kind bytes and static fallback tuples" in {
      (loader: BaboonLoader[F]) =>
        for {
          family <- loadPkg(loader)
        } yield {
          val domain     = family.domains.toMap.values.head.versions.toMap.values.head
          val user       = domain.defs.meta.nodes.values.collectFirst { case member: DomainMember.User => member.id }.get
          val underlying = Some(TypeRef.Scalar(user))
          val dom        = Some(domain.id.toString)
          val ver        = Some(domain.version.v.toString)
          val tid        = Some(user.toString)
          val cases = List(
            (TypeRef.Any(TypeRef.AnyVariant.Global, None), AnyFieldPlan(0x07.toByte, None, None, None)),
            (TypeRef.Any(TypeRef.AnyVariant.ThisDom, None), AnyFieldPlan(0x03.toByte, dom, None, None)),
            (TypeRef.Any(TypeRef.AnyVariant.Current, None), AnyFieldPlan(0x01.toByte, dom, ver, None)),
            (TypeRef.Any(TypeRef.AnyVariant.Global, underlying), AnyFieldPlan(0x06.toByte, None, None, tid)),
            (TypeRef.Any(TypeRef.AnyVariant.ThisDom, underlying), AnyFieldPlan(0x02.toByte, dom, None, tid)),
            (TypeRef.Any(TypeRef.AnyVariant.Current, underlying), AnyFieldPlan(0x00.toByte, dom, ver, tid)),
          )
          cases.foreach { case (field, expected) => assert(AnyFieldPlan.forField(field, domain) == expected) }
        }
    }
  }
}

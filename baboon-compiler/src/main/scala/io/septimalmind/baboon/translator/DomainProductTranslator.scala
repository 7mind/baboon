package io.septimalmind.baboon.translator

import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.{Domain, DomainMember}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

object DomainProductTranslator {
  def translate[F[+_, +_]: Error2, A](
    domain: Domain,
    products: Set[CompilerProduct],
    product: CompilerProduct,
    translateMember: DomainMember.User => F[NEList[BaboonIssue], List[A]],
  ): F[NEList[BaboonIssue], List[A]] = {
    if (products.contains(product)) {
      F.flatTraverseAccumErrors(domain.defs.meta.nodes.toList) {
        case (_, member: DomainMember.User) => translateMember(member)
        case _                              => F.pure(List.empty[A])
      }
    } else {
      F.pure(List.empty[A])
    }
  }
}

package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, DomainMember, TypeId, TypeRef, Typedef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.nio.file.Path

/** Front-end tests for WHERE a name is looked up from: a reference keeps the meaning it has at the
  * place it is written, whichever place its text ends up being typed at (a template instantiated in
  * another namespace, an alias followed from another namespace), and a qualified reference `a.B`
  * names a member of `a` and nothing else.
  *
  * The assertions render every user type in full (`pkg.owner.Name`), so they pin down WHICH of
  * several same-named types a reference resolved to.
  */
final class LexicalScopeFrontEndTest extends LexicalScopeFrontEndTestBase[Either]

abstract class LexicalScopeFrontEndTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def resolveBaboonFile(resourcePath: String): List[Path] = {
    val file = IzResources
      .getPath(resourcePath)
      .getOrElse(throw new AssertionError(s"resource not found: $resourcePath"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    List(file)
  }

  private def runPipeline(loader: BaboonLoader[F], paths: List[Path]): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loader.load(paths).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  private def domainOf(outcome: Either[NEList[BaboonIssue], BaboonFamily], pkgPath: String): Domain = {
    val family = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
    val lineage = family.domains.toMap
      .find(_._1.path.mkString(".") == pkgPath)
      .getOrElse(throw new AssertionError(s"no lineage for pkg '$pkgPath'"))
      ._2
    lineage.versions.toMap.values.head
  }

  private def renderRef(ref: TypeRef): String = {
    def renderId(id: TypeId): String = id match {
      case u: TypeId.User => u.render
      case other          => other.name.name
    }
    ref match {
      case TypeRef.Scalar(id)            => renderId(id)
      case TypeRef.Constructor(id, args) => s"${renderId(id)}[${args.toList.map(renderRef).mkString(", ")}]"
      case any: TypeRef.Any              => any.toString
    }
  }

  private def dtoOf(domain: Domain, qualifiedName: String): Typedef.Dto = {
    domain.defs.meta.nodes.values.collectFirst {
      case DomainMember.User(_, dto: Typedef.Dto, _, _) if dto.id.render == qualifiedName => dto
    }.getOrElse {
      throw new AssertionError(
        s"no DTO $qualifiedName; DTOs found: ${domain.defs.meta.nodes.values.collect { case DomainMember.User(_, d: Typedef.Dto, _, _) => d.id.render }.toList}"
      )
    }
  }

  /** The fully-qualified type of every field of the DTO `qualifiedName`, keyed by field name. */
  private def fieldTypes(domain: Domain, qualifiedName: String): Map[String, String] = {
    dtoOf(domain, qualifiedName).fields.map(f => f.name.name -> renderRef(f.tpe)).toMap
  }

  "name lookup" should {

    "resolve the names in a template's body where the template is declared (issue #113)" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("scope-lexical-ok/template-body.baboon"))
        } yield {
          val pkg    = "my.ok.scope.tplbody"
          val domain = domainOf(outcome, pkg)

          // Alias instantiation from the top level: everything the body names lives in `inner`.
          val intPage = dtoOf(domain, s"$pkg.IntPage")
          assert(intPage.contracts.map(_.render) == List(s"$pkg.inner.Tagged"))
          assert(
            fieldTypes(domain, s"$pkg.IntPage") ==
              Map("tag" -> "str", "at" -> "i32", "items" -> "lst[i32]", "first" -> s"$pkg.inner.Item")
          )

          // Alias instantiation from a sibling namespace: the ARGUMENT is written in `other` and
          // resolves there, while the body still resolves in `inner`.
          assert(
            fieldTypes(domain, s"$pkg.other.ThingPage") ==
              Map("tag" -> "str", "at" -> "i32", "items" -> s"lst[$pkg.other.Thing]", "first" -> s"$pkg.inner.Item")
          )

          // Structural arm, through a template whose body instantiates another template of its
          // own namespace by an unprefixed name.
          assert(
            fieldTypes(domain, s"$pkg.other.Holder") ==
              Map(
                "tag"   -> "str",
                "at"    -> "i32",
                "items" -> s"lst[$pkg.other.Thing]",
                "first" -> s"$pkg.inner.Item",
                "extra" -> s"$pkg.inner.Item",
              )
          )
        }
    }

    "not let a same-named type at the instantiation site capture a template body's reference (issue #113)" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("scope-lexical-ok/template-capture.baboon"))
        } yield {
          val pkg    = "my.ok.scope.tplcapture"
          val domain = domainOf(outcome, pkg)
          assert(fieldTypes(domain, s"$pkg.IntPage") == Map("items" -> "lst[i32]", "first" -> s"$pkg.inner.Item"))
          assert(fieldTypes(domain, s"$pkg.Holder") == Map("items" -> "lst[i32]", "first" -> s"$pkg.inner.Item"))
        }
    }

    "resolve the target of an alias where the alias is declared" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("scope-lexical-ok/alias-target.baboon"))
        } yield {
          val pkg    = "my.ok.scope.aliastarget"
          val domain = domainOf(outcome, pkg)
          assert(fieldTypes(domain, s"$pkg.Y") == Map("f" -> s"$pkg.a.X", "g" -> s"lst[$pkg.a.X]"))
        }
    }

    "not let a same-named type at the use site capture an alias target" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("scope-lexical-ok/alias-capture.baboon"))
        } yield {
          val pkg    = "my.ok.scope.aliascapture"
          val domain = domainOf(outcome, pkg)
          assert(fieldTypes(domain, s"$pkg.Y") == Map("f" -> s"$pkg.a.X"))
        }
    }

    "not resolve the qualifier of a `was` name through an enclosing scope" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("scope-lexical-ok/was-prefix.baboon"))
        } yield {
          val pkg     = "my.ok.scope.wasprefix"
          val domain  = domainOf(outcome, pkg)
          val renames = domain.renames.map { case (to, from) => to.render -> from.render }
          assert(renames == Map(s"$pkg.deep.New" -> s"$pkg.deep.inner.Old"))
        }
    }

    "reject a qualified reference to a name its qualifier does not declare (issue #114)" in {
      (loader: BaboonLoader[F]) =>
        for {
          outcome <- runPipeline(loader, resolveBaboonFile("baboon-fixtures-bad/scope-bad/qualified-fallback.baboon"))
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val unresolved = issues.toList.collect { case BaboonIssue.Typer(i: TyperIssue.UnexpectedNonBuiltin) => i.name.name }
          assert(unresolved == List("Outside"), s"expected `Outside` to be unresolved, got: $issues")
        }
    }
  }
}

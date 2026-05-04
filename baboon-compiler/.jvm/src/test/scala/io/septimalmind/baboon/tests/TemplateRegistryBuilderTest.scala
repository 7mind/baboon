package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, InputPointer, RawTypeName}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

import scala.reflect.ClassTag

/** PR-29.4 (M29) front-end pipeline tests for `TemplateRegistryBuilder`.
  *
  * Exercises the full path: parse → PR-29.4 template-registry pass → ADT inheritance expander →
  * scope-build → type-check. Verifies:
  *
  *   1. (positive) A domain containing only `data X[T] { f: T }` (no alias) produces a
  *      `Domain.defs.meta.nodes` that does NOT contain `X` as a `DomainMember`. The template is
  *      excised before the typer ever resolves `T`.
  *
  *   2. (negative) `data X[T, T] { f: T }` (duplicate type param) produces
  *      `TyperIssue.DuplicateTypeParam` (matrix #4).
  *
  *   3. (backwards-compat) A non-template domain continues to type-check and its member count
  *      is unaffected by the new pass.
  */
final class TemplateRegistryBuilderTest extends TemplateRegistryBuilderTestBase[Either]

abstract class TemplateRegistryBuilderTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // ─── helpers ─────────────────────────────────────────────────────────────────

  private def makeInput(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "template-registry-test.baboon",
  ): F[Nothing, Either[NEList[BaboonIssue], Domain]] = {
    val input = makeInput(name, content)
    parser.parse(input).flatMap(typer.process).map(Right(_): Either[NEList[BaboonIssue], Domain]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], Domain])
    }
  }

  private def typerIssues(issues: NEList[BaboonIssue]): List[TyperIssue] =
    issues.toList.collect { case BaboonIssue.Typer(ti) => ti }

  private def assertProducesTyperIssue[T <: TyperIssue: ClassTag](
    outcome: Either[NEList[BaboonIssue], Domain]
  ): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val ti     = typerIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      ti.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $ti",
    )
  }

  // ─── test fixtures ────────────────────────────────────────────────────────────

  /** A domain with one template and no instantiation. The template must not appear as a
    * DomainMember; the domain has no user-defined roots and therefore no reachable members.
    */
  private val templateOnlyDomain: String =
    """model template.registry.test
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  f: i32
      |}
      |""".stripMargin

  /** A domain with `data X[T, T]` — duplicate type parameter `T` (matrix #4). */
  private val duplicateTypeParamDomain: String =
    """model template.registry.test.neg
      |
      |version "1.0.0"
      |
      |data X[T, T] {
      |  f: i32
      |}
      |""".stripMargin

  /** A plain non-template domain for backwards-compat check. */
  private val nonTemplateDomain: String =
    """model template.registry.compat
      |
      |version "1.0.0"
      |
      |root data Foo {
      |  x: i32
      |  y: str
      |}
      |""".stripMargin

  /** A domain with a template nested inside a namespace (alongside a non-template anchor so the
    * namespace is not structurally empty after template excision).
    */
  private val nestedNamespaceTemplateDomain: String =
    """model template.registry.ns
      |
      |version "1.0.0"
      |
      |ns foo {
      |  data X[T] {
      |    f: i32
      |  }
      |  root data Anchor {
      |    v: i32
      |  }
      |}
      |""".stripMargin

  /** A domain with a non-adjacent duplicate type param: `data X[T, U, T]`. */
  private val nonAdjacentDuplicateTypeParamDomain: String =
    """model template.registry.test.neg2
      |
      |version "1.0.0"
      |
      |data X[T, U, T] {
      |  f: i32
      |}
      |""".stripMargin

  /** A domain mixing a template and a non-template under the same namespace. */
  private val mixedNamespaceDomain: String =
    """model template.registry.mixed
      |
      |version "1.0.0"
      |
      |ns foo {
      |  data X[T] {
      |    f: i32
      |  }
      |  root data Y {
      |    f: i32
      |  }
      |}
      |""".stripMargin

  /** A domain with `data X[T, T, U, U]` — two distinct duplicate type parameters. */
  private val multiDuplicateTypeParamDomain: String =
    """model template.registry.test.neg3
      |
      |version "1.0.0"
      |
      |data X[T, T, U, U] {
      |  f: i32
      |}
      |""".stripMargin

  /** Two top-level templates with the same name — triggers DuplicateTemplateName (PR-29.4-D04). */
  private val duplicateTemplateNameTopLevel: String =
    """model template.registry.dup
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  f: T
      |}
      |
      |data X[U] {
      |  g: U
      |}
      |""".stripMargin

  /** Two templates with the same name inside the same `ns foo { … }` block. */
  private val duplicateTemplateNameInNs: String =
    """model template.registry.dup.ns
      |
      |version "1.0.0"
      |
      |ns foo {
      |  data X[T] {
      |    f: T
      |  }
      |  data X[U] {
      |    g: U
      |  }
      |  root data Anchor {
      |    v: i32
      |  }
      |}
      |""".stripMargin

  /** A top-level single-template domain used for registry contents inspection. */
  private val singleTemplateDomain: String =
    """model template.registry.test
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  f: i32
      |}
      |""".stripMargin

  /** Two templates named X in *different* namespaces — must NOT trigger DuplicateTemplateName.
    * The template registry uses (pkg, owner, name) as key, so cross-namespace entries are distinct.
    * Each namespace has a non-template anchor so neither namespace becomes empty after template excision
    * (an empty namespace after excision triggers ScopeCannotBeEmpty, not DuplicateTemplateName).
    * The root data Anchor in ns b gives the domain at least one root for type-checking.
    */
  private val crossNamespacesSameNameTemplateDomain: String =
    """model template.registry.crossns
      |
      |version "1.0.0"
      |
      |ns a { data X[T] { f: T } data AnchorA { x: i32 } }
      |ns b { data X[U] { g: U } root data Anchor { x: i32 } }
      |""".stripMargin

  /** Three top-level templates with the same name — the implementation reports one
    * DuplicateTemplateName per duplicate group (not one per extra duplicate after the first).
    * The second `data X` is marked for easy line counting.
    */
  private val triplicateTemplateNameTopLevel: String =
    """model template.registry.dup3
      |
      |version "1.0.0"
      |
      |data X[T] { f: T }
      |data X[U] { g: U }
      |data X[V] { h: V }
      |""".stripMargin

  /** Two templates with the same name inside a two-level nested namespace `ns a { ns b { … } }`.
    * Verifies that multi-segment paths are formatted as "a.b".
    * An anchor is included so the domain has at least one root.
    */
  private val duplicateTemplateNameNestedNs: String =
    """model template.registry.dup.nested.ns
      |
      |version "1.0.0"
      |
      |ns a { ns b { data X[T] { f: T } data X[U] { g: U } root data Anchor { x: i32 } } }
      |""".stripMargin

  /** A template X[T] and a non-template `data X { f: i32 }` coexist.
    * The template is excised by TemplateRegistryBuilder; the non-template goes to ScopeBuilder.
    * Whatever ScopeBuilder does about the name collision is separate — this fixture asserts
    * only that no DuplicateTemplateName issue fires (the two code paths are independent).
    */
  private val templateAndNonTemplateSameNameDomain: String =
    """model template.registry.mixed.nontemplate
      |
      |version "1.0.0"
      |
      |data X[T] { f: T }
      |root data X { f: i32 }
      |""".stripMargin

  // ─── tests ────────────────────────────────────────────────────────────────────

  "TemplateRegistryBuilder (PR-29.4)" should {

    "excise template X[T] from Domain.defs.meta.nodes (positive: no instantiation)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, templateOnlyDomain)
        } yield {
          val domain        = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val userTypeNames = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u.name.name }.toSet
          assert(
            !userTypeNames.contains("X"),
            s"template X must not appear in Domain.defs.meta.nodes, but found user types: $userTypeNames",
          )
        }
    }

    "produce DuplicateTypeParam for data X[T, T] (negative: matrix #4)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, duplicateTypeParamDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.DuplicateTypeParam](outcome)
        }
    }

    "DuplicateTypeParam carries the duplicate name and owner name" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, duplicateTypeParamDomain)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssue = typerIssues(issues).collectFirst { case d: TyperIssue.DuplicateTypeParam => d }
            .getOrElse(throw new AssertionError(s"no DuplicateTypeParam in: $issues"))
          assert(dupIssue.name == "T", s"expected duplicate param name 'T', got '${dupIssue.name}'")
          assert(dupIssue.ownerName == "X", s"expected owner name 'X', got '${dupIssue.ownerName}'")
        }
    }

    "leave a non-template domain unaffected (backwards-compat)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nonTemplateDomain)
        } yield {
          val domain        = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val userTypeNames = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u.name.name }.toSet
          assert(
            userTypeNames.contains("Foo"),
            s"expected Foo in domain.defs.meta.nodes, got: $userTypeNames",
          )
        }
    }

    "excise nested-namespace template X[T] from Domain.defs.meta.nodes (namespace foo)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nestedNamespaceTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val userTypeNames = domain.defs.meta.nodes.keys.collect {
            case u: TypeId.User if u.owner == Owner.Ns(Seq(TypeName("foo"))) => u.name.name
          }.toSet
          assert(
            !userTypeNames.contains("X"),
            s"nested template X under namespace foo must not appear in Domain.defs.meta.nodes, but found: $userTypeNames",
          )
        }
    }

    "produce DuplicateTypeParam for non-adjacent duplicate data X[T, U, T]" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nonAdjacentDuplicateTypeParamDomain)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssue = typerIssues(issues).collectFirst { case d: TyperIssue.DuplicateTypeParam => d }
            .getOrElse(throw new AssertionError(s"no DuplicateTypeParam in: $issues"))
          assert(dupIssue.name == "T", s"expected duplicate param name 'T', got '${dupIssue.name}'")
          assert(dupIssue.ownerName == "X", s"expected owner name 'X', got '${dupIssue.ownerName}'")
        }
    }

    "produce two DuplicateTypeParam issues for data X[T, T, U, U] (all duplicates reported at once)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, multiDuplicateTypeParamDomain)
        } yield {
          val issues    = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssues = typerIssues(issues).collect { case d: TyperIssue.DuplicateTypeParam => d }
          assert(
            dupIssues.size == 2,
            s"expected 2 DuplicateTypeParam issues (T and U), got ${dupIssues.size}: ${dupIssues.map(_.name)}",
          )
          val names = dupIssues.map(_.name).toSet
          assert(names == Set("T", "U"), s"expected duplicate names {T, U}, got $names")
          dupIssues.foreach {
            d =>
              assert(d.ownerName == "X", s"expected owner name 'X', got '${d.ownerName}'")
          }
        }
    }

    "excise only template X[T] from mixed namespace, preserving non-template Y" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mixedNamespaceDomain)
        } yield {
          val domain  = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val nsOwner = Owner.Ns(Seq(TypeName("foo")))
          val nsUserNames = domain.defs.meta.nodes.keys.collect {
            case u: TypeId.User if u.owner == nsOwner => u.name.name
          }.toSet
          assert(
            !nsUserNames.contains("X"),
            s"template X under namespace foo must not appear in Domain.defs.meta.nodes, but found: $nsUserNames",
          )
          assert(
            nsUserNames.contains("Y"),
            s"non-template Y under namespace foo must appear in Domain.defs.meta.nodes, but found: $nsUserNames",
          )
        }
    }

    "produce DuplicateTemplateName for two sibling top-level templates with the same name (PR-29.4-D04)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, duplicateTemplateNameTopLevel)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssue = typerIssues(issues).collectFirst { case d: TyperIssue.DuplicateTemplateName => d }
            .getOrElse(throw new AssertionError(s"no DuplicateTemplateName in: ${typerIssues(issues)}"))
          assert(dupIssue.name == "X", s"expected name 'X', got '${dupIssue.name}'")
          assert(dupIssue.ownerName == "<toplevel>", s"expected ownerName '<toplevel>', got '${dupIssue.ownerName}'")
          // The second `data X[U]` in duplicateTemplateNameTopLevel is on line 9 (1-based).
          // Pin the source position to catch regressions where the meta targets the wrong definition.
          // InputPointer.ComparisonHack overrides equals to class-identity only, so we pattern-match
          // rather than using ==.
          val secondXLine = 9
          dupIssue.meta.pos match {
            case full: InputPointer.Full =>
              assert(
                full.start.line == secondXLine,
                s"expected meta.pos.start.line == $secondXLine (second data X), got ${full.start.line}",
              )
            case other =>
              throw new AssertionError(s"expected InputPointer.Full for meta.pos, got: $other")
          }
        }
    }

    "produce DuplicateTemplateName for two sibling templates inside the same namespace (PR-29.4-D04)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, duplicateTemplateNameInNs)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssue = typerIssues(issues).collectFirst { case d: TyperIssue.DuplicateTemplateName => d }
            .getOrElse(throw new AssertionError(s"no DuplicateTemplateName in: ${typerIssues(issues)}"))
          assert(dupIssue.name == "X", s"expected name 'X', got '${dupIssue.name}'")
          assert(dupIssue.ownerName == "foo", s"expected ownerName 'foo', got '${dupIssue.ownerName}'")
        }
    }

    // D01: cross-namespace same-name templates must NOT fire DuplicateTemplateName
    "not produce DuplicateTemplateName for same-name templates in distinct namespaces (cross-ns negative control)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, crossNamespacesSameNameTemplateDomain)
        } yield {
          // The typer fails fast on DuplicateTemplateName, so a Right(Domain) outcome implies none fired.
          assert(outcome.isRight, s"expected typer to succeed for cross-namespace same-name templates, got: $outcome")
        }
    }

    // D03: 3+ duplicates — exactly ONE DuplicateTemplateName per group (not one per extra duplicate)
    "produce exactly one DuplicateTemplateName for three sibling templates with the same name" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, triplicateTemplateNameTopLevel)
        } yield {
          val issues    = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssues = typerIssues(issues).collect { case d: TyperIssue.DuplicateTemplateName => d }
          assert(
            dupIssues.size == 1,
            s"expected exactly 1 DuplicateTemplateName for 3-way duplicate, got ${dupIssues.size}: $dupIssues",
          )
          assert(dupIssues.head.name == "X", s"expected name 'X', got '${dupIssues.head.name}'")
        }
    }

    // D04: multi-segment ns path — ownerName must be "a.b" (dot-joined)
    "produce DuplicateTemplateName with ownerName 'a.b' for duplicate templates inside a two-level nested namespace" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, duplicateTemplateNameNestedNs)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val dupIssue = typerIssues(issues).collectFirst { case d: TyperIssue.DuplicateTemplateName => d }
            .getOrElse(throw new AssertionError(s"no DuplicateTemplateName in: ${typerIssues(issues)}"))
          assert(dupIssue.name == "X", s"expected name 'X', got '${dupIssue.name}'")
          assert(dupIssue.ownerName == "a.b", s"expected ownerName 'a.b', got '${dupIssue.ownerName}'")
        }
    }

    // D05: template X[T] and non-template `data X { … }` coexist — no DuplicateTemplateName
    "not produce DuplicateTemplateName when a template and a non-template share the same name" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, templateAndNonTemplateSameNameDomain)
        } yield {
          // The template is excised by TemplateRegistryBuilder; the non-template goes to ScopeBuilder.
          // Whatever ScopeBuilder does about the collision is a separate concern.
          // This assertion is ONLY about the absence of DuplicateTemplateName.
          val dupTemplateIssues = outcome match {
            case Right(_)    => Nil
            case Left(errs)  => typerIssues(errs).collect { case d: TyperIssue.DuplicateTemplateName => d }
          }
          assert(
            dupTemplateIssues.isEmpty,
            s"expected no DuplicateTemplateName issues, got: $dupTemplateIssues",
          )
        }
    }

    "register top-level template X[T] in Domain.templateRegistry with expected key and body" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, singleTemplateDomain)
        } yield {
          val domain   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val registry = domain.templateRegistry
          assert(
            registry.templates.size == 1,
            s"expected exactly 1 template in registry, got ${registry.templates.size}: ${registry.templates.keys}",
          )
          val expectedKey = (Pkg(NEList("template", "registry", "test")), Owner.Toplevel, TypeName("X"))
          val body = registry.templates.get(expectedKey).getOrElse {
            throw new AssertionError(s"expected key $expectedKey not found in registry; keys: ${registry.templates.keys}")
          }
          assert(
            body.typeParams == List(RawTypeName("T")),
            s"expected typeParams == List(RawTypeName(\"T\")), got: ${body.typeParams}",
          )
        }
    }
  }
}

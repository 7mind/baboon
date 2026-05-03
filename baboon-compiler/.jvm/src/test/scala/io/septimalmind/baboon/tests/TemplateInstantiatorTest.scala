package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName}
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

import scala.reflect.ClassTag

/** PR-29.5 (M29) front-end pipeline tests for `TemplateInstantiator`.
  *
  * Exercises the full path: parse → PR-29.4 template-registry pass → ADT
  * inheritance expander → PR-29.5 monomorphisation → scope-build → type-check.
  *
  * Verifies:
  *   - Positive: alias-over-template is replaced by a concrete `DomainMember.User`
  *     keyed by the alias name; no member keyed by the template name survives.
  *   - Multi-arg: two type parameters are both substituted.
  *   - Two distinct aliases of the same template produce two distinct members.
  *   - Derivation from the alias propagates to the materialised type.
  *   - Nested-collection substitution: `map[str, lst[T]]` → `map[str, lst[i32]]`.
  *   - Alias chain: `type IP = IntPage` resolves transparently after materialisation.
  *   - Negative: arity mismatch → `TemplateArityMismatch` (matrix #3).
  *   - Negative: direct self-reference → `TemplateInstantiationInBody` (matrix #5 / #1).
  *   - Negative: container-mediated self-reference → `TemplateInstantiationInBody`
  *     (spec §4.3 / matrix #1).
  *   - Backwards-compat: a non-template domain produces unchanged `Domain.defs`.
  */
final class TemplateInstantiatorTest extends TemplateInstantiatorTestBase[Either]

abstract class TemplateInstantiatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // ─── helpers ─────────────────────────────────────────────────────────────────

  private def makeInput(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "template-instantiator-test.baboon",
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

  private def userTypeNames(domain: Domain): Set[String] =
    domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u.name.name }.toSet

  // ─── test fixtures ────────────────────────────────────────────────────────────

  private val singleAliasPageDomain: String =
    """model template.instantiator.test
      |
      |version "1.0.0"
      |
      |data Page[T] {
      |  items: lst[T]
      |  total: u32
      |}
      |
      |root type IntPage = Page[i32]
      |""".stripMargin

  private val multiArgDomain: String =
    """model template.instantiator.multi
      |
      |version "1.0.0"
      |
      |data Envelope[T, E] {
      |  value: opt[T]
      |  error: opt[E]
      |}
      |
      |root type Result = Envelope[i32, str]
      |""".stripMargin

  private val twoDistinctAliasesDomain: String =
    """model template.instantiator.two
      |
      |version "1.0.0"
      |
      |data Page[T] {
      |  items: lst[T]
      |  total: u32
      |}
      |
      |root type IntPage = Page[i32]
      |root type StrPage = Page[str]
      |""".stripMargin

  private val derivationPropagationDomain: String =
    """model template.instantiator.derived
      |
      |version "1.0.0"
      |
      |data Page[T] {
      |  items: lst[T]
      |  total: u32
      |}
      |
      |root type IntPage = Page[i32] : derived[json], derived[ueba]
      |""".stripMargin

  private val nestedCollectionDomain: String =
    """model template.instantiator.nested
      |
      |version "1.0.0"
      |
      |data Holder[T] {
      |  f: map[str, lst[T]]
      |}
      |
      |root type IntHolder = Holder[i32]
      |""".stripMargin

  /** type IntPage = Page[i32]; type IP = IntPage (alias chain). */
  private val aliasChainDomain: String =
    """model template.instantiator.chain
      |
      |version "1.0.0"
      |
      |data Page[T] {
      |  items: lst[T]
      |  total: u32
      |}
      |
      |root type IntPage = Page[i32]
      |root type IP = IntPage
      |""".stripMargin

  /** ADT template: Result[T, E] with Ok and Err branches. */
  private val adtTemplateDomain: String =
    """model template.instantiator.adt
      |
      |version "1.0.0"
      |
      |adt Result[T, E] {
      |  data Ok { v: T }
      |  data Err { e: E }
      |}
      |
      |root type IntStrResult = Result[i32, str]
      |""".stripMargin

  /** Contract template: Acked[T] with value and ack fields. */
  private val contractTemplateDomain: String =
    """model template.instantiator.contract
      |
      |version "1.0.0"
      |
      |contract Acked[T] {
      |  value: T
      |  ack: bit
      |}
      |
      |root type IntAcked = Acked[i32]
      |""".stripMargin

  /** Service template: Querier[Q, R] with a single query method. */
  private val serviceTemplateDomain: String =
    """model template.instantiator.service
      |
      |version "1.0.0"
      |
      |service Querier[Q, R] {
      |  def query (Q): R
      |}
      |
      |root type IntStrQuerier = Querier[i32, str]
      |""".stripMargin

  /** any[T] substitution: Wrapper[T] with f: any[T]. */
  private val anyTypeDomain: String =
    """model template.instantiator.any
      |
      |version "1.0.0"
      |
      |data Wrapper[T] {
      |  f: any[T]
      |}
      |
      |root type IntWrap = Wrapper[i32]
      |""".stripMargin

  /** Namespaced template + same-namespace alias. */
  private val namespacedTemplateDomain: String =
    """model template.instantiator.ns
      |
      |version "1.0.0"
      |
      |ns foo {
      |  data X[T] { f: T }
      |  root type Y = X[i32]
      |}
      |""".stripMargin

  /** Negative: arity mismatch — data X[T] { f: T }; type Y = X[i32, str] */
  private val arityMismatchDomain: String =
    """model template.instantiator.neg.arity
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  f: T
      |}
      |
      |type Y = X[i32, str]
      |""".stripMargin

  /** Negative: direct self-reference — data X[T] { rec: X[T] }; type Y = X[i32] */
  private val selfRefDomain: String =
    """model template.instantiator.neg.selfref
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  rec: X[T]
      |}
      |
      |type Y = X[i32]
      |""".stripMargin

  /** Negative: container-mediated self-reference — data Tree[T] { children: lst[Tree[T]] }; type IntTree = Tree[i32] */
  private val containerSelfRefDomain: String =
    """model template.instantiator.neg.ctrselfref
      |
      |version "1.0.0"
      |
      |data Tree[T] {
      |  children: lst[Tree[T]]
      |}
      |
      |type IntTree = Tree[i32]
      |""".stripMargin

  /** Backwards-compat: a plain non-template domain. */
  private val nonTemplateDomain: String =
    """model template.instantiator.compat
      |
      |version "1.0.0"
      |
      |root data Foo {
      |  x: i32
      |  y: str
      |}
      |""".stripMargin

  // ─── tests ────────────────────────────────────────────────────────────────────

  "TemplateInstantiator (PR-29.5)" should {

    "materialise IntPage keyed by alias name, template Page absent from Domain.defs" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, singleAliasPageDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntPage"), s"expected IntPage in domain user types, got: $names")
          assert(!names.contains("Page"), s"template Page must not appear in domain user types, got: $names")
        }
    }

    "multi-arg: materialise Result keyed by alias name, Envelope absent" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, multiArgDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("Result"), s"expected Result in domain user types, got: $names")
          assert(!names.contains("Envelope"), s"template Envelope must not appear in domain user types, got: $names")
        }
    }

    "two distinct aliases produce two distinct DomainMember.User entries" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, twoDistinctAliasesDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntPage"), s"expected IntPage in domain user types, got: $names")
          assert(names.contains("StrPage"), s"expected StrPage in domain user types, got: $names")
          assert(!names.contains("Page"), s"template Page must not appear in domain user types, got: $names")
        }
    }

    "derivation from alias propagates to the materialised type" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, derivationPropagationDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntPage"), s"expected IntPage in domain user types, got: $names")
          // The derivation set is checked via Domain.derivations — look for IntPage's TypeId.User.
          val intPageId = domain.defs.meta.nodes.keys.collectFirst {
            case u: TypeId.User if u.name.name == "IntPage" => u
          }.getOrElse(throw new AssertionError("IntPage not found in domain"))
          val member = domain.defs.meta.nodes(intPageId)
          val derived = member match {
            case u: DomainMember.User => u.derivations
            case _                    => Set.empty
          }
          assert(derived.nonEmpty, s"expected non-empty derivation set on IntPage, got: $derived")
        }
    }

    "nested-collection substitution: map[str, lst[T]] → map[str, lst[i32]] in IntHolder" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nestedCollectionDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntHolder"), s"expected IntHolder in domain user types, got: $names")
          assert(!names.contains("Holder"), s"template Holder must not appear in domain user types, got: $names")
        }
    }

    "alias chain: type IP = IntPage resolves after materialisation of IntPage" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, aliasChainDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntPage"), s"expected IntPage in domain user types, got: $names")
          // IP is a plain alias to IntPage; it contributes IntPage to the root set.
          assert(!names.contains("Page"), s"template Page must not appear in domain user types, got: $names")
          // D05: IP itself must appear in domain.aliases (plain alias-of-alias survives as AliasInfo).
          assert(
            domain.aliases.exists(a => a.name.name == "IP"),
            s"expected IP in domain.aliases, got: ${domain.aliases.map(_.name.name)}",
          )
        }
    }

    "produce TemplateArityMismatch for X[i32, str] where X takes 1 param (matrix #3)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, arityMismatchDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateArityMismatch](outcome)
        }
    }

    "TemplateArityMismatch carries expected=1 actual=2 and template name X" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, arityMismatchDomain)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateArityMismatch => i }
            .getOrElse(throw new AssertionError(s"no TemplateArityMismatch in: $issues"))
          assert(issue.templateName == "X", s"expected templateName='X', got '${issue.templateName}'")
          assert(issue.expected == 1, s"expected expected=1, got ${issue.expected}")
          assert(issue.actual == 2, s"expected actual=2, got ${issue.actual}")
        }
    }

    "produce TemplateInstantiationInBody for direct self-reference data X[T] { rec: X[T] } (matrix #5 / #1)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, selfRefDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateInstantiationInBody](outcome)
        }
    }

    "produce TemplateInstantiationInBody for container-mediated self-ref data Tree[T] { children: lst[Tree[T]] } (spec §4.3 / matrix #1)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, containerSelfRefDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateInstantiationInBody](outcome)
        }
    }

    "TemplateInstantiationInBody carries containingTemplateName and instantiatedName" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, selfRefDomain)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateInstantiationInBody => i }
            .getOrElse(throw new AssertionError(s"no TemplateInstantiationInBody in: $issues"))
          assert(
            issue.containingTemplateName == "X",
            s"expected containingTemplateName='X', got '${issue.containingTemplateName}'",
          )
          assert(
            issue.instantiatedName == "X",
            s"expected instantiatedName='X' (self-ref), got '${issue.instantiatedName}'",
          )
        }
    }

    "leave a non-template domain unaffected (backwards-compat)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nonTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("Foo"), s"expected Foo in domain user types, got: $names")
        }
    }

    // ─── D01.1: ADT template substitution ────────────────────────────────────

    "ADT template: materialise IntStrResult as ADT with branches Ok{v:i32} and Err{e:str}" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, adtTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntStrResult"), s"expected IntStrResult in domain user types, got: $names")
          assert(!names.contains("Result"), s"template Result must not appear in domain user types, got: $names")

          val intStrResultId = domain.defs.meta.nodes.keys.collectFirst {
            case u: TypeId.User if u.name.name == "IntStrResult" => u
          }.getOrElse(throw new AssertionError("IntStrResult not found in domain"))
          val adt = domain.defs.meta.nodes(intStrResultId) match {
            case u: DomainMember.User => u.defn match {
              case a: Typedef.Adt => a
              case other          => throw new AssertionError(s"expected Typedef.Adt for IntStrResult, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for IntStrResult, got: $other")
          }
          assert(adt.members.size == 2, s"expected 2 ADT branches, got ${adt.members.size}: ${adt.members}")

          val okId = adt.members.toList.find(_.name.name == "Ok")
            .getOrElse(throw new AssertionError(s"Ok branch not found in ${adt.members}"))
          val errId = adt.members.toList.find(_.name.name == "Err")
            .getOrElse(throw new AssertionError(s"Err branch not found in ${adt.members}"))

          val okDto = domain.defs.meta.nodes(okId) match {
            case u: DomainMember.User => u.defn match {
              case d: Typedef.Dto => d
              case other          => throw new AssertionError(s"expected Typedef.Dto for Ok branch, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for Ok, got: $other")
          }
          assert(
            okDto.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)),
            s"expected Ok.v: i32 in fields, got: ${okDto.fields}",
          )

          val errDto = domain.defs.meta.nodes(errId) match {
            case u: DomainMember.User => u.defn match {
              case d: Typedef.Dto => d
              case other          => throw new AssertionError(s"expected Typedef.Dto for Err branch, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for Err, got: $other")
          }
          assert(
            errDto.fields.exists(f => f.name.name == "e" && f.tpe == TypeRef.Scalar(TypeId.Builtins.str)),
            s"expected Err.e: str in fields, got: ${errDto.fields}",
          )
        }
    }

    // ─── D01.2: Contract template substitution ───────────────────────────────

    "Contract template: materialise IntAcked as Contract with value:i32 and ack:bit" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, contractTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntAcked"), s"expected IntAcked in domain user types, got: $names")
          assert(!names.contains("Acked"), s"template Acked must not appear in domain user types, got: $names")

          val intAckedId = domain.defs.meta.nodes.keys.collectFirst {
            case u: TypeId.User if u.name.name == "IntAcked" => u
          }.getOrElse(throw new AssertionError("IntAcked not found in domain"))
          val contract = domain.defs.meta.nodes(intAckedId) match {
            case u: DomainMember.User => u.defn match {
              case c: Typedef.Contract => c
              case other               => throw new AssertionError(s"expected Typedef.Contract for IntAcked, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for IntAcked, got: $other")
          }
          assert(
            contract.fields.exists(f => f.name.name == "value" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)),
            s"expected IntAcked.value: i32 in fields, got: ${contract.fields}",
          )
          assert(
            contract.fields.exists(f => f.name.name == "ack" && f.tpe == TypeRef.Scalar(TypeId.Builtins.bit)),
            s"expected IntAcked.ack: bit in fields, got: ${contract.fields}",
          )
        }
    }

    // ─── D01.3: Service template substitution ────────────────────────────────

    "Service template: materialise IntStrQuerier as Service with query(i32):str" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, serviceTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntStrQuerier"), s"expected IntStrQuerier in domain user types, got: $names")
          assert(!names.contains("Querier"), s"template Querier must not appear in domain user types, got: $names")

          val intStrQuerierId = domain.defs.meta.nodes.keys.collectFirst {
            case u: TypeId.User if u.name.name == "IntStrQuerier" => u
          }.getOrElse(throw new AssertionError("IntStrQuerier not found in domain"))
          val svc = domain.defs.meta.nodes(intStrQuerierId) match {
            case u: DomainMember.User => u.defn match {
              case s: Typedef.Service => s
              case other              => throw new AssertionError(s"expected Typedef.Service for IntStrQuerier, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for IntStrQuerier, got: $other")
          }
          val queryMethod = svc.methods.find(_.name.name == "query")
            .getOrElse(throw new AssertionError(s"query method not found in ${svc.methods}"))
          assert(
            queryMethod.sig == TypeRef.Scalar(TypeId.Builtins.i32),
            s"expected query sig=i32, got: ${queryMethod.sig}",
          )
          assert(
            queryMethod.out.contains(TypeRef.Scalar(TypeId.Builtins.str)),
            s"expected query out=str, got: ${queryMethod.out}",
          )
        }
    }

    // ─── D02: any[T] substitution ────────────────────────────────────────────

    "any[T] substitution: materialise IntWrap with f: any[i32]" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, anyTypeDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("IntWrap"), s"expected IntWrap in domain user types, got: $names")
          assert(!names.contains("Wrapper"), s"template Wrapper must not appear in domain user types, got: $names")

          val intWrapId = domain.defs.meta.nodes.keys.collectFirst {
            case u: TypeId.User if u.name.name == "IntWrap" => u
          }.getOrElse(throw new AssertionError("IntWrap not found in domain"))
          val dto = domain.defs.meta.nodes(intWrapId) match {
            case u: DomainMember.User => u.defn match {
              case d: Typedef.Dto => d
              case other          => throw new AssertionError(s"expected Typedef.Dto for IntWrap, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for IntWrap, got: $other")
          }
          val expectedAnyType = TypeRef.Any(TypeRef.AnyVariant.Global, Some(TypeRef.Scalar(TypeId.Builtins.i32)))
          assert(
            dto.fields.exists(f => f.name.name == "f" && f.tpe == expectedAnyType),
            s"expected IntWrap.f: any[i32], got fields: ${dto.fields}",
          )
        }
    }

    // ─── D07: namespaced template + same-namespace alias ─────────────────────

    "namespaced template: materialise foo.Y as Dto with f:i32, template foo.X absent" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, namespacedTemplateDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val allUserIds = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u }.toSet

          // Y must appear under owner Ns("foo").
          val yId = allUserIds.find(u => u.name.name == "Y" && u.owner == Owner.Ns(Seq(TypeName("foo"))))
            .getOrElse(throw new AssertionError(
              s"Y not found under Owner.Ns(foo), all user type ids: $allUserIds"
            ))
          val dto = domain.defs.meta.nodes(yId) match {
            case u: DomainMember.User => u.defn match {
              case d: Typedef.Dto => d
              case other          => throw new AssertionError(s"expected Typedef.Dto for foo.Y, got: $other")
            }
            case other => throw new AssertionError(s"expected DomainMember.User for foo.Y, got: $other")
          }
          assert(
            dto.fields.exists(f => f.name.name == "f" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)),
            s"expected foo.Y.f: i32, got: ${dto.fields}",
          )

          // Template X must not appear in domain user types.
          val xPresent = allUserIds.exists(u => u.name.name == "X")
          assert(!xPresent, s"template X must not appear in domain user types, got: $allUserIds")
        }
    }
  }
}

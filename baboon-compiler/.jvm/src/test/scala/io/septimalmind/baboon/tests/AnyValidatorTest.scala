package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, VerificationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{BaboonFamily, Typedef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class AnyValidatorTest extends AnyValidatorTestBase[Either]

abstract class AnyValidatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private def loadFamily(manager: BaboonFamilyManager[F], name: String, body: String): F[NEList[BaboonIssue], BaboonFamily] = {
    manager.load(List(makeInput(name, body)))
  }

  // Drives the full compile pipeline (parser → typer → comparator → validator) and returns
  // Either[issues, BaboonFamily]. Tests use this to assert on specific validator issue types.
  private def runValidator(manager: BaboonFamilyManager[F], name: String, body: String): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loadFamily(manager, name, body).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  private def verificationIssues(issues: NEList[BaboonIssue]): List[VerificationIssue] = {
    issues.toList.collect { case BaboonIssue.Verification(vi) => vi }
  }

  "any validator — positive cases" should {

    "accept `any[Foo]` when Foo has derived[ueba]" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.ok.d1
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: any[Foo]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-ok-d1.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept bare `any` (variant A)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.ok.bare
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: any
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-ok-bare.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept `any[domain:this, Foo]` and `any[domain:current, Foo]`" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.ok.dquals
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  d2: any[domain:this, Foo]
            |  d3: any[domain:current, Foo]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-ok-dquals.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }

    "accept `opt[any]`, `lst[any]`, and `map[str, any[Foo]]`" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.ok.generic
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  a: opt[any]
            |  b: lst[any[domain:current]]
            |  c: map[str, any[Foo]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-ok-generic.baboon", model)
        } yield {
          assert(outcome.isRight, s"expected clean validation, got: $outcome")
        }
    }
  }

  "any validator — negative cases" should {

    "reject `any[Foo]` when Foo lacks derived[ueba]" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.noueba
            |
            |version "1.0.0"
            |
            |data Foo {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: any[Foo]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-noueba.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingLacksUebaDerivation]),
            s"expected AnyUnderlyingLacksUebaDerivation, got: $vi",
          )
          // Must NOT also fire the not-user-type check (Foo IS a user DTO).
          assert(
            !vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"did not expect AnyUnderlyingNotUserType for valid user DTO, got: $vi",
          )
        }
    }

    "reject `any[i32]` (builtin scalar)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.scalar
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: any[i32]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-scalar.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"expected AnyUnderlyingNotUserType, got: $vi",
          )
        }
    }

    "reject `any[opt[Foo]]` (underlying is a constructor)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.collection
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: any[opt[Foo]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-collection.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"expected AnyUnderlyingNotUserType, got: $vi",
          )
        }
    }

    "reject `map[any, str]` (any as map key)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.mapkey
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: map[any, str]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-mapkey.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsMapKey]),
            s"expected AnyAsMapKey, got: $vi",
          )
        }
    }

    "reject `set[any]` (any as set element)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.setelem
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: set[any]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-setelem.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsSetElement]),
            s"expected AnyAsSetElement, got: $vi",
          )
        }
    }

    "reject `map[any[Foo], str]` with AnyAsMapKey even when underlying is valid" in {
      // Per design: the `any` wrapper makes the key opaque regardless of underlying;
      // the `derived[ueba]` check is orthogonal when the position is already invalid.
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.mapkeyfoo
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: map[any[Foo], str]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-mapkeyfoo.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsMapKey]),
            s"expected AnyAsMapKey, got: $vi",
          )
          assert(
            !vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingLacksUebaDerivation]),
            s"did not expect AnyUnderlyingLacksUebaDerivation (Foo has derived[ueba]), got: $vi",
          )
        }
    }

    "reject `set[any[Foo]]` with AnyAsSetElement even when underlying is valid" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.setfoo
            |
            |version "1.0.0"
            |
            |data Foo : derived[ueba] {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: set[any[Foo]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-setfoo.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsSetElement]),
            s"expected AnyAsSetElement, got: $vi",
          )
        }
    }

    "reject `any[Foreign]` (foreign type as underlying)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.foreign
            |
            |version "1.0.0"
            |
            |foreign Bar {
            |  scala = "scala.Predef.String"
            |  cs = "string"
            |  py = "str"
            |  rust = "String"
            |  typescript = "string"
            |  kotlin = "kotlin.String"
            |  java = "java.lang.String"
            |  dart = "String"
            |  swift = "String"
            |}
            |
            |root data Holder {
            |  f: any[Bar]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-foreign.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"expected AnyUnderlyingNotUserType, got: $vi",
          )
        }
    }

    "reject `contract C { f: any[i32] }` referenced from a root DTO" in {
      // Asserts D04's contract coverage. The model trimmer prunes contracts that aren't
      // reachable from a root, so a *truly* standalone contract is never validated.
      // The minimum reachable shape is a root DTO that `is` the contract — the validator
      // then visits both the Contract and the inheriting DTO (acceptable duplication
      // per the design comment). Either side firing the issue type proves the contract
      // path is wired.
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.contract
            |
            |version "1.0.0"
            |
            |contract Standalone {
            |  f: any[i32]
            |}
            |
            |root data Holder {
            |  is Standalone
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-contract.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"expected AnyUnderlyingNotUserType, got: $vi",
          )
          // The Contract owner must appear in the issues (proves contract-side walk).
          val contractIssues = vi.collect {
            case i: VerificationIssue.AnyUnderlyingNotUserType
                if i.owner.isInstanceOf[Typedef.Contract] =>
              i
          }
          assert(
            contractIssues.nonEmpty,
            s"expected at least one AnyUnderlyingNotUserType pointing at the Contract, got: $vi",
          )
        }
    }

    "reject `opt[map[any, str]]` (deeply nested any-as-map-key)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.deepmap
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: opt[map[any, str]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-deepmap.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsMapKey]),
            s"expected AnyAsMapKey at any depth, got: $vi",
          )
        }
    }

    "reject `lst[set[any]]` (deeply nested any-as-set-element)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.deepset1
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: lst[set[any]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-deepset1.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsSetElement]),
            s"expected AnyAsSetElement at any depth, got: $vi",
          )
        }
    }

    "reject `opt[set[any]]` (deeply nested any-as-set-element under opt)" in {
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.deepset2
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: opt[set[any]]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-deepset2.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsSetElement]),
            s"expected AnyAsSetElement at any depth, got: $vi",
          )
        }
    }

    "reject `any[i32]` in service method out slot (D17)" in {
      // Asserts D17's service-walk coverage. The validator synthesises pseudo-Fields
      // named `<methodName>.{sig|out|err}` for every method slot and runs the same
      // any-rule checks against them. The OUT slot is the most reliable place to put
      // the offending `any[i32]` (parser requires exactly one `out` per method).
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model my.test.svc
            |
            |version "1.0.0"
            |
            |data Req { x: i32 }
            |
            |root service S {
            |  def m (
            |    in = Req
            |    out = any[i32]
            |  )
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-svc.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          val svcIssues = vi.collect {
            case i: VerificationIssue.AnyUnderlyingNotUserType
                if i.owner.isInstanceOf[Typedef.Service] =>
              i
          }
          assert(
            svcIssues.nonEmpty,
            s"expected at least one AnyUnderlyingNotUserType pointing at the Service, got: $vi",
          )
          assert(
            svcIssues.exists(_.badFields.exists(_.name.name.endsWith(".out"))),
            s"expected an offending pseudo-field named `<method>.out`, got: ${svcIssues.flatMap(_.badFields.map(_.name.name))}",
          )
        }
    }

    "reject `any[i32]` in adt own (contract-flattened) fields (D18)" in {
      // Asserts D18's adt-own-fields walk. ADT `fields` are populated by flattening
      // the fields of every contract referenced via `is`. With `contract BadC { f: any[i32] }`
      // and `root adt A is BadC { ... }`, A.fields will include the offending field —
      // the adt-walk in BaboonValidator must observe it and fire AnyUnderlyingNotUserType
      // with `owner.isInstanceOf[Typedef.Adt]`.
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model my.test.adt
            |
            |version "1.0.0"
            |
            |contract BadC {
            |  f: any[i32]
            |}
            |
            |root adt A {
            |  is BadC
            |
            |  data B {}
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-adt.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"expected AnyUnderlyingNotUserType, got: $vi",
          )
          val adtIssues = vi.collect {
            case i: VerificationIssue.AnyUnderlyingNotUserType
                if i.owner.isInstanceOf[Typedef.Adt] =>
              i
          }
          assert(
            adtIssues.nonEmpty,
            s"expected at least one AnyUnderlyingNotUserType pointing at the Adt (proves adt-own-fields walk fires), got: $vi",
          )
        }
    }

    "reject `map[any, any]` with AnyAsMapKey but allow the value-side `any`" in {
      // Per design §"Validator rules": `any` is allowed as map value but rejected as map key.
      // This test asserts only the key-side issue fires — no value-side rule should trigger.
      (manager: BaboonFamilyManager[F]) =>
        val model =
          """model any.validator.bad.mapkeyval
            |
            |version "1.0.0"
            |
            |root data Holder {
            |  f: map[any, any]
            |}
            |""".stripMargin
        for {
          outcome <- runValidator(manager, "any-val-bad-mapkeyval.baboon", model)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val vi     = verificationIssues(issues)
          assert(
            vi.exists(_.isInstanceOf[VerificationIssue.AnyAsMapKey]),
            s"expected AnyAsMapKey, got: $vi",
          )
          assert(
            !vi.exists(_.isInstanceOf[VerificationIssue.AnyAsSetElement]),
            s"did not expect AnyAsSetElement, got: $vi",
          )
          // Value-side `any` (untyped) has no underlying — no derivation / user-type issues.
          assert(
            !vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingNotUserType]),
            s"did not expect AnyUnderlyingNotUserType, got: $vi",
          )
          assert(
            !vi.exists(_.isInstanceOf[VerificationIssue.AnyUnderlyingLacksUebaDerivation]),
            s"did not expect AnyUnderlyingLacksUebaDerivation, got: $vi",
          )
        }
    }
  }
}

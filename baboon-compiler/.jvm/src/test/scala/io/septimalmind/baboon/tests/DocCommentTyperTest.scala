package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.{BaboonTyper, DocFormat}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** Typer-side tests for PR-30.3 (M30): doc-comment lifting from `RawNodeMeta`
  * into the typed model.
  *
  * Verifies (per `docs/spec/docstrings.md`):
  *   - prefix `/** … */` on a top-level `data`/`adt`/`enum`/`contract`/
  *     `service` lifts to `DomainMember.User.docs.prefix.cleaned`;
  *   - prefix doc on a field lifts to `Field.docs.prefix.cleaned`;
  *   - postfix `//!` on a field lifts to `Field.docs.suffix.cleaned`;
  *   - prefix doc on a `def` inside a service lifts to
  *     `MethodDef.docs.prefix.cleaned`;
  *   - prefix doc on individual enum VALUES is silently dropped (spec §9 — per-
  *     enum-value docs are deferred);
  *   - prefix doc on ADT inheritance arms (`+ Ref`, etc.) is silently dropped
  *     (spec §3.2 — arms are structural, no carrier);
  *   - prefix doc on a plain (non-template) alias is silently dropped because
  *     the alias has no `DomainMember.User` carrier;
  *   - template propagation (Q1 lock — spec §6): `IntPage / Page[T]` with both
  *     alias-doc and template-doc produces the merged type-level doc and the
  *     field doc propagated verbatim from the template body.
  */
final class DocCommentTyperTest extends DocCommentTyperTestBase[Either]

abstract class DocCommentTyperTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def runTyper(parser: BaboonParser[F], typer: BaboonTyper[F], src: String, name: String): F[NEList[BaboonIssue], Domain] = {
    val input = BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), src)
    for {
      parsed <- parser.parse(input)
      domain <- typer.process(parsed)
    } yield domain
  }

  private def userByName(domain: Domain, name: String): DomainMember.User = {
    val id = domain.defs.meta.nodes.keys.collectFirst {
      case u: TypeId.User if u.name.name == name => u
    }.getOrElse(throw new AssertionError(s"$name not found in: ${domain.defs.meta.nodes.keys}"))
    domain.defs.meta.nodes(id) match {
      case u: DomainMember.User => u
      case other                => throw new AssertionError(s"$name must be a User, got $other")
    }
  }

  "DocFormat.cleanPrefix" should {

    "strip Javadoc-style `*`-prefixed multi-line bodies — spec §5.5 worked example" in {
      (_: BaboonParser[F]) =>
        val raw =
          """/**
            | *  First paragraph.
            | *  Continued.
            | *
            | *  Second paragraph.
            | */""".stripMargin
        val cleaned = DocFormat.cleanPrefix(raw)
        assert(cleaned == "First paragraph.\nContinued.\n\nSecond paragraph.")
    }

    "handle bodies without `*` continuation markers — spec §5.2 second example" in {
      (_: BaboonParser[F]) =>
        val raw =
          """/**
            |  text without star
            |  more text
            |  */""".stripMargin
        val cleaned = DocFormat.cleanPrefix(raw)
        assert(cleaned == "text without star\nmore text")
    }

    "handle separator lines shorter than the common prefix — spec §5.2 step 3c edge case" in {
      (_: BaboonParser[F]) =>
        // Common prefix " *  " (space, asterisk, two spaces). Separator line is " *" only —
        // strip should not index past the separator's end; result is empty for that line.
        val raw =
          """/**
            | *  alpha
            | *
            | *  beta
            | */""".stripMargin
        val cleaned = DocFormat.cleanPrefix(raw)
        assert(cleaned == "alpha\n\nbeta")
    }

    "be idempotent on already-cleaned input" in {
      (_: BaboonParser[F]) =>
        val cleaned1 = DocFormat.cleanPrefix("/** hello */")
        // Re-feed the cleaned form (no delimiters) — should remain identical.
        val cleaned2 = DocFormat.cleanPrefix(cleaned1)
        assert(cleaned1 == "hello")
        assert(cleaned2 == "hello")
    }
  }

  "DocFormat.cleanSuffix" should {
    "strip the `//!` marker and a single optional leading space" in {
      (_: BaboonParser[F]) =>
        assert(DocFormat.cleanSuffix("//! the user id") == "the user id")
        assert(DocFormat.cleanSuffix("//!the user id")  == "the user id")
        assert(DocFormat.cleanSuffix("//!  two-space") == " two-space")
    }
  }

  "doc-comment typer lift" should {

    "lift a prefix doc on `root data` to DomainMember.User.docs.prefix" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.types
            |
            |version "1.0.0"
            |
            |/** Type-level doc for Foo. */
            |root data Foo {
            |  /** field doc */
            |  x: i32  //! a postfix
            |}
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-types.baboon")
        } yield {
          val foo = userByName(domain, "Foo")
          assert(foo.docs.prefix.map(_.cleaned).contains("Type-level doc for Foo."))
          assert(foo.docs.suffix.isEmpty)

          val dto    = foo.defn.asInstanceOf[Typedef.Dto]
          val xField = dto.fields.find(_.name.name == "x").getOrElse(fail("x not found"))
          assert(xField.docs.prefix.map(_.cleaned).contains("field doc"))
          assert(xField.docs.suffix.map(_.cleaned).contains("a postfix"))
        }
    }

    "lift a prefix doc on `root service` and on a service `def` to their respective docs fields" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.svc
            |
            |version "1.0.0"
            |
            |data Req  { x: i32 }
            |data Resp { y: i32 }
            |
            |/** service doc */
            |root service Svc {
            |  /** call this */
            |  def doIt (Req): Resp
            |}
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-svc.baboon")
        } yield {
          val svcUser = userByName(domain, "Svc")
          assert(svcUser.docs.prefix.map(_.cleaned).contains("service doc"))
          val svc    = svcUser.defn.asInstanceOf[Typedef.Service]
          val method = svc.methods.find(_.name.name == "doIt").getOrElse(fail("doIt missing"))
          assert(method.docs.prefix.map(_.cleaned).contains("call this"))
        }
    }

    "silently drop a prefix doc on an individual enum VALUE (spec §9 deferred non-feature)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.enumvalue
            |
            |version "1.0.0"
            |
            |/** the colour enum */
            |enum Colour {
            |  /** this doc has no carrier */
            |  Red
            |  Green
            |}
            |
            |root data Holder { c: Colour }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-enumvalue.baboon")
        } yield {
          val cu = userByName(domain, "Colour")
          // Enum-type-level doc DOES surface.
          assert(cu.docs.prefix.map(_.cleaned).contains("the colour enum"))
          // Per-enum-value doc is silently dropped — there is no per-value Docs slot
          // in the typed `EnumMember`, by design.
          val enumDef = cu.defn.asInstanceOf[Typedef.Enum]
          // Sanity: members exist and parse succeeded with the doc above Red.
          assert(enumDef.members.toList.map(_.name).contains("Red"))
        }
    }

    "silently drop a prefix doc on an ADT inheritance arm (spec §3.2)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.adtarm
            |
            |version "1.0.0"
            |
            |adt Other {
            |  data X { v: i32 }
            |  data Y { w: i32 }
            |}
            |
            |/** envelope adt */
            |adt Env {
            |  /** this doc binds to nothing */
            |  + Other
            |  data Z { z: i32 }
            |}
            |
            |root data Holder { e: Env }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-adtarm.baboon")
        } yield {
          val env = userByName(domain, "Env")
          assert(env.docs.prefix.map(_.cleaned).contains("envelope adt"))
          // The "+ Other" arm's doc is structural and has no User carrier.
          // The included branches X/Y/Z are themselves Users — none should
          // carry the dropped arm doc.
          val zUser = userByName(domain, "Z")
          assert(zUser.docs.prefix.isEmpty)
        }
    }

    "silently drop a prefix doc on a plain (non-template) alias (spec §3.1)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.alias
            |
            |version "1.0.0"
            |
            |root data X { v: i32 }
            |
            |/** this alias has no carrier */
            |type Y = X
            |
            |root data Holder { y: Y }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-alias.baboon")
        } yield {
          // Plain aliases do not surface as DomainMember.User; the alias name
          // simply isn't a typed-model node. The doc above the alias has no
          // carrier and is silently dropped.
          val ids = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u }.toList
          assert(!ids.exists(_.name.name == "Y"), s"Y should not surface as a User; got $ids")
        }
    }

    "merge alias-doc and template-doc on a synthesized template instantiation (Q1 lock — spec §6)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.template
            |
            |version "1.0.0"
            |
            |/** template doc */
            |data Page[T] {
            |  /** items doc */
            |  items: lst[T]
            |  total: u32
            |}
            |
            |/** alias doc */
            |type IntPage = Page[i32]
            |
            |root data Holder { p: IntPage }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-template.baboon")
        } yield {
          val intPage = userByName(domain, "IntPage")
          assert(
            intPage.docs.prefix.map(_.cleaned).contains("alias doc\n\ntemplate doc"),
            s"got: ${intPage.docs.prefix.map(_.cleaned)}",
          )
          val dto   = intPage.defn.asInstanceOf[Typedef.Dto]
          val items = dto.fields.find(_.name.name == "items").getOrElse(fail("items missing"))
          assert(items.docs.prefix.map(_.cleaned).contains("items doc"))
          val total = dto.fields.find(_.name.name == "total").getOrElse(fail("total missing"))
          assert(total.docs.prefix.isEmpty)
        }
    }

    "use template-only doc on the synthesized type when alias has no doc" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.template2
            |
            |version "1.0.0"
            |
            |/** only-template doc */
            |data Box[T] { value: T }
            |
            |type IntBox = Box[i32]
            |
            |root data Holder { b: IntBox }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-template2.baboon")
        } yield {
          val intBox = userByName(domain, "IntBox")
          assert(intBox.docs.prefix.map(_.cleaned).contains("only-template doc"))
        }
    }

    "use alias-only doc on the synthesized type when template has no doc" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val src =
          """model m30.typer.template3
            |
            |version "1.0.0"
            |
            |data Bag[T] { item: T }
            |
            |/** only-alias doc */
            |type IntBag = Bag[i32]
            |
            |root data Holder { b: IntBag }
            |""".stripMargin
        for {
          domain <- runTyper(parser, typer, src, "m30-typer-template3.baboon")
        } yield {
          val intBag = userByName(domain, "IntBag")
          assert(intBag.docs.prefix.map(_.cleaned).contains("only-alias doc"))
        }
    }
  }
}

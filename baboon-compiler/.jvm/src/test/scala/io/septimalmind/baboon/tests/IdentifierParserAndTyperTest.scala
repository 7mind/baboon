package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, RawDtoMember, RawFieldName, RawTLDef, RawTypeName}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class IdentifierParserAndTyperTest extends IdentifierParserAndTyperTestBase[Either]

abstract class IdentifierParserAndTyperTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  // ──────────────────────────────────────────────────────────────────────────
  // Parser-level tests: verify the raw AST shape produced by the grammar
  // ──────────────────────────────────────────────────────────────────────────

  "baboon id keyword — parser" should {

    // 1.  `id Foo { x: i32 }` → RawTLDef.Identifier(root=false, RawIdentifier(name="Foo", ...))
    "parse `id Foo { x: i32 }` as a non-root RawTLDef.Identifier" in {
      (parser: BaboonParser[F]) =>
        val input = makeInput(
          "id-basic.baboon",
          """model test.id.basic
            |
            |version "1.0.0"
            |
            |id Foo { x: i32 }
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
        } yield {
          val defs  = parsed.members.defs
          val idDef = defs.collectFirst { case d: RawTLDef.Identifier => d }
          assert(idDef.isDefined, s"expected RawTLDef.Identifier in top-level defs, got: $defs")
          val id = idDef.get
          assert(!id.root, s"expected root=false, got root=${id.root}")
          assert(id.value.name == RawTypeName("Foo"), s"expected name=Foo, got ${id.value.name}")
          val fieldNames = id.value.members.collect { case f: RawDtoMember.FieldDef => f.field.name }
          assert(
            fieldNames.contains(RawFieldName("x")),
            s"expected field named x, got: $fieldNames",
          )
        }
    }

    // 2.  `root id Bar { x: uid }` → root=true
    "parse `root id Bar { x: uid }` with root=true" in {
      (parser: BaboonParser[F]) =>
        val input = makeInput(
          "id-root.baboon",
          """model test.id.root
            |
            |version "1.0.0"
            |
            |root id Bar { x: uid }
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
        } yield {
          val defs  = parsed.members.defs
          val idDef = defs.collectFirst { case d: RawTLDef.Identifier => d }
          assert(idDef.isDefined, s"expected RawTLDef.Identifier in top-level defs, got: $defs")
          val id = idDef.get
          assert(id.root, s"expected root=true, got root=${id.root}")
          assert(id.value.name == RawTypeName("Bar"), s"expected name=Bar, got ${id.value.name}")
        }
    }

    // 3.  `ns N { id Q { x: str } }` — id nested inside ns is allowed
    "parse `id` inside a namespace" in {
      (parser: BaboonParser[F]) =>
        val input = makeInput(
          "id-ns.baboon",
          """model test.id.ns
            |
            |version "1.0.0"
            |
            |ns N { id Q { x: str } }
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
        } yield {
          val defs = parsed.members.defs
          // The namespace itself appears as RawTLDef.Namespace
          val nsDef = defs.collectFirst { case d: RawTLDef.Namespace => d }
          assert(nsDef.isDefined, s"expected a namespace def, got: $defs")
          val innerDefs = nsDef.get.value.defns
          val idDef     = innerDefs.collectFirst { case d: RawTLDef.Identifier => d }
          assert(idDef.isDefined, s"expected RawTLDef.Identifier inside namespace, got: $innerDefs")
          assert(idDef.get.value.name == RawTypeName("Q"), s"expected name=Q, got ${idDef.get.value.name}")
        }
    }

    // 4.  `id` as a field name inside a `data` DTO — Q-M18-9: must not be rejected
    "allow `id` as a field name inside a `data` DTO" in {
      (parser: BaboonParser[F]) =>
        val input = makeInput(
          "id-as-field-name.baboon",
          """model test.id.fieldname
            |
            |version "1.0.0"
            |
            |root data User {
            |  id: uid
            |  name: str
            |}
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
        } yield {
          val defs   = parsed.members.defs
          val dtoDef = defs.collectFirst { case d: RawTLDef.DTO => d }
          assert(dtoDef.isDefined, s"expected a DTO def, got: $defs")
          val fieldNames = dtoDef.get.value.members.collect {
            case f: RawDtoMember.FieldDef => f.field.name
          }
          assert(
            fieldNames.contains(RawFieldName("id")),
            s"expected field named `id`, got: $fieldNames",
          )
          assert(
            fieldNames.contains(RawFieldName("name")),
            s"expected field named `name`, got: $fieldNames",
          )
        }
    }
  }

  // ──────────────────────────────────────────────────────────────────────────
  // Typer-level tests: verify the typed AST flag `isIdentifier`
  // ──────────────────────────────────────────────────────────────────────────

  "baboon id keyword — typer" should {

    // 5.  `id Foo { x: i32 }` → Typedef.Dto with isIdentifier == true
    "produce Typedef.Dto with isIdentifier=true for `id Foo { x: i32 }`" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val input = makeInput(
          "id-typer-pos.baboon",
          """model test.id.typer.pos
            |
            |version "1.0.0"
            |
            |root id Foo { x: i32 }
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
          domain <- typer.process(parsed)
        } yield {
          val dtos = domain.defs.meta.nodes.values.collect {
            case u: DomainMember.User if u.defn.isInstanceOf[Typedef.Dto] =>
              u.defn.asInstanceOf[Typedef.Dto]
          }.toList
          val foo = dtos.find(_.id.name.name == "Foo")
          assert(foo.isDefined, s"expected Typedef.Dto named Foo, found: ${dtos.map(_.id.name.name)}")
          assert(
            foo.get.isIdentifier,
            s"expected isIdentifier=true for `id Foo`, got isIdentifier=${foo.get.isIdentifier}",
          )
        }
    }

    // 6.  `data Foo { x: i32 }` → Typedef.Dto with isIdentifier == false
    "produce Typedef.Dto with isIdentifier=false for `data Foo { x: i32 }`" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val input = makeInput(
          "id-typer-neg.baboon",
          """model test.id.typer.neg
            |
            |version "1.0.0"
            |
            |root data Foo { x: i32 }
            |""".stripMargin,
        )
        for {
          parsed <- parser.parse(input)
          domain <- typer.process(parsed)
        } yield {
          val dtos = domain.defs.meta.nodes.values.collect {
            case u: DomainMember.User if u.defn.isInstanceOf[Typedef.Dto] =>
              u.defn.asInstanceOf[Typedef.Dto]
          }.toList
          val foo = dtos.find(_.id.name.name == "Foo")
          assert(foo.isDefined, s"expected Typedef.Dto named Foo, found: ${dtos.map(_.id.name.name)}")
          assert(
            !foo.get.isIdentifier,
            s"expected isIdentifier=false for `data Foo`, got isIdentifier=${foo.get.isIdentifier}",
          )
        }
    }
  }
}

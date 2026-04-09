package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.explore.commands.{ShowCommand, TypesCommand}
import io.septimalmind.baboon.explore.{ExploreContext, ExploreInputs, TypeRenderer}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonEnquiries.BaboonEnquiriesImpl
import io.septimalmind.baboon.typer.BaboonRuntimeCodec
import io.septimalmind.baboon.typer.model._
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.reflect.TagKK

final class ExplorerTest extends ExplorerTestBase[Either]

abstract class ExplorerTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val testPkg = Pkg(NEList("testpkg", "pkg0"))
  private val v1      = Version.parse("1.0.0")
  private val v3      = Version.parse("3.0.0")

  private def withExploreCtx(loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F], version: Version = v1)(
    fn: ExploreContext[F] => Unit
  ): F[NEList[io.septimalmind.baboon.parser.model.issues.BaboonIssue], Unit] = {
    for {
      family <- loadPkg(loader)
    } yield {
      val ctx = new ExploreContext[F](
        family,
        new BaboonEnquiriesImpl(),
        codec,
        loader,
        ExploreInputs(Set.empty, Set.empty),
      )
      ctx.switchTo(testPkg, Some(version)) match {
        case Left(err) => fail(s"Failed to switch domain: $err")
        case Right(()) => fn(ctx)
      }
    }
  }

  "explorer context" should {
    "find regular types and aliases in v1" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          // Regular types
          assert(ctx.findType("T1_D1").isDefined, "T1_D1 should be findable")
          assert(ctx.findType("T4_A1").isDefined, "T4_A1 (ADT) should be findable")
          assert(ctx.findType("T1_E1").isDefined, "T1_E1 (enum) should be findable")

          // Aliases
          assert(ctx.findAlias("BinaryData").isDefined, "BinaryData alias should be findable")
          assert(ctx.findAlias("StringList").isDefined, "StringList alias should be findable")
          assert(ctx.findAlias("ChainedAlias").isDefined, "ChainedAlias alias should be findable")
          assert(ctx.findAlias("NsAlias").isDefined, "NsAlias (namespace-scoped) should be findable")

          // Non-existent
          assert(ctx.findType("NonExistent").isEmpty)
          assert(ctx.findAlias("NonExistent").isEmpty)

          // Aliases should not appear as regular types
          assert(ctx.findType("BinaryData").isEmpty, "Alias should not be findable as a regular type")
        }
    }

    "list all v1 aliases" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val names = ctx.allAliases.map(_.name.name).toSet
          assert(names.contains("BinaryData"))
          assert(names.contains("StringList"))
          assert(names.contains("OptionalInt"))
          assert(names.contains("IntMap"))
          assert(names.contains("EnumAlias"))
          assert(names.contains("AdtAlias"))
          assert(names.contains("ChainedAlias"))
          assert(names.contains("DoubleChain"))
          assert(names.contains("NsAlias"))
        }
    }

    "list v3 aliases" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v3) { ctx =>
          val names = ctx.allAliases.map(_.name.name).toSet
          assert(names.contains("MyStr"))
          assert(names.contains("MyList"))
        }
    }
  }

  "types command" should {
    "list both types and aliases" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val result = TypesCommand.execute(Seq.empty, ctx.asInstanceOf[ExploreContext[Either]])
          assert(result.isRight, s"types command failed: ${result.left.getOrElse("")}")
          val output = result.toOption.get

          assert(output.contains("T1_D1"), s"Output should contain T1_D1")
          assert(output.contains("T4_A1"), s"Output should contain T4_A1")
          assert(output.contains("BinaryData"), s"Output should contain BinaryData alias")
          assert(output.contains("StringList"), s"Output should contain StringList alias")
        }
    }

    "filter aliases by name" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val result = TypesCommand.execute(Seq("Binary"), ctx.asInstanceOf[ExploreContext[Either]])
          assert(result.isRight)
          val output = result.toOption.get
          assert(output.contains("BinaryData"), "Should find BinaryData alias")
          assert(!output.contains("StringList"), "Should not find StringList when filtering for Binary")
        }
    }
  }

  "show command" should {
    "render regular types" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val result = ShowCommand.execute(Seq("T7_Empty"), ctx.asInstanceOf[ExploreContext[Either]])
          assert(result.isRight, s"show T7_Empty failed: ${result.left.getOrElse("")}")
          val output = result.toOption.get
          assert(output.contains("data"), "Should render with 'data' keyword")
          assert(output.contains("T7_Empty"), "Should contain type name")
        }
    }

    "render aliases" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val result = ShowCommand.execute(Seq("BinaryData"), ctx.asInstanceOf[ExploreContext[Either]])
          assert(result.isRight, s"show BinaryData failed: ${result.left.getOrElse("")}")
          val output = result.toOption.get
          assert(output.contains("type"), "Should render with 'type' keyword")
          assert(output.contains("BinaryData"), "Should contain alias name")
          assert(output.contains("bytes"), "Should contain target type")
        }
    }

    "fail for non-existent types" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val result = ShowCommand.execute(Seq("NonExistent"), ctx.asInstanceOf[ExploreContext[Either]])
          assert(result.isLeft, "Should fail for non-existent type")
        }
    }
  }

  "type renderer" should {
    "render all typedef variants" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v1) { ctx =>
          val dom      = ctx.currentDomain.get
          val renderer = new TypeRenderer(dom)

          // DTO
          val dtoRendered = renderer.render(ctx.findType("T1_D1").get)
          assert(dtoRendered.contains("data"), s"DTO should render with 'data': $dtoRendered")

          // ADT
          val adtRendered = renderer.render(ctx.findType("T4_A1").get)
          assert(adtRendered.contains("adt"), s"ADT should render with 'adt': $adtRendered")

          // Enum
          val enumRendered = renderer.render(ctx.findType("T1_E1").get)
          assert(enumRendered.contains("enum"), s"Enum should render with 'enum': $enumRendered")

          // Foreign
          val foreignRendered = renderer.render(ctx.findType("ObscureInt").get)
          assert(foreignRendered.contains("foreign"), s"Foreign should render with 'foreign': $foreignRendered")

          // Alias
          val aliasRendered = renderer.renderAlias(ctx.findAlias("BinaryData").get)
          assert(aliasRendered.contains("type"), s"Alias should render with 'type': $aliasRendered")
          assert(aliasRendered.contains("bytes"), s"Alias should show target: $aliasRendered")

          // Collection alias
          val listAliasRendered = renderer.renderAlias(ctx.findAlias("StringList").get)
          assert(listAliasRendered.contains("lst[str]"), s"Collection alias should render target: $listAliasRendered")
        }
    }

    "render contracts and services in v3" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v3) { ctx =>
          val dom      = ctx.currentDomain.get
          val renderer = new TypeRenderer(dom)

          val contractRendered = renderer.render(ctx.findType("S0").get)
          assert(contractRendered.contains("contract"), s"Contract should render with 'contract': $contractRendered")

          val serviceRendered = renderer.render(ctx.findType("I1").get)
          assert(serviceRendered.contains("service"), s"Service should render with 'service': $serviceRendered")
        }
    }

    "use correct keywords for all types" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        withExploreCtx(loader, codec, v3) { ctx =>
          val dom      = ctx.currentDomain.get
          val renderer = new TypeRenderer(dom)

          dom.defs.meta.nodes.values.collect { case u: DomainMember.User => u }.foreach { member =>
            val rendered = renderer.renderTypeName(member)
            member.defn match {
              case _: Typedef.Dto      => assert(rendered.contains("data"), s"${member.id.name.name}: expected 'data' keyword")
              case _: Typedef.Adt      => assert(rendered.contains("adt"), s"${member.id.name.name}: expected 'adt' keyword")
              case _: Typedef.Enum     => assert(rendered.contains("enum"), s"${member.id.name.name}: expected 'enum' keyword")
              case _: Typedef.Foreign  => assert(rendered.contains("foreign"), s"${member.id.name.name}: expected 'foreign' keyword")
              case _: Typedef.Contract => assert(rendered.contains("contract"), s"${member.id.name.name}: expected 'contract' keyword")
              case _: Typedef.Service  => assert(rendered.contains("service"), s"${member.id.name.name}: expected 'service' keyword")
            }
          }
        }
    }
  }
}

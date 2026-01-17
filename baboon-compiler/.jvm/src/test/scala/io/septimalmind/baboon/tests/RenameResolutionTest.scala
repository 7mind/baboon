package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeName, Version}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

final class RenameResolutionTest extends RenameResolutionTestBase[Either]

abstract class RenameResolutionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "renamed namespace members" should {
    "resolve non-absolute was refs across parent namespaces" in {
      (loader: BaboonLoader[F]) =>
        for {
          fam <- loadRenamePkg(loader)
        } yield {
          val pkg = Pkg(NEList("rename", "ns"))
          val lineage = fam.domains.toMap(pkg)
          val domain = lineage.versions.toMap(Version.parse("2.0.0"))

          val newAbsId = TypeId.User(
            pkg,
            Owner.Ns(Seq(TypeName("outer"), TypeName("inner"), TypeName("deep"), TypeName("deeper"))),
            TypeName("NewTypeAbs"),
          )
          val oldAbsId = TypeId.User(
            pkg,
            Owner.Ns(Seq(TypeName("outer"))),
            TypeName("OldTypeAbs"),
          )
          val newRelId = TypeId.User(
            pkg,
            Owner.Ns(Seq(TypeName("outer"), TypeName("inner"), TypeName("deep"), TypeName("deeper"))),
            TypeName("NewTypeRel"),
          )
          val oldRelId = TypeId.User(
            pkg,
            Owner.Ns(Seq(TypeName("outer"), TypeName("inner"))),
            TypeName("OldTypeRel"),
          )

          assert(domain.renames.get(newAbsId).contains(oldAbsId))
          assert(domain.renames.get(newRelId).contains(oldRelId))
        }
    }
  }

  private def loadRenamePkg(loader: BaboonLoader[F]): F[NEList[BaboonIssue], io.septimalmind.baboon.typer.model.BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/rename-ns")
      .get
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons = IzFiles
      .walk(root.toFile)
      .toList
      .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(baboons)
  }
}

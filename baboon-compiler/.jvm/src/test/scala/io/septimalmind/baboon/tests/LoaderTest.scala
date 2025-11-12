package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

final class LoaderTest extends LoaderTestBase[Either]

abstract class LoaderTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "baboon loader" should {
    "load baboon families" in {
      (loader: BaboonLoader[F]) =>
        for {
          res <- loadPkg(loader)
          _    = assert(res.isInstanceOf[BaboonFamily])
        } yield ()
    }
  }

  "baboon converter should" should {
    "read baboon binaries" in {
      (loader: BaboonLoader[F]) =>
        for {
          fam <- loadPkg(loader)
        } yield ()
    }
  }

  private def loadPkg(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/pkg0")
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

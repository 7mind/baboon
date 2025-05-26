package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.QuasiIOEither.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

final class LoaderTest extends LoaderTestBase[Either]

abstract class LoaderTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "baboon loader" should {
    "load baboon families" in {
      (loader: BaboonLoader[F]) =>
        val root = IzResources
          .getPath("baboon/pkg0")
          .get
          .asInstanceOf[IzResources.LoadablePathReference]
          .path
        val baboons = IzFiles
          .walk(root.toFile)
          .toList
          .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))

        for {
          res <- loader.load(baboons)
          _    = assert(res.isInstanceOf[BaboonFamily])
        } yield ()
    }
  }
}

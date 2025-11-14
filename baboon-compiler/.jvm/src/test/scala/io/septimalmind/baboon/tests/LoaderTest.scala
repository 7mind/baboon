package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
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

}

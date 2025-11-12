package io.septimalmind.baboon.tests

import io.circe.Json
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.RawHeader
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{Pkg, TypeId, Version}
import io.septimalmind.baboon.typer.{BaboonRuntimeCodec, ComponentParsers}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.NEList
import izumi.reflect.TagKK

final class RTCodecTest extends RTCodecTestBase[Either]

abstract class RTCodecTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  "baboon converter should" should {
    "read baboon binaries" in {
      (loader: BaboonLoader[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          fam       <- loadPkg(loader)
          data: Json = io.circe.parser.parse("""{"B2":{"f":135983116}}""").toOption.get
          encoded   <- codec.encode(fam, Pkg(NEList("testpkg", "pkg0")), Version("3.0.0"), "testpkg.pkg0/[testpkg.pkg0/:#T5_A1]#B1", data)
          decoded   <- codec.decode(fam, Pkg(NEList("testpkg", "pkg0")), Version("3.0.0"), "testpkg.pkg0/[testpkg.pkg0/:#T5_A1]#B1", encoded)
        } yield {
          assert(data == decoded)
        }
    }
  }

}

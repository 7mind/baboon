package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources



class LoaderTest extends BaboonTest {
  "baboon loader" should {
    "load baboon families" in { (loader: BaboonLoader) =>
      val root = IzResources
        .getPath("baboon/pkg0")
        .get
        .asInstanceOf[IzResources.LoadablePathReference]
        .path
      val baboons = IzFiles
        .walk(root.toFile)
        .toList
        .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      val loaded = loader.load(baboons)
      assert(loaded.isRight)
//      println(loaded)
    }
  }
}

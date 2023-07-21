package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader.BaboonLoaderImpl
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import org.scalatest.wordspec.AnyWordSpec

class LoaderTest extends AnyWordSpec {
  "baboon loader" should {
    "load baboon families" in {

      val root = IzResources
        .getPath("baboon/pkg0")
        .get
        .asInstanceOf[IzResources.LoadablePathReference]
        .path
      val baboons = IzFiles
        .walk(root.toFile)
        .toList
        .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      val loader = new BaboonLoaderImpl()
      val loaded = loader.load(baboons)
      println(loaded)
    }
  }
}

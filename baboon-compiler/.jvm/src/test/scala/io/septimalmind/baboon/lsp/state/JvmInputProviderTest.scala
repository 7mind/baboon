package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.lsp.util.JvmPathOps
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

class JvmInputProviderTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  private var tempRoot: Path = _

  override def beforeAll(): Unit = {
    tempRoot = Files.createTempDirectory("JvmInputProviderTest")
  }

  override def afterAll(): Unit = {
    // Clean up temp directory tree
    Files
      .walk(tempRoot)
      .iterator()
      .asScala
      .toSeq
      .reverse
      .foreach(p => Files.deleteIfExists(p))
  }

  "JvmInputProvider.getWorkspaceInputs" should {

    "return each .baboon file exactly once when one model dir nests inside another" in {
      // Layout:
      //   <tempRoot>/outer/
      //     outer.baboon
      //     inner/
      //       inner.baboon
      //
      // Both /outer and /outer/inner are passed as modelDirs.
      val outerDir = Files.createTempDirectory(tempRoot, "outer")
      val innerDir = Files.createDirectories(outerDir.resolve("inner"))

      val outerFile = outerDir.resolve("outer.baboon")
      val innerFile = innerDir.resolve("inner.baboon")
      Files.writeString(outerFile, "# outer")
      Files.writeString(innerFile, "# inner")

      val modelDirs = Set(outerDir, innerDir)
      val provider  = new JvmInputProvider(modelDirs, JvmPathOps)

      val inputs = provider.getWorkspaceInputs
      val paths  = inputs.map(_.path.asString)

      paths.distinct shouldBe paths  // no duplicates
      paths should contain(outerFile.toAbsolutePath.normalize().toString)
      paths should contain(innerFile.toAbsolutePath.normalize().toString)
      paths.size shouldBe 2
    }

    "return each .baboon file exactly once when the same dir is passed twice" in {
      val dir  = Files.createTempDirectory(tempRoot, "same")
      val file = dir.resolve("model.baboon")
      Files.writeString(file, "# model")

      val modelDirs = Set(dir, dir)  // same path — Set deduplicates, so effectively one dir
      val provider  = new JvmInputProvider(modelDirs, JvmPathOps)

      val inputs = provider.getWorkspaceInputs
      inputs.size shouldBe 1
    }

    "not return non-.baboon files" in {
      val dir = Files.createTempDirectory(tempRoot, "mixed")
      Files.writeString(dir.resolve("model.baboon"), "# model")
      Files.writeString(dir.resolve("readme.txt"), "ignored")

      val provider = new JvmInputProvider(Set(dir), JvmPathOps)
      val inputs   = provider.getWorkspaceInputs

      inputs.size shouldBe 1
      inputs.head.path.asString should endWith(".baboon")
    }

    "return empty seq for a non-existent model dir" in {
      val missing  = tempRoot.resolve("does-not-exist")
      val provider = new JvmInputProvider(Set(missing), JvmPathOps)
      provider.getWorkspaceInputs shouldBe empty
    }
  }
}

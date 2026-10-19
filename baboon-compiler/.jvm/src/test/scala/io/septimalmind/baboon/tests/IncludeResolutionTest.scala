package io.septimalmind.baboon.tests

import io.septimalmind.baboon.CompilerOptions
import io.septimalmind.baboon.parser.{BaboonInclusionResolverImpl, BaboonInclusionResolverMapImpl, BaboonParser}
import io.septimalmind.baboon.parser.model.{FSPath, InputPointer, RawInclude, RawNodeMeta}
import io.septimalmind.baboon.parser.model.issues.ParserIssue
import izumi.functional.bio.impl.BioEither
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** `include` resolves next to the including file first, then against the model roots, of which
  * exactly one may hold the path.
  */
final class IncludeResolutionTest extends AnyWordSpec {
  private implicit val error2: izumi.functional.bio.Error2[Either] = BioEither

  private def fs(path: String): FSPath          = FSPath.parse(NEString.unsafeFrom(path))
  private def include(path: String): RawInclude = RawInclude(RawNodeMeta(InputPointer.Undefined), path)
  private def options(roots: Path*): CompilerOptions =
    CompilerOptions(Set.empty, roots.map(root => fs(root.toString)).toSet, None, debug = false, Seq.empty, None, None)

  private def write(path: Path, content: String): Path = {
    Files.createDirectories(path.getParent)
    Files.write(path, content.getBytes(StandardCharsets.UTF_8))
  }

  "the file-system resolver" should {
    val root  = Files.createTempDirectory("baboon-includes")
    val model = write(root.resolve("sub/model.baboon"), "")
    write(root.resolve("sub/defs.bmo"), "sub")
    write(root.resolve("sub/shared/common.bmo"), "sub shared")
    write(root.resolve("shared/common.bmo"), "root shared")
    write(root.resolve("other/only.bmo"), "other")
    write(root.resolve("a/dup.bmo"), "a")
    write(root.resolve("b/dup.bmo"), "b")

    def resolve(path: String, roots: Path*): Either[ParserIssue, String] =
      new BaboonInclusionResolverImpl[Either](options(roots*)).resolveInclude(include(path), fs(model.toString)).map(_._2)

    "find a file next to the including one with a parent directory as the model root" in {
      assert(resolve("defs.bmo", root) == Right("sub"))
      assert(resolve("../other/only.bmo", root) == Right("other"))
    }

    "prefer the file next to the including one over the model roots" in {
      assert(resolve("shared/common.bmo", root) == Right("sub shared"))
    }

    "fall back to the model roots" in {
      assert(resolve("other/only.bmo", root) == Right("other"))
      assert(resolve("only.bmo", root.resolve("other")) == Right("other"))
    }

    "reject a path that several model roots hold" in {
      resolve("dup.bmo", root.resolve("a"), root.resolve("b")) match {
        case Left(ParserIssue.IncludeAmbiguous(inc, candidates)) =>
          assert(inc.value == "dup.bmo")
          assert(candidates == List(root.resolve("a/dup.bmo").toString, root.resolve("b/dup.bmo").toString))
        case other => fail(other.toString)
      }
      assert(resolve("dup.bmo", root.resolve("a"), root.resolve("a")) == Right("a"))
    }

    "name every location it looked at when nothing matches" in {
      resolve("missing.bmo", root.resolve("a"), root.resolve("b")) match {
        case Left(ParserIssue.IncludeNotFound(inc, tried)) =>
          assert(inc.value == "missing.bmo")
          assert(tried == List("sub/missing.bmo", "a/missing.bmo", "b/missing.bmo").map(p => root.resolve(p).toString))
        case other => fail(other.toString)
      }
    }
  }

  "the in-memory resolver" should {
    val inputs = List(
      BaboonParser.Input(fs("models/sub/model.baboon"), ""),
      BaboonParser.Input(fs("models/sub/defs.bmo"), "sub"),
      BaboonParser.Input(fs("models/top.baboon"), ""),
      BaboonParser.Input(fs("top.bmo"), "top"),
    )
    val resolver = new BaboonInclusionResolverMapImpl[Either](options(), inputs)

    "find a file next to the including one, at any depth" in {
      assert(resolver.resolveInclude(include("defs.bmo"), fs("models/sub/model.baboon")).map(_._2) == Right("sub"))
      assert(resolver.resolveInclude(include("sub/defs.bmo"), fs("models/top.baboon")).map(_._2) == Right("sub"))
      assert(resolver.resolveInclude(include("../../top.bmo"), fs("models/sub/model.baboon")).map(_._2) == Right("top"))
      assert(resolver.resolveInclude(include("top.bmo"), fs("root.baboon")).map(_._2) == Right("top"))
    }

    "not climb above the inputs" in {
      assert(resolver.resolveInclude(include("../../../top.bmo"), fs("models/sub/model.baboon")).isLeft)
    }
  }
}

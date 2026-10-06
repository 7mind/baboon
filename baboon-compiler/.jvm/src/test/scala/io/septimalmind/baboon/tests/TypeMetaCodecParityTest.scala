package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.BaboonRuntimeResources
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** The interpreted envelope conversions (`BaboonRuntimeEnvelopeCodec`) read and write the top-level
  * envelope with a compile-side copy of the runtime's `BaboonTypeMetaCodec`; any drift from the
  * shipped Scala runtime would be a second wire-format implementation.
  */
class TypeMetaCodecParityTest extends AnyFunSuite {
  test("compile-side and embedded Scala BaboonTypeMetaCodec implementations remain identical") {
    val relative = Paths.get("baboon-compiler/src/main/scala/baboon/runtime/shared/BaboonRuntimeShared.scala")
    val cwd      = Paths.get(System.getProperty("user.dir")).toAbsolutePath
    val source = Iterator
      .iterate[Path](cwd)(_.getParent).takeWhile(_ != null)
      .map(_.resolve(relative)).find(Files.isRegularFile(_))
      .getOrElse(fail(s"Cannot locate compile-side runtime source from $cwd"))
    val mirror   = new String(Files.readAllBytes(source), StandardCharsets.UTF_8)
    val embedded = BaboonRuntimeResources.read("baboon-runtime/scala/BaboonRuntimeShared.scala")
    assert(codecObject(mirror) == codecObject(embedded))
  }

  private def codecObject(source: String): String = {
    val declaration = "  object BaboonTypeMetaCodec {"
    val start       = source.indexOf(declaration)
    assert(start >= 0, "BaboonTypeMetaCodec declaration is missing")
    val end = source.indexOf("\n  }", start)
    assert(end > start, "BaboonTypeMetaCodec closing delimiter is missing")
    source.substring(start, end + "\n  }".length)
  }
}

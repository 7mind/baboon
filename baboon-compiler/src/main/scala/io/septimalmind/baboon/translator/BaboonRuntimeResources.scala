package io.septimalmind.baboon.translator

import izumi.fundamentals.platform.resources.PortableResource

object BaboonRuntimeResources {
  // Macro picks up resource files at compile time. When new runtime files are
  // added under `baboon-compiler/src/main/resources/baboon-runtime/<lang>/`,
  // touch this comment to force a recompile of this unit (sbt won't otherwise
  // detect resource-tree changes and the macro's embedded map will be stale).
  // Last touched: 2026-05-28 for muxer runtime additions (Java IBaboon*Service, *Muxer).
  private val resources: Map[String, String] = PortableResource.embedSources("baboon-compiler/src/main/resources/{baboon-runtime/**}")

  def read(path: String): String = {
    resources.getOrElse(path, throw new RuntimeException(s"Runtime resource not found: $path, available: ${resources.keys.mkString(", ")}"))
  }
}

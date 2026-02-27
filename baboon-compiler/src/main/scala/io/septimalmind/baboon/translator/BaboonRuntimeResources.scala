package io.septimalmind.baboon.translator

import izumi.fundamentals.platform.resources.PortableResource

object BaboonRuntimeResources {
  private val resources: Map[String, String] = PortableResource.embedSources("baboon-compiler/src/main/resources/{baboon-runtime/**}")

  def read(path: String): String = {
    resources.getOrElse(path, throw new RuntimeException(s"Runtime resource not found: $path, available: ${resources.keys.mkString(", ")}"))
  }
}

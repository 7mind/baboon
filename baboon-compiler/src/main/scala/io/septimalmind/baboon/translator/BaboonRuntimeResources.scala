package io.septimalmind.baboon.translator

import izumi.fundamentals.platform.resources.PortableResource

object BaboonRuntimeResources {
  // Macro picks up resource files at compile time. When new runtime files are
  // added under `baboon-compiler/src/main/resources/baboon-runtime/<lang>/`,
  // touch this comment to force a recompile of this unit (sbt won't otherwise
  // detect resource-tree changes and the macro's embedded map will be stale).
  // Last touched: 2026-06-02 — split swift service-wiring runtime out of baboon_runtime.swift
  // into baboon_service_wiring.swift (the monolith's embedded Utf8 constant exceeded the
  // 65535-byte JVM class-file limit after the abstract-service-context additions; CI's scalac
  // backend errors on it where the local one silently splits).
  private val resources: Map[String, String] = PortableResource.embedSources("baboon-compiler/src/main/resources/{baboon-runtime/**}")

  def read(path: String): String = {
    resources.getOrElse(path, throw new RuntimeException(s"Runtime resource not found: $path, available: ${resources.keys.mkString(", ")}"))
  }
}

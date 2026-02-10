package io.septimalmind.baboon.translator

import io.septimalmind.baboon.{HktConfig, ServiceResultConfig}
import io.septimalmind.baboon.typer.model.Domain

case class ResolvedServiceResult(
  noErrors: Boolean,
  resultType: Option[String],
  pattern: Option[String],
  hkt: Option[HktConfig],
) {
  def renderReturnType(outType: String, errType: Option[String], unitType: String): String = {
    val success = if (outType.nonEmpty) outType else unitType
    val error   = errType.getOrElse(unitType)

    if (noErrors || errType.isEmpty) {
      success
    } else {
      (hkt, pattern) match {
        case (Some(h), Some(p)) =>
          val expanded = p.replace("$error", error).replace("$success", success)
          s"${h.name}$expanded"
        case (None, Some(p)) =>
          val expanded = p.replace("$error", error).replace("$success", success)
          val rt       = resultType.getOrElse("")
          s"$rt$expanded"
        case _ =>
          success
      }
    }
  }

  def traitTypeParam: Option[String] = {
    if (noErrors) None
    else hkt.map(h => s"${h.name}${h.signature}")
  }
}

object ServiceResultResolver {
  private val pragmaPrefix = Map(
    "scala"      -> "scala.service.result.",
    "cs"         -> "cs.service.result.",
    "rust"       -> "rust.service.result.",
    "python"     -> "python.service.result.",
    "typescript" -> "typescript.service.result.",
  )

  private val pragmaSuffixes: Seq[(String, String)] = Seq(
    "no-errors"     -> "\"true\" or \"false\"",
    "type"          -> "result type name",
    "pattern"       -> "e.g. [$error, $success]",
    "hkt"           -> "\"true\" or \"false\"",
    "hkt.name"      -> "e.g. F",
    "hkt.signature" -> "e.g. [+_, +_]",
  )

  val knownPragmaKeys: Seq[(String, String)] = for {
    (_, prefix) <- pragmaPrefix.toSeq.sortBy(_._1)
    (suffix, description) <- pragmaSuffixes
  } yield (s"$prefix$suffix", description)

  def resolve(domain: Domain, languageKey: String, cliConfig: ServiceResultConfig, cliPragmas: Map[String, String]): ResolvedServiceResult = {
    val prefix        = pragmaPrefix.getOrElse(languageKey, s"$languageKey.service.result.")
    val domainPragmas = domain.pragmas.filter { case (k, _) => k.startsWith(prefix) }.map { case (k, v) => (k.stripPrefix(prefix), v) }
    val extraPragmas  = cliPragmas.filter { case (k, _) => k.startsWith(prefix) }.map { case (k, v) => (k.stripPrefix(prefix), v) }

    val merged = domainPragmas ++ extraPragmas

    val noErrors   = merged.get("no-errors").map(_.toBoolean).orElse(Some(cliConfig.noErrors)).getOrElse(false)
    val resultType = merged.get("type").orElse(cliConfig.resultType)
    val pattern    = merged.get("pattern").orElse(cliConfig.pattern)

    val hkt = if (merged.get("hkt").exists(_.toBoolean)) {
      Some(HktConfig(
        name      = merged.getOrElse("hkt.name", "F"),
        signature = merged.getOrElse("hkt.signature", "[+_, +_]"),
      ))
    } else {
      cliConfig.hkt
    }

    ResolvedServiceResult(noErrors, resultType, pattern, hkt)
  }
}

package io.septimalmind.baboon.translator

import io.septimalmind.baboon.{HktConfig, ServiceResultConfig}
import io.septimalmind.baboon.typer.model.Domain
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

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

  /** Tree-preserving counterpart of [[renderReturnType]]: `None` means the method returns
    * the unit type, otherwise the returned tree keeps the language-value nodes of
    * `outType`/`errType` instead of flattening them to text.
    *
    * Backends whose renderer post-processes type nodes MUST use this form — the C# renderer
    * rewrites references to deduplicated types to their surviving twin at render time, and a
    * type flattened to a string here escapes that rewrite and names a type that was never
    * emitted.
    */
  def renderReturnTypeTree[T](
    outType: Option[TextTree[T]],
    errType: Option[TextTree[T]],
    unitType: TextTree[T],
  ): Option[TextTree[T]] = {
    val success = outType.getOrElse(unitType)

    if (noErrors || errType.isEmpty) {
      outType
    } else {
      val container = expandPattern(errType.get, success)
      (hkt, container) match {
        case (Some(h), Some(c)) => Some(Seq(TextTree.text[T](h.name), c).join(""))
        case (None, Some(c))    => Some(Seq(TextTree.text[T](resultType.getOrElse("")), c).join(""))
        case _                  => outType
      }
    }
  }

  /** Expands the configured `$error`/`$success` pattern, splicing the argument trees in place
    * of the placeholders. `None` when no pattern is configured.
    */
  def expandPattern[T](error: TextTree[T], success: TextTree[T]): Option[TextTree[T]] = {
    pattern.map {
      p =>
        val values = Map(
          ServiceResultResolver.errorPlaceholder   -> error,
          ServiceResultResolver.successPlaceholder -> success,
        )
        val matches = ServiceResultResolver.placeholders.findAllMatchIn(p).toList
        val spliced: (List[TextTree[T]], Int) = matches.foldLeft((List.empty[TextTree[T]], 0)) {
          case ((acc, pos), m) =>
            (acc ++ List(TextTree.text[T](p.substring(pos, m.start)), values(m.matched)), m.end)
        }
        (spliced._1 :+ TextTree.text[T](p.substring(spliced._2))).join("")
    }
  }

  def traitTypeParam: Option[String] = {
    if (noErrors) None
    else hkt.map(h => s"${h.name}${h.signature}")
  }
}

object ServiceResultResolver {
  val errorPlaceholder: String   = "$error"
  val successPlaceholder: String = "$success"

  private[translator] val placeholders =
    s"${_root_.java.util.regex.Pattern.quote(errorPlaceholder)}|${_root_.java.util.regex.Pattern.quote(successPlaceholder)}".r

  private val pragmaPrefix = Map(
    "scala"      -> "scala.service.result.",
    "cs"         -> "cs.service.result.",
    "dart"       -> "dart.service.result.",
    "java"       -> "java.service.result.",
    "kotlin"     -> "kotlin.service.result.",
    "rust"       -> "rust.service.result.",
    "python"     -> "python.service.result.",
    "swift"      -> "swift.service.result.",
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
    (_, prefix)           <- pragmaPrefix.toSeq.sortBy(_._1)
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
      Some(
        HktConfig(
          name      = merged.getOrElse("hkt.name", "F"),
          signature = merged.getOrElse("hkt.signature", "[+_, +_]"),
        )
      )
    } else {
      cliConfig.hkt
    }

    ResolvedServiceResult(noErrors, resultType, pattern, hkt)
  }
}

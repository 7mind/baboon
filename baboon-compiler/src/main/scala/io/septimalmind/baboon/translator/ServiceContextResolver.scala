package io.septimalmind.baboon.translator

import io.septimalmind.baboon.ServiceContextConfig
import io.septimalmind.baboon.typer.model.Domain

sealed trait ResolvedServiceContext
object ResolvedServiceContext {
  case object NoContext extends ResolvedServiceContext
  case class AbstractContext(typeName: String, parameterName: String) extends ResolvedServiceContext
  case class ConcreteContext(typeName: String, parameterName: String) extends ResolvedServiceContext
}

object ServiceContextResolver {
  private val languages = Seq("cs", "python", "rust", "scala", "typescript")

  private val pragmaSuffixes: Seq[(String, String)] = Seq(
    ""                -> "\"none\", \"abstract\", or \"type\"",
    ".type"           -> "context type name (default: Ctx)",
    ".parameter.name" -> "context parameter name (default: ctx)",
  )

  val knownPragmaKeys: Seq[(String, String)] = for {
    lang <- languages
    (suffix, description) <- pragmaSuffixes
  } yield (s"$lang.service.context$suffix", description)

  private val validIdentifier = "^[A-Za-z_][A-Za-z0-9_]*$".r

  private def validateTypeName(typeName: String, mode: String): String = {
    val parts = typeName.split('.')
    assert(parts.nonEmpty, s"service.context.type must not be empty")
    parts.foreach { part =>
      assert(validIdentifier.matches(part), s"service.context.type component '$part' is not a valid identifier in '$typeName'")
    }
    if (mode == "abstract") {
      assert(parts.length == 1, s"service.context.type must be a simple name for abstract mode, got '$typeName'")
    }
    typeName
  }

  private def validateParameterName(parameterName: String): String = {
    assert(validIdentifier.matches(parameterName), s"service.context.parameter.name '$parameterName' is not a valid identifier")
    parameterName
  }

  def resolve(domain: Domain, languageKey: String, cliConfig: ServiceContextConfig, cliPragmas: Map[String, String]): ResolvedServiceContext = {
    val modeKey  = s"$languageKey.service.context"
    val typeKey  = s"$languageKey.service.context.type"
    val paramKey = s"$languageKey.service.context.parameter.name"

    val mode = cliPragmas.get(modeKey)
      .orElse(domain.pragmas.get(modeKey))
      .getOrElse(cliConfig.mode)

    mode match {
      case "none" =>
        ResolvedServiceContext.NoContext
      case "abstract" | "type" =>
        val typeName = cliPragmas.get(typeKey)
          .orElse(domain.pragmas.get(typeKey))
          .getOrElse(cliConfig.typeName)
        val paramName = cliPragmas.get(paramKey)
          .orElse(domain.pragmas.get(paramKey))
          .getOrElse(cliConfig.parameterName)

        val validatedType  = validateTypeName(typeName, mode)
        val validatedParam = validateParameterName(paramName)

        if (mode == "abstract") {
          ResolvedServiceContext.AbstractContext(validatedType, validatedParam)
        } else {
          ResolvedServiceContext.ConcreteContext(validatedType, validatedParam)
        }
      case other =>
        throw new IllegalArgumentException(
          s"Invalid service.context mode '$other' for $languageKey. Must be 'none', 'abstract', or 'type'"
        )
    }
  }
}

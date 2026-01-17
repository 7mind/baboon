package io.septimalmind.baboon.lsp.state

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.functional.bio.Error2
import izumi.functional.quasi.QuasiIORunner
import izumi.fundamentals.collections.nonempty.NEList

import scala.collection.mutable

/** JVM implementation of LspCompiler that wraps BaboonFamilyManager */
class JvmBaboonCompiler[F[+_, +_]: Error2](
  parser: BaboonParser[F],
  manager: BaboonFamilyManager[F],
  runner: QuasiIORunner[F[Throwable, _]]
) extends LspCompiler {

  private case class CachedParsed(content: String, domain: RawDomain)

  private val cacheLock = new Object
  @volatile private var cache: Map[FSPath, CachedParsed] = Map.empty

  override def reload(inputs: Seq[BaboonParser.Input]): Either[NEList[BaboonIssue], BaboonFamily] = {
    val F = implicitly[Error2[F]]
    runner.run(
      F.catchAll(
        F.flatMap(buildReloadInputs(inputs.toList)) { reloadInputs =>
          F.map(manager.reload(reloadInputs))(family => Right(family): Either[NEList[BaboonIssue], BaboonFamily])
        }
      )(issues => F.pure(Left(issues): Either[NEList[BaboonIssue], BaboonFamily]))
    )
  }

  private def buildReloadInputs(
    inputs: List[BaboonParser.Input]
  ): F[NEList[BaboonIssue], List[BaboonParser.ReloadInput]] = {
    val F = implicitly[Error2[F]]
    val paths = inputs.map(_.path)
    assert(paths.distinct.size == paths.size, "Duplicate file paths provided to LSP compiler")

    val cachedSnapshot = cacheLock.synchronized {
      cache
    }
    val toParse = mutable.ListBuffer.empty[BaboonParser.Input]
    val parsedByPath = mutable.HashMap.empty[FSPath, RawDomain]

    inputs.foreach { input =>
      cachedSnapshot.get(input.path) match {
        case Some(cached) if cached.content == input.content =>
          parsedByPath.put(input.path, cached.domain)
        case _ =>
          toParse += input
      }
    }

    F.map(F.traverseAccumErrors(toParse.toList)(parser.parse)) { parsed =>
      toParse.toList.zip(parsed).foreach { case (input, domain) =>
        parsedByPath.put(input.path, domain)
      }

      val newCache = inputs.map { input =>
        input.path -> CachedParsed(input.content, requireParsed(parsedByPath, input.path))
      }.toMap

      cacheLock.synchronized {
        cache = newCache
      }

      inputs.map { input =>
        BaboonParser.ReloadInput.Parsed(input.path, requireParsed(parsedByPath, input.path))
      }
    }
  }

  private def requireParsed(
    parsedByPath: mutable.HashMap[FSPath, RawDomain],
    path: FSPath
  ): RawDomain = {
    parsedByPath.getOrElse(path, throw new IllegalStateException(s"Missing parsed domain for ${path.asString}"))
  }
}
